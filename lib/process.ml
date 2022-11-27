let () =
  Random.self_init ()

type command = string * string list

let failf fmt = fmt |> Fmt.kstr @@ fun msg -> Error (`Msg msg)

let pp_args =
  let sep = Fmt.(const string) " " in
  Fmt.(list ~sep (quote string))

let pp_cmd f = function
  | "", args -> pp_args f args
  | bin, args -> Fmt.pf f "(%S, %a)" bin pp_args args

let check_status pp_cmd cmd = function
  | 0, _ -> Ok ()
  | 127, _ ->
      let cmd_name =
        match cmd with
        | "", args ->
            if List.length args > 0 then Some (List.nth args 0) else None
        | p, _ -> Some p
      in
      if Option.is_some cmd_name then
        failf "%t exited with status %d. Is %s installed?" pp_cmd 127
          (Option.get cmd_name)
      else failf "%t exited with status %d" pp_cmd 127
  (* XXX: Todo check how this compares to the original logic *)
  | x, 0L -> failf "%t exited with status %d" pp_cmd x
  | _, x -> failf "%t failed with signal %Ld" pp_cmd x

let make_tmp_dir ?(prefix = "tmp-") ?(mode = 0o700) parent =
  let rec mktmp = function
    | 0 -> Fmt.failwith "Failed to generate temporary directory name!"
    | n -> (
      let tmppath =
        Printf.sprintf "%s/%s%x" parent prefix (Random.int 0x3fffffff)
      in
      try
        Unix.mkdir tmppath mode;
        tmppath
      with Unix.Unix_error (Unix.EEXIST, _, _) ->
        Log.warn (fun f -> f "Temporary directory %s already exists!" tmppath);
        mktmp (n - 1) )
  in
  mktmp 10

let unlink = Eio.Path.unlink

let rm_f_tree root =
  let open Eio in
  let rec rmtree path =
    Eio.Path.with_open_in path @@ fun r ->
    let info = File.stat r in
    match info.kind with
    | `Directory ->
      Unix.chmod (snd path) 0o700;
      Eio.Path.read_dir path
      |> List.iter (function
          | "." | ".." -> ()
          | leaf -> rmtree (Eio.Path.(path / leaf))
        );
      Eio.Path.rmdir path
    | _->
      unlink path
    
  in
  rmtree root

let with_tmpdir ?prefix (fs : Eio.Fs.dir Eio.Path.t) (fn : Eio.Fs.dir Eio.Path.t -> 'a) =
  let tmpdir = make_tmp_dir ?prefix ~mode:0o700 (Filename.get_temp_dir_name ()) in
  let tmpdir = Eio.Path.(fs / tmpdir) in
  Fun.protect
    (fun () -> fn tmpdir)
    ~finally:(fun () -> rm_f_tree tmpdir)

let send_to ch contents =
  let open Eio_luv.Low_level in
  let buf = Luv.Buffer.create (String.length contents) in
  Luv.Buffer.blit_from_string ~source_offset:0 buf contents;
  try
      Stream.write ch [ buf ];
       Ok ()
  with ex -> Error (`Msg (Printexc.to_string ex))

let pp_command pp_cmd cmd f = Fmt.pf f "Command %a" pp_cmd cmd

let copy_to_log ~job src =
  let open Eio_luv.Low_level in
  let buf = Luv.Buffer.create 4096 in
  let rec aux () =
    assert (Luv.Stream.is_readable (Handle.to_luv src));
    match Stream.read_into src buf with
    | 0 -> ()
    | data -> Job.write job (Luv.Buffer.(to_string (sub buf ~offset:0 ~length:data))); aux ()
    | exception End_of_file -> ()
  in
  aux ()

let or_raise = function
  | Ok v -> v
  | Error e -> 
    Logs.debug (fun f -> f "Current.Process");
    raise (Eio_luv.Low_level.Luv_error e)

let read src =
  let open Eio_luv.Low_level in
  let buf = Luv.Buffer.create 4096 in
  let rec aux acc =
    match Stream.read_into src buf with
    | 0 -> acc
    | data -> aux (acc ^ (Luv.Buffer.(to_string (sub buf ~offset:0 ~length:data))))
    | exception End_of_file -> acc
  in
  aux ""

module Proc = Eio_luv.Low_level.Process

let add_shutdown_hooks ~cancellable ~job ~cmd proc =
  if cancellable then (
    Job.on_cancel job (fun reason ->
        if not (Proc.has_exited proc) then (
          Log.info (fun f -> f "Cancelling %a (%s)" pp_cmd cmd reason);
          Proc.send_signal proc Luv.Signal.sigkill;
        )
      )
  ) else (
    (* Always terminate process if the job ends: *)
    Switch.add_hook_or_exec job.Job.switch (fun _reason ->
        if not (Proc.has_exited proc) then Proc.send_signal proc Luv.Signal.sigkill
      )
  )

let socketpair ~sw ty =
  let ty =
    match ty with
    | Unix.SOCK_DGRAM -> `DGRAM
    | Unix.SOCK_STREAM -> `STREAM
    | Unix.SOCK_RAW -> `RAW
    | Unix.SOCK_SEQPACKET -> failwith "Type SEQPACKET not support by libuv"
  in
  let a, b = Luv.TCP.socketpair ty 0 |> or_raise in
  let wrap x =
    let sock = Luv.TCP.init ~loop:(Eio_luv.Low_level.get_loop ()) () |> or_raise in
    Luv.TCP.open_ sock x |> or_raise;
    let h = Eio_luv.Low_level.Handle.of_luv ~sw ~close_unix:true sock in
    h
  in
  (wrap a, wrap b)

let exec ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job ((p, args) as cmd) =
  let cwd = Option.map Fpath.to_string cwd in
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  (match cwd with Some cwd -> Log.info (fun f -> f "CWD: %s" cwd) | _ -> Log.info (fun f -> f "no cwd"));
  Eio.Switch.run @@ fun sw ->
  (* XXX: There doesn't seem to be a way to connect child stderr to child stdout
          in Luv and using a pipe gets EBUSY... see https://github.com/libuv/libuv/pull/2598 *)
  let write, read = socketpair ~sw Unix.SOCK_STREAM in
  let write_stream = Eio_luv.Low_level.Handle.to_luv write in
  let stdin_pipe = Eio_luv.Low_level.Pipe.init ~sw () in
  let proc = Proc.spawn ~sw ?cwd ~redirect:[
    Luv.Process.inherit_stream ~fd:Luv.Process.stderr ~from_parent_stream:write_stream ();
    Luv.Process.inherit_stream ~fd:Luv.Process.stdout ~from_parent_stream:write_stream ();
    Proc.to_parent_pipe ~fd:Luv.Process.stdin ~parent_pipe:stdin_pipe ()
  ] p args in
  let copy_thread () = copy_to_log ~job read in
  add_shutdown_hooks ~cancellable ~job ~cmd proc;
  let stdin_result = send_to stdin_pipe stdin in
  let status = Proc.await_exit proc in
  Eio_luv.Low_level.Handle.close write;
  copy_thread (); (* Ensure all data has been copied before returning *)
  match check_status pp_error_command cmd status with
  | Ok () -> stdin_result
  | Error _ as e -> e

let check_output ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job ((p, args) as cmd) =
  let cwd = Option.map Fpath.to_string cwd in
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  Eio.Switch.run @@ fun sw ->
  let err_pipe = Eio_luv.Low_level.Pipe.init ~sw () in
  let stdout_pipe = Eio_luv.Low_level.Pipe.init ~sw () in
  let stdin_pipe = Eio_luv.Low_level.Pipe.init ~sw () in
  let proc = Proc.spawn ~sw ?cwd ~redirect:[
    Proc.to_parent_pipe ~fd:Luv.Process.stderr ~parent_pipe:err_pipe ();
    Proc.to_parent_pipe ~fd:Luv.Process.stdout ~parent_pipe:stdout_pipe ();
    Proc.to_parent_pipe ~fd:Luv.Process.stdin ~parent_pipe:stdin_pipe ()
  ] p args in
  let copy () = copy_to_log ~job err_pipe in
  add_shutdown_hooks ~cancellable ~job ~cmd proc;
  let stdin_result = send_to stdin_pipe stdin in
  let stdout = read stdout_pipe in
  copy ();
  let status = Proc.await_exit proc in
  match check_status pp_error_command cmd status with
  | Error _ as e -> e
  | Ok () ->
    match stdin_result with
    | Error _ as e -> e
    | Ok () ->
      Ok stdout

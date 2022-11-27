open Current.Syntax

module S = S

let pp_tag = Fmt.using (Astring.String.cuts ~sep:":") Fmt.(list ~sep:(any ":@,") string)

module Raw = struct
  module Image = Image

  module PullC = Current_cache.Make(Pull)

  let pull ~docker_context ~schedule ~sw ?arch tag =
    PullC.get ~sw ~schedule Pull.No_context { Pull.Key.docker_context; tag; arch }

  module PeekC = Current_cache.Make(Peek)

  let peek ~docker_context ~schedule ~arch ~sw tag =
    PeekC.get ~sw ~schedule Peek.No_context { Peek.Key.docker_context; tag; arch }

  module BC = Current_cache.Make(Build)

  let build ~docker_context ?level ?schedule ?timeout ?(squash=false) ?dockerfile ?path ?pool ?(build_args=[]) ~pull ~fs ~sw commit =
    let dockerfile =
      match dockerfile with
      | None -> `File (Fpath.v "Dockerfile")
      | Some (`File _ as f) -> f
      | Some (`Contents c) -> `Contents c
    in
    BC.get ~sw ?schedule ({ Build.pull; pool; timeout; level }, fs)
    { Build.Key.commit; dockerfile; docker_context; squash; build_args; path }

  module RC = Current_cache.Make(Run)

  let run ~docker_context ?pool ?(run_args=[]) image ~args  =
    RC.get { Run.pool } { Run.Key.image; args; docker_context; run_args }

  module PrC = Current_cache.Make(Pread)

  let pread ~docker_context ?pool ?(run_args=[]) image ~args =
    PrC.get { Pread.pool } { Pread.Key.image; args; docker_context; run_args }

  module TC = Current_cache.Output(Tag)

  let tag ~docker_context ~tag ~sw image =
    TC.set ~sw Tag.No_context { Tag.Key.tag; docker_context } { Tag.Value.image }

  module Push_cache = Current_cache.Output(Push)

  let push ~docker_context ?auth ~tag ~sw image =
    Push_cache.set ~sw auth { Push.Key.tag; docker_context } { Push.Value.image }

  module SC = Current_cache.Output(Service)

  let service ~docker_context ~name ~image ~sw () =
    SC.set ~sw Service.No_context { Service.Key.name; docker_context } { Service.Value.image }

  module CC = Current_cache.Output(Compose)

  let compose ?(pull=true) ~docker_context ~name ~contents ~sw () =
    CC.set ~sw Compose.{ pull } { Compose.Key.name; docker_context } { Compose.Value.contents }

  module CCC = Current_cache.Output(Compose_cli)

  let compose_cli ?(pull=true) ~docker_context ~name ~detach ~contents ~sw () =
    CCC.set ~sw Compose_cli.{ pull } { Compose_cli.Key.name; docker_context; detach } { Compose_cli.Value.contents }

  module Cmd = struct

    let ( >>!= ) = Result.bind

    type t = Current.Process.command

    let docker args ~docker_context = Cmd.docker ~docker_context args

    let rm_f id = docker ["container"; "rm"; "-f"; id]
    let kill id = docker ["container"; "kill"; id]

    (* Try to "docker kill $id". If it fails, just log a warning and continue. *)
    let try_kill_container ~docker_context ~job id =
      match Current.Process.exec ~cancellable:false ~job (kill ~docker_context id) with
      | Ok () -> ()
      | Error (`Msg m) -> Current.Job.log job "Warning: Failed to kill container %S: %s" id m

    let with_container ~docker_context ~kill_on_cancel ~job t fn =
      Current.Process.check_output ~cancellable:false ~job t >>!= fun id ->
      let id = String.trim id in
      let did_rm = ref false in
      let result =
        try
           begin
             if kill_on_cancel then (
               Current.Job.on_cancel job (fun _ ->
                   if !did_rm = false then try_kill_container ~docker_context ~job id
                )
             )
           end;
           fn id
        with ex -> (Fmt.error_msg "with_container: uncaught exception: %a" Fmt.exn ex)
      in
      did_rm := true;
      match Current.Process.exec ~cancellable:false ~job (rm_f ~docker_context id) with
      | Ok () -> result         (* (the common case, where removing the container succeeds) *)
      | Error (`Msg rm_error) as rm_e ->
        match result with
        | Ok _ -> rm_e
        | Error _ as e ->
          (* The job failed, and removing the container failed too.
             Log the second error and return the first. *)
          Current.Job.log job "Failed to remove container %S when job failed: %s" id rm_error;
          e

    let pp = Cmd.pp
  end
end

module Make (Host : S.HOST) = struct
  module Image = Image

  let docker_context = Host.docker_context

  let pp_opt_arch f = function
    | None -> ()
    | Some arch -> Fmt.pf f "@,%s" arch

  let pull ?label ?arch ~schedule ~sw tag =
    let label = Option.value label ~default:tag in
    Current.component "pull %s%a" label pp_opt_arch arch |>
    let> () = Current.return () in
    Raw.pull ~sw ~docker_context ~schedule ?arch tag

  let peek ?label ~arch ~schedule ~sw tag =
    let label = Option.value label ~default:tag in
    Current.component "peek %s@,%s" label arch |>
    let> () = Current.return () in
    Raw.peek ~sw ~docker_context ~schedule ~arch tag

  let pp_sp_label = Fmt.(option (sp ++ string))

  let get_build_context = function
    | `No_context -> Current.return `No_context
    | `Git commit -> Current.map (fun x -> `Git x) commit
    | `Dir path -> Current.map (fun path -> `Dir path) path

  let build ?level ?schedule ?timeout ?squash ?label ?dockerfile ?path ?pool ?build_args ~pull ~fs ~sw src =
    Current.component "build%a" pp_sp_label label |>
    let> commit = get_build_context src
    and> dockerfile = Current.option_seq dockerfile in
    Raw.build ~fs ~sw ~docker_context ?level ?schedule ?timeout ?squash ?dockerfile ?path ?pool ?build_args ~pull commit

  let run ?label ?pool ?run_args image ~args ~sw =
    Current.component "run%a" pp_sp_label label |>
    let> image = image in
    Raw.run ~sw ~docker_context ?pool ?run_args image ~args

  let pread ?label ?pool ?run_args image ~args ~sw =
    Current.component "pread%a" pp_sp_label label |>
    let> image = image in
    Raw.pread ~sw ~docker_context ?pool ?run_args image ~args

  let tag ~tag ~sw image =
    Current.component "docker-tag@,%a" pp_tag tag |>
    let> image = image in
    Raw.tag ~sw ~docker_context ~tag image

  let push ?auth ~tag ~sw image =
    Current.component "docker-push@,%a" pp_tag tag |>
    let> image = image in
    Raw.push ~sw ~docker_context ?auth ~tag image

  let service ~name ~image ~sw () =
    Current.component "docker-service@,%s" name |>
    let> image = image in
    Raw.service ~sw ~docker_context ~name ~image ()

  let compose ?pull ~name ~contents ~sw () =
    Current.component "docker-compose@,%s" name |>
    let> contents = contents in
    Raw.compose ~sw ?pull ~docker_context ~name ~contents ()

  let compose_cli ?pull ~name ~detach ~contents ~sw () =
    Current.component "docker-compose-cli@,%s" name |>
    let> contents = contents in
    Raw.compose_cli ~sw ?pull ~docker_context ~name ~detach ~contents ()
end

module Default = Make(struct
    let docker_context = Sys.getenv_opt "DOCKER_CONTEXT"
  end)

module MC = Current_cache.Output(Push_manifest)

let push_manifest ?auth ~tag ~fs ~sw manifests =
  Current.component "docker-push-manifest@,%a" pp_tag tag |>
  let> manifests = Current.list_seq manifests in
  MC.set ~sw (auth, fs) tag { Push_manifest.Value.manifests }

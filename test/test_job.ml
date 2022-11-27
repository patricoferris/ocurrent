module Job = Current.Job

let read path =
  let ch = open_in_bin (Fpath.to_string path) in
  let data = really_input_string ch (in_channel_length ch) in
  close_in ch;
  data

let ( >>!= ) x f =
  x |> function
  | Ok y -> f y
  | Error `Msg m -> failwith m

let streams sw () =
  Job.timestamp := (fun () -> 0.0);
  let switch = Current.Switch.create ~label:"streams" () in
  let config = Current.Config.v () in
  let job = Job.create ~sw ~switch ~label:"streams" ~config () in
  let log_data = Eio.Fiber.fork_promise ~sw (fun () -> Job.wait_for_log_data job) in
  assert (not (Eio.Promise.is_resolved log_data));
  let cmd = ("sh", [ "sh"; "-c"; "echo out1; echo >&2 out2; echo out3" ]) in
  Current.Process.exec ~cancellable:true ~job cmd >>!= fun () ->
  Eio.Promise.await @@ Current.Switch.turn_off switch;
  assert (Eio.Promise.is_resolved log_data);
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Combined results" "1970-01-01 00:00.00: Exec: (\"sh\", \"sh\" \"-c\" \"echo out1; echo >&2 out2; echo out3\")\n\
                                              out1\nout2\nout3\n" (read path)

let output sw () =
  Job.timestamp := (fun () -> 0.0);
  let switch = Current.Switch.create ~label:"output" () in
  let config = Current.Config.v () in
  let job = Job.create ~sw ~switch ~label:"output" ~config () in
  let cmd = ("sh", [ "sh"; "-c"; "echo out1; echo >&2 out2; echo out3" ]) in
  Current.Process.check_output ~cancellable:true ~job cmd >>!= fun out ->
  Eio.Promise.await @@ Current.Switch.turn_off switch;
  Alcotest.(check string) "Output" "out1\nout3\n" out;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: (\"sh\", \"sh\" \"-c\" \"echo out1; echo >&2 out2; echo out3\")\n\
                                 out2\n" (read path)

let pp_cmd ppf (v, args) =
  let remove_token s = 
    match Astring.String.cut ~sep:":" s with
    | Some ("token", _secret) -> "token:<TOKEN>"
    | _ -> s
  in
  Current.Process.pp_cmd ppf (v, List.map remove_token args)

let pp_command sw () =
  Job.timestamp := (fun () -> 0.0);
  let switch = Current.Switch.create ~label:"command" () in
  let config = Current.Config.v () in
  let job = Job.create ~sw ~switch ~label:"output" ~config () in
  let cmd = ("echo", [ "echo"; "token:abcdefgh" ]) in
  Current.Process.check_output ~pp_cmd ~cancellable:true ~job cmd >>!= fun out ->
  Eio.Promise.await @@ Current.Switch.turn_off switch;
  Alcotest.(check string) "Output" "token:abcdefgh\n" out;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: (\"echo\", \"echo\" \"token:<TOKEN>\")\n" (read path)

let cancel sw () =
  Job.timestamp := (fun () -> 0.0);
  let switch = Current.Switch.create ~label:"cancel" () in
  let config = Current.Config.v () in
  let job = Job.create ~sw ~switch ~label:"output" ~config () in
  let cmd = ("sleep", [ "sleep"; "120" ]) in
  let res = Eio.Fiber.fork_promise ~sw (fun () -> Current.Process.exec ~cancellable:true ~job cmd) in
  Current.Job.cancel job "Timeout";
  begin match Eio.Promise.await_exn res with
    | Ok () -> Alcotest.fail "Should have failed!"
    | Error `Msg m when Astring.String.is_prefix ~affix:"Command (\"sleep\", \"sleep\" \"120\") exited with status" m -> ()
    | Error `Msg m -> Alcotest.failf "Expected signal error, not %S" m
  end;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: (\"sleep\", \"sleep\" \"120\")\n\
                                 1970-01-01 00:00.00: Cancelling: Timeout\n" (read path)

let pp_promise f = function
  | None -> Fmt.string f "Sleep"
  | Some (Ok ()) -> Fmt.string f "Ok ()"
  | Some (Error ex) -> Fmt.exn f ex

let promise_state = Alcotest.testable pp_promise (=)

let pool sw () =
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 1 in
  let sw1 = Current.Switch.create ~label:"cancel-1" () in
  let sw2 = Current.Switch.create ~label:"cancel-2" () in
  let job1 = Job.create ~sw ~switch:sw1 ~label:"job-1" ~config () in
  let job2 = Job.create ~sw ~switch:sw2 ~label:"job-2" ~config () in
  let s1 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job1) in
  let s2 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job2) in
  (* Lwt.pause () >>= fun () -> *)
  Eio.Fiber.yield ();
  Alcotest.(check promise_state) "First job started" (Some (Ok ())) (Eio.Promise.peek s1);
  Alcotest.(check promise_state) "Second job queued" None (Eio.Promise.peek s2);
  Eio.Promise.await @@ Current.Switch.turn_off sw1;
  Eio.Fiber.yield ();
  (* XXX: Hmmm, one yield doesn't seem to be enough? *)
  let _r = Eio.Promise.await s2 in
  Alcotest.(check promise_state) "Second job ready" (Some (Ok ())) (Eio.Promise.peek s2);
  Eio.Promise.await @@ Current.Switch.turn_off sw2

let pool_cancel sw () =
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 0 in
  let sw1 = Current.Switch.create ~label:"cancel-1" () in
  let job1 = Job.create ~sw ~switch:sw1 ~label:"job-1" ~config () in
  let s1 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job1) in
  Alcotest.(check promise_state) "Job queued" None (Eio.Promise.peek s1);
  Current.Job.cancel job1 "Cancel";
  Eio.Fiber.yield ();
  let _r = Eio.Promise.await s1 in
  Job.log job1 "Continuing job for a bit";
  Alcotest.(check promise_state) "Job cancelled" (Some (Error (Failure "Cancelled waiting for resource from pool \"test\""))) (Eio.Promise.peek s1)

let pool_priority sw () =
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 1 in
  let sw1 = Current.Switch.create ~label:"cancel-1" () in
  let sw2 = Current.Switch.create ~label:"cancel-2" () in
  let sw3 = Current.Switch.create ~label:"cancel-3" () in
  let job1 = Job.create ~sw ~switch:sw1 ~label:"job-1" ~config () in
  let job2 = Job.create ~sw ~switch:sw2 ~label:"job-2" ~config () in
  let job3 = Job.create ~sw ~priority:`High ~switch:sw3 ~label:"job-3" ~config () in
  let s1 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job1) in
  let s2 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job2) in
  let s3 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job3) in
  Eio.Fiber.yield ();
  let _r = Eio.Promise.await s1 in
  Alcotest.(check promise_state) "First job started" (Some (Ok ())) (Eio.Promise.peek s1);
  Alcotest.(check promise_state) "Second job queued" None (Eio.Promise.peek s2);
  Alcotest.(check promise_state) "Third job queued" None (Eio.Promise.peek s3);
  Eio.Promise.await @@ Current.Switch.turn_off sw1;
  Eio.Fiber.yield ();
  let _r = Eio.Promise.await s3 in
  Alcotest.(check promise_state) "Second job queued" None (Eio.Promise.peek s2);
  Alcotest.(check promise_state) "High-priority third job ready" (Some (Ok ())) (Eio.Promise.peek s3);
  Eio.Promise.await @@ Current.Switch.turn_off sw3;
  Eio.Fiber.yield ();
  let _r = Eio.Promise.await s2 in
  Alcotest.(check promise_state) "Second job ready" (Some (Ok ())) (Eio.Promise.peek s2)

let tests =
  [
    Driver.test_case_gc "streams" streams;
    Driver.test_case_gc "output" output;
    Driver.test_case_gc "pp_cmd" pp_command;
    Driver.test_case_gc "cancel" cancel;
    Driver.test_case_gc "pool" pool;
    Driver.test_case_gc "pool_cancel" pool_cancel;
    Driver.test_case_gc "pool_priority" pool_priority;
  ]

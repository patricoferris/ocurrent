(** Like [Lwt_switch], but the cleanup functions are called in sequence, not
    in parallel, and a reason for the shutdown may be given. *)

type callback = unit -> unit

type t = {
  mutable state : [`On of string * callback Stack.t | `Turning_off of unit Eio.Promise.t | `Off];
}

let pp_reason f x = Current_term.Output.pp (Fmt.any "()") f (x :> unit Current_term.Output.t)

let turn_off t =
  match t.state with
  | `Off ->
    Log.debug (fun f -> f "Switch.turn_off: already off");
    Eio.Promise.create_resolved ()
  | `Turning_off thread ->
    thread
  | `On (_, callbacks) ->
    let th, set_th = Eio.Promise.create () in
    t.state <- `Turning_off th;
    let rec aux () =
      match Stack.pop callbacks with
      | fn -> fn () |> aux
      | exception Stack.Empty ->
        t.state <- `Off;
        Eio.Promise.resolve set_th ();
        th
    in
    aux ()

(* Once the first callback is added, attach a GC finaliser so we can detect if the user
   forgets to turn it off. *)
let gc ~sw t =
  match t.state with
  | `Off | `Turning_off _ -> ()
  | `On (label, _) ->
    Log.err (fun f -> f "Switch %S GC'd while still on!" label);
    Eio.Fiber.fork ~sw (fun () -> Eio.Promise.await @@ turn_off t)

let add_hook_or_fail t fn =
  (* XXX: Pretty sure this switch needs to be passed in! *)
  Eio.Switch.run @@ fun sw ->
  match t.state with
  | `On (_, callbacks) ->
    if Stack.is_empty callbacks then Gc.finalise (gc ~sw) t;
    Stack.push fn callbacks
  | `Off -> Fmt.failwith "Switch already off!"
  | `Turning_off _ -> Fmt.failwith "Switch is being turned off!"

let add_hook_or_exec t fn =
  (* XXX: Same with this switch! *)
  Eio.Switch.run @@ fun sw ->
  match t.state with
  | `On (_, callbacks) ->
    if Stack.is_empty callbacks then Gc.finalise (gc ~sw) t;
    Stack.push fn callbacks
  | `Off ->
    fn ()
  | `Turning_off thread ->
    Eio.Promise.await thread;
    fn ()

let add_hook_or_exec_opt t fn =
  match t with
  | None -> ()
  | Some t -> add_hook_or_exec t fn

let create ~label () = {
  state = `On (label, Stack.create ());
}

let create_off () = {
  state = `Off;
}

let is_on t =
  match t.state with
  | `On _ -> true
  | `Off | `Turning_off _ -> false

let pp f t =
  match t.state with
  | `On (label, _) -> Fmt.pf f "on(%S)" label
  | `Off -> Fmt.pf f "off"
  | `Turning_off _ -> Fmt.string f "turning-off"

val input_installation_webhook : unit -> unit
(** Call this when we get the "installation" event. *)

(* Public API; see Current_git.mli for details of these: *)

type t

val webhook_secret : t -> string
val cmdliner : net:Eio.Net.t -> sw:Eio.Switch.t -> t Cmdliner.Term.t
val cmdliner_opt : net:Eio.Net.t -> sw:Eio.Switch.t ->  t option Cmdliner.Term.t
val installation : net:Eio.Net.t -> sw:Eio.Switch.t -> t -> account:string -> int -> Installation.t
val installations : net:Eio.Net.t -> sw:Eio.Switch.t -> t -> Installation.t list Current.t

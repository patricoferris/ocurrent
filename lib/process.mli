type command = string * string list

val pp_cmd : Format.formatter -> command -> unit

val exec :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> command -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> command ->
  unit Current_term.S.or_error

val check_output :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> command -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> command ->
  string Current_term.S.or_error

val with_tmpdir : ?prefix:string -> Eio.Fs.dir Eio.Path.t -> (Eio.Fs.dir Eio.Path.t -> 'a) -> 'a

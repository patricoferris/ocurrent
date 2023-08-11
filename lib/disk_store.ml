let state_dir_root = 
  match Sys.getenv_opt "CURRENT_DIR_PREFIX" with
  | Some dir ->
    Fpath.v @@ Filename.concat dir "var"
  | None -> failwith "Must set CURRENT_DIR_PREFIX in the environment"

let state_dir name =
  let name = Fpath.v name in
  assert (Fpath.is_rel name);
  let path = Fpath.append state_dir_root name in
  match Bos.OS.Dir.create path with
  | Ok (_ : bool) -> path
  | Error (`Msg m) -> failwith m

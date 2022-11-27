let tls_config =
  Mirage_crypto_rng_unix.initialize ();
  let null ?ip:_ ~host:_ _certs = Ok None in
  Tls.Config.client ~authenticator:null ()


let (>>?=) f v = Result.bind f v

let host_and_path uri =
  match Uri.host uri, Uri.path uri with
  | Some host, path -> Ok (host, path)
  | _ -> Error (`Msg ("Failed to extract host from " ^ Uri.to_string uri))

let get ?headers ~net uri =
  let open Eio in
  host_and_path uri >>?= fun (host, path) ->
  match Net.getaddrinfo_stream ~service:"https" net host with
  | [] -> Error (`Msg "Host resolution failed")
  | stream :: _ ->
    Switch.run @@ fun sw ->
    let conn = Net.connect ~sw net stream in
    let conn =
      Tls_eio.client_of_flow tls_config
        ?host:
          (Domain_name.of_string_exn host
          |> Domain_name.host |> Result.to_option)
        conn
    in
    Ok (Cohttp_eio.Client.get ?headers ~conn (host, None) path)

let post ?headers ~net ~body uri =
  let open Eio in
  host_and_path uri >>?= fun (host, path) ->
  match Net.getaddrinfo_stream ~service:"https" net host with
  | [] -> Error (`Msg "Host resolution failed")
  | stream :: _ ->
    Switch.run @@ fun sw ->
    let conn = Net.connect ~sw net stream in
    let conn =
      Tls_eio.client_of_flow tls_config
        ?host:
          (Domain_name.of_string_exn host
          |> Domain_name.host |> Result.to_option)
        conn
    in
    Ok (Cohttp_eio.Client.post ?headers ~conn ~body:(Cohttp_eio.Body.Fixed body) (host, None) path)
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic
open Yojson.Basic.Util

let find_field json field_name =
  let open Yojson.Basic.Util in
  let first_item = List.hd (to_list json) in
  let symbol = first_item |> member field_name |> to_float in
  symbol

let ping (ticker : string) () =
  let uri =
    Uri.of_string
      ("https://yahoo-finance15.p.rapidapi.com/api/yahoo/qu/quote/" ^ ticker)
  in

  let headers =
    Header.init () |> fun h ->
    Header.add h "X-RapidAPI-Key"
      "9c784e7fd4msh27e0c693f9db50bp194ed1jsnae17d653ab59"
    |> fun h -> Header.add h "X-RapidAPI-Host" "yahoo-finance15.p.rapidapi.com"
  in

  Lwt_main.run
    ( Client.get ~headers uri >>= fun (resp, body) ->
      (* let status = Response.status resp in Printf.printf "Response status:
         %s\n" (Code.string_of_status status); *)
      Cohttp_lwt__.Body.to_string body >>= fun body_str ->
      (* Printf.printf "Response body: %s\n" body_str; *)
      let json = Yojson.Basic.from_string body_str in
      Lwt.return json )

let get_price ticker () =
  let json = ping ticker () in
  let symbol = find_field json "regularMarketPrice" in
  symbol

let test () =
  let value = get_price "MSFT" () in
  Printf.printf "The current
\n\n\n\n\n   \n market price is: %f\n" value

let x = test ()

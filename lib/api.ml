open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic
open Unix
open Exception

let ping (ticker : string) (uri : Uri.t) () =
  let headers =
    Header.init () |> fun h ->
    Header.add h "X-RapidAPI-Key"
      "5d3445235emshfc2077d35576f7ep154ce0jsnc22c8c609a52"
    |> fun h -> Header.add h "X-RapidAPI-Host" "apistocks.p.rapidapi.com"
  in

  Lwt_main.run
    ( Client.get ~headers uri >>= fun (resp, body) ->
      Cohttp_lwt__.Body.to_string body >>= fun body_str ->
      let json = Yojson.Basic.from_string body_str in
      Lwt.return json )

let ping_id (ticker : string) () =
  let uri =
    Uri.of_string
      ("https://apistocks.p.rapidapi.com/intraday?symbol=" ^ ticker
     ^ "&interval=5min&maxreturn=100")
  in
  ping ticker uri ()

let ping_day ticker (start_day : string) (end_day : string) () =
  let uri =
    Uri.of_string
      ("https://apistocks.p.rapidapi.com/daily?symbol=" ^ ticker ^ "&dateStart="
     ^ start_day ^ "&dateEnd=" ^ end_day)
  in
  ping ticker uri ()

let print_json ticker () =
  let json_result = ping_id ticker () in
  Yojson.Basic.pretty_to_string json_result

let print_json_results ticker () =
  let json_result = ping_id ticker () in
  match json_result with
  | `Assoc fields ->
      let results =
        match List.assoc "Results" fields with
        | `List results -> results
        | _ -> []
      in
      Yojson.Basic.pretty_to_string (`List results)
  | _ -> raise InvalidJSONFormat

let print_last_results ticker () =
  let json_data = ping_id ticker () in
  match json_data with
  | `Assoc fields -> (
      let results =
        match List.assoc "Results" fields with
        | `List results -> results
        | _ -> []
      in
      match List.rev results with
      | `Assoc result_fields :: _ ->
          Yojson.Basic.to_string (`Assoc result_fields)
      | _ -> raise NoResultsFound)
  | _ -> raise InvalidJSONFormat

let get_date_current ticker json_data =
  match json_data with
  | `Assoc fields -> (
      let results =
        match List.assoc "Results" fields with
        | `List results -> results
        | _ -> []
      in
      match List.rev results with
      | `Assoc result_fields :: _ -> (
          match List.assoc "Date" result_fields with
          | `String date -> date
          | _ -> raise NoDateFound)
      | _ -> raise NoResultsFound)
  | _ -> raise InvalidJSONFormat

let get_price_current ticker json_data =
  match json_data with
  | `Assoc fields -> (
      let results =
        match List.assoc "Results" fields with
        | `List results -> results
        | _ -> []
      in
      match List.rev results with
      | `Assoc result_fields :: _ -> (
          match List.assoc "Close" result_fields with
          | `Float close_price -> close_price
          | _ -> raise NoClosingPriceFound)
      | _ -> raise NoResultsFound)
  | _ -> raise InvalidJSONFormat

let get_id ticker =
  let json_data = ping_id ticker () in
  (get_price_current ticker json_data, get_date_current ticker json_data)

let get_price ticker = fst (get_id ticker)
let local_time = Unix.localtime (Unix.time ())

let get_yr_m_d t =
  let year = Printf.sprintf "%04d" (t.tm_year + 1900) in
  let month = Printf.sprintf "%02d" (t.tm_mon + 1) in
  let day = Printf.sprintf "%02d" t.tm_mday in
  year ^ "-" ^ month ^ "-" ^ day

let get_hr_min t =
  let hr = Printf.sprintf "%02d" t.tm_hour in
  let min = Printf.sprintf "%02d" t.tm_min in
  hr ^ ":" ^ min

let get_time (t: Unix.tm) =
  (get_yr_m_d t) ^ " " ^ (get_hr_min t)
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic
open Unix

let ping_id (ticker : string) () =
  let uri =
    Uri.of_string
      ("https://apistocks.p.rapidapi.com/intraday?symbol=" ^ ticker
     ^ "&interval=5min&maxreturn=100")
  in

  let headers =
    Header.init ()
    |> fun h ->
    Header.add h "X-RapidAPI-Key" "5d3445235emshfc2077d35576f7ep154ce0jsnc22c8c609a52"
    |> fun h ->
    Header.add h "X-RapidAPI-Host" "apistocks.p.rapidapi.com"
  in

  Lwt_main.run
    (Client.get ~headers uri
     >>= fun (resp, body) ->
     Cohttp_lwt__.Body.to_string body
     >>= fun body_str ->
     let json = Yojson.Basic.from_string body_str in
     Lwt.return json)

let print_json ticker () =
  let json_result = (ping_id ticker ()) in
  let json_string = Yojson.Basic.pretty_to_string json_result in
  print_endline json_string

let print_json_results ticker () =
  let json_result = (ping_id ticker ()) in
  match json_result with
  | `Assoc fields ->
      let results =
        match List.assoc "Results" fields with
        | `List results -> results
        | _ -> []
      in
      let results_string = Yojson.Basic.pretty_to_string (`List results) in
      print_endline results_string
  | _ -> print_endline "Invalid JSON format"

let get_closing_price ticker date_time =
  let json_data = ping_id ticker () in
  match json_data with
  | `Assoc fields ->
      let results =
        match List.assoc "Results" fields with
        | `List results -> results
        | _ -> []
      in
      let matching_result =
        List.find_opt
          (function
            | `Assoc result_fields ->
                let date_value =
                  List.assoc "Date" result_fields |> function
                  | `String date -> date
                  | _ -> ""
                in
                date_value = date_time
            | _ -> false)
          results
      in
      (match matching_result with
      | Some (`Assoc result_fields) ->
          (match List.assoc "Close" result_fields with
          | `Float close_price -> close_price
          | _ -> failwith "Closing price not found")
      | _ -> failwith "Date and time not found")
  | _ -> failwith "Invalid JSON format"

let local_time = Unix.localtime (Unix.time())

let get_hr_min =
  let round_by_five n = (n + 2) / 5 * 5 in
  let t = local_time in
  let hr = Printf.sprintf "%02d" t.tm_hour in
  let rounded_min =
    if t.tm_min > 10 then round_by_five t.tm_min - 20
    else round_by_five t.tm_min + 40
  in
  let min = Printf.sprintf "%02d" rounded_min in
  hr ^ ":" ^ min

let get_time =
  let t = local_time in
  let year = Printf.sprintf "%04d" (t.tm_year + 1900) in
  let month = Printf.sprintf "%02d" (t.tm_mon + 1) in
  let day = Printf.sprintf "%02d" t.tm_mday in
  year ^ "-" ^ month ^ "-" ^ day ^ " " ^ get_hr_min

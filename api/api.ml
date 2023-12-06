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

let print_last_results ticker () =
  let json_data = ping_id ticker () in
  match json_data with
  | `Assoc fields ->
      let results =
        match List.assoc "Results" fields with
        | `List results -> results
        | _ -> []
      in
      (match List.rev results with
      | (`Assoc result_fields)::_ ->
          print_endline (Yojson.Basic.to_string (`Assoc result_fields))
      | _ -> invalid_arg "No results found")
  | _ -> invalid_arg "Invalid JSON format"

let get_date_current ticker json_data =
  match json_data with
  | `Assoc fields ->
      let results =
        match List.assoc "Results" fields with
        | `List results -> results
        | _ -> []
      in
      (match List.rev results with
      | (`Assoc result_fields)::_ ->
          (match List.assoc "Date" result_fields with
          | `String date -> date
          | _ -> invalid_arg "Date not found")
      | _ -> invalid_arg "No results found")
  | _ -> invalid_arg "Invalid JSON format"

let get_price_current ticker json_data =
  match json_data with
  | `Assoc fields ->
      let results =
        match List.assoc "Results" fields with
        | `List results -> results
        | _ -> []
      in
      (match List.rev results with
      | (`Assoc result_fields)::_ ->
          (match List.assoc "Close" result_fields with
          | `Float close_price -> close_price
          | _ -> invalid_arg "Closing price not found")
      | _ -> invalid_arg "No results found")
  | _ -> invalid_arg "Invalid JSON format"

let get ticker = 
  let json_data = ping_id ticker () in
    (get_price_current ticker json_data, get_date_current ticker json_data)

let get_price ticker = fst (get ticker)

let local_time = Unix.localtime (Unix.time())

let get_time =
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
  in
  let t = local_time in
  let year = Printf.sprintf "%04d" (t.tm_year + 1900) in
  let month = Printf.sprintf "%02d" (t.tm_mon + 1) in
  let day = Printf.sprintf "%02d" t.tm_mday in
  year ^ "-" ^ month ^ "-" ^ day ^ " " ^ get_hr_min

let unix_time_of_string datetime_str =
  let split_datetime datetime_str =
    match String.split_on_char ' ' datetime_str with
    | [date; time] -> Some (String.split_on_char '-' date, String.split_on_char ':' time)
    | _ -> None
  in
  match split_datetime datetime_str with
  | Some (date_parts, time_parts) ->
    (match date_parts, time_parts with
    | [year; month; day], [hour; minute] ->
      let tm = {tm_year = int_of_string year - 1900; 
                tm_mon = int_of_string month - 1; tm_mday = int_of_string day;
                tm_hour = int_of_string hour; tm_min = int_of_string minute; 
                tm_sec = 0; tm_wday = 0; tm_yday = 0; tm_isdst = false} in
      Some (Unix.mktime tm)
    | _ -> None)
  | None -> None

let unix_time datetime_str =
  match unix_time_of_string datetime_str with
    Some (f, _) -> f
    | None -> failwith "Incorrect date time entered"

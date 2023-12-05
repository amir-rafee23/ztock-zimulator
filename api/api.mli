(** Pings an API to return the Yojson.Basic.t json representation allowing for
    scraping, for intraday trading
    *)
val ping_id : string -> unit -> Yojson.Basic.t
(** Prints the full json returned by ping for a given ticker *)
val print_json : string -> unit -> unit
(** Prints only the Results portion of the json for a given ticker*)
val print_json_results : string -> unit -> unit
(** returns the close price [float] for a given ticker at a given date_time*)
val get_price : string -> string (* data_time *) -> float
(** the current local time*)
val local_time : Unix.tm
(** gets the appropriate formatting for hr and min,
    delays it by 20 minute to ensure API is updated (as only updates about 20
    minutes behind)*)
val get_hr_min : string
(** formats time as "YYYY-MM-DD HH-mm"*)
val get_time : string
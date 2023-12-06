(** Pings an API to return the Yojson.Basic.t json representation allowing for
    scraping, for intraday trading*)
val ping_id : string -> unit -> Yojson.Basic.t
(** Prints the full json returned by ping for a given ticker *)
val print_json : string -> unit -> string
(** Prints only the Results portion of the json for a given ticker*)
val print_json_results : string -> unit -> string
(** return the last results, date, price, etc.*)
val print_last_results : string -> unit -> string
(** returns last close price *)
val get_price_current : string -> Yojson.Basic.t -> float
(** returns the date of the last recorded price*)
val get_date_current : string -> Yojson.Basic.t -> string
(** returns the date of last recorded price and the price [price, date]*)
val get : string -> float * string
(** returns fst get ticker*)
val get_price : string -> float
(** the current local time*)
val local_time : Unix.tm
(** formats time as "YYYY-MM-DD HH-mm"*)
val get_time : string
(** returns option [float * Unix.tm of some string is json date formatting]*)
val unix_time_of_string : string -> (float * Unix.tm) option
(** returns some date time formatting as just a float unix time
    YYYY-MM-DD HH-mm -> a float *)
val unix_time : string -> float

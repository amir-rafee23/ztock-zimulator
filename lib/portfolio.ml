open Unix
open Api

(** The signature of a user's portfolio. *)
module type PortfolioType = sig
  type t

  val empty_portfolio : t
  val contains_stock : t -> string -> bool
  val quantity_stock : t -> string -> int
  val stock_price_over_time : t -> string -> int list
  val add_stock : t -> string -> int -> t
  val remove_stock : t -> string -> int -> t
  val batches_data : t -> string -> string -> (float * int * float) list
  val display_portfolio : t -> string
  val cost_basis : t -> string -> int option
  val display_portfolio_filesys : t -> string list list
end

(** A Map whose keys are strings. *)
module String_map : Map.S with type key = string = Map.Make (struct
  type t = string

  (* Note that [compare] works on the keys, which are strings. *)
  let compare = String.compare
end)

(* No module type annotation to allow access to the concrete values, for
   [filesys]. Separate check that [UserPortfolio] satisfies [PortfolioType] done
   later. *)
module UserPortfolio = struct
  (** Representation type: A Map from stock tickers (keys) to data regarding the
      stock: 1) quantity currently held 2) most recent date the stock went from
      being absent to present in the portfolio 3) buy batches 4) sell batches
      stored in a record (values).

      A buy/sell batch contains information regarding quantity, price, date, and
      order type (buy/sell). Separate function to calculate cost-basis (net
      amount of money spent to get that quantity of shares / total number of
      current shares).

      Note that all dates/times used are in epoch time. *)

  type batches_element_data = {
    price : float;
    quantity : int;
    (* Epoch time.*)
    date : float;
  }

  type stock_data = {
    quantity : int;
    (* Epoch time. *)
    initial_buy_date : float;
    buy_batches : batches_element_data list;
    sell_batches : batches_element_data list;
  }

  type t = stock_data String_map.t

  let empty_portfolio = String_map.empty

  let contains_stock (portfolio : t) (stock : string) =
    String_map.mem stock portfolio

  let quantity_stock (portfolio : t) (stock : string) : int =
    let data = String_map.find_opt stock portfolio in
    match data with
    (* [stock] not in portfolio. *)
    | None -> 0
    (* [stock] in portfolio. *)
    | Some x -> x.quantity

  let stock_price_over_time (portfolio : t) (stock : string) : int list =
    failwith "unimplemented"

  let add_stock (portfolio : t) (stock : string) (qty : int) : t =
    (* Precondition. *)
    (* TODO: Add descriptive error message. *)
    assert (qty >= 0);

    (* Check if [stock] is in [portfolio]. *)
    if String_map.mem stock portfolio = false then
      (* Initialize the stock's data (the value in the portfolio Map). *)

      (* Quantity of the stock. *)
      let q = qty in

      (* Set [initial_buy_date]. *)
      let i = Unix.time () in

      (* Initialize [buy_batches]. *)
      let b = [ { price = Api.get_price stock; quantity = q; date = i } ] in

      (* Initialize [sell_batches]. *)
      let s = [] in

      (* The stock's data. *)
      let data =
        {
          quantity = q;
          initial_buy_date = i;
          buy_batches = b;
          sell_batches = s;
        }
      in

      (* Create the binding. *)
      String_map.add stock data portfolio
    else
      (* [stock] already in portfolio. Need to update the stock's data. Only the
         quantity and buy batches are changed. *)

      (* New quantity of the stock. *)
      let q = quantity_stock portfolio stock + qty in

      (* New [buy_batches]. *)
      let old_b = (String_map.find stock portfolio).buy_batches in
      (* TODO: Real-time price.*)
      let b =
        old_b
        @ [
            {
              price = Api.get_price stock;
              (* Amount of [stock] added in just this buy order, NOT the total
                 quantity of [stock] currently held.*)
              quantity = qty;
              date = Unix.time ();
            };
          ]
      in

      (* New stock data. *)
      let old_data = String_map.find stock portfolio in

      let new_data = { old_data with quantity = q; buy_batches = b } in

      (* Update the binding. *)
      String_map.add stock new_data portfolio

  let remove_stock (portfolio : t) (stock : string) (qty : int) : t =
    (* Precondition. *)
    assert (qty >= 0);

    (* Check if [stock] is in [portfolio.] *)
    if
      String_map.mem stock portfolio = false
      (* Not in portfolio. Return unchanged portfolio. *)
    then portfolio
    else if
      (* In portfolio. Check if the quantity to be removed exceeds the initial
         quantity. *)
      qty > (String_map.find stock portfolio).quantity
    then
      failwith
        ("Quantity of " ^ stock
       ^ " to be removed exceeds initial quantity held!")
    else
      (* Need to update the stock data. Only quantity, sell_batches are
         changed. *)

      (* New quantity. *)
      let q = (String_map.find stock portfolio).quantity - qty in

      (* If new quantity is 0, then remove stock from the portfolio and
         return. *)
      if q = 0 then String_map.remove stock portfolio
      else
        (* New sell_batches. *)
        (*TODO: price*)
        let s =
          (String_map.find stock portfolio).sell_batches
          @ [
              {
                price = Api.get_price stock;
                (* Amount of [stock] removed in just this sell order, NOT the
                   total quantity of [stock] currently held.*)
                quantity = qty;
                date = Unix.time ();
              };
            ]
        in

        (* New stock data. *)
        let old_data = String_map.find stock portfolio in
        let data = { old_data with quantity = q; sell_batches = s } in

        (* Update binding. *)
        String_map.add stock data portfolio

  (** Helper to [batches_data]. Processes [batches] to return a list of tuples,
      each of the form (price, quantity, epoch time) for each order in
      [batches]. *)
  let rec get_batches_data (batches : batches_element_data list) :
      (float * int * float) list =
    match batches with
    | [] -> []
    | data :: t ->
        [ (data.price, data.quantity, data.date) ] @ get_batches_data t

  let batches_data (portfolio : t) (batches_type : string) (stock : string) :
      (float * int * float) list =
    if contains_stock portfolio stock = false then []
    else if batches_type = "buy" then
      get_batches_data (String_map.find stock portfolio).buy_batches
    else get_batches_data (String_map.find stock portfolio).sell_batches

  let rec display_portfolio (portfolio : t) : string =
    (* Stocks held in [portfolio]. *)
    let stocks = String_map.bindings portfolio in

    match stocks with
    | [] -> "Portfolio is empty."
    | (stock, data) :: t ->
        (* Data on [stock]. *)

        (* Determine the string representation of the epoch initial buy date.
           month/day/year hour:min:sec is the output format. *)
        let local_time = Unix.localtime data.initial_buy_date in
        let str_date_time =
          (* TODO: Factor out common code (maybe use map). *)

          (* tm_mon gives the month - 1*)
          (local_time.tm_mon + 1 |> string_of_int)
          ^ "/"
          ^ (local_time.tm_mday |> string_of_int)
          ^ "/"
          (*tm_year gives the year - 1900*)
          ^ (local_time.tm_year + 1900 |> string_of_int)
          ^ " "
          ^ (local_time.tm_hour |> string_of_int)
          ^ ":"
          ^ (local_time.tm_min |> string_of_int)
          ^ ":"
          ^ (local_time.tm_sec |> string_of_int)
        in

        let current_price = Api.get_price stock in

        (* TODO: this function just returns a string. No actual printing is
           done. Find another reason for bug with Zach's code. *)
        let str =
          Printf.sprintf
            "STOCK: %s\n\
             Quantity: %i\n\
             Current price: $%F\n\
             Initial buy date: %s\n\
             Current total holding value: $%F%!\n"
            stock data.quantity current_price str_date_time
            (current_price *. float_of_int data.quantity)
        in
        (* Handle possibly printing more stocks. *)
        if t = [] then str
        else
          (* More stocks to be printed. *)
          let remaining_portfolio = String_map.remove stock portfolio in
          str ^ "\n\n" ^ display_portfolio remaining_portfolio

  let cost_basis (portfolio : t) (stock : string) : int option =
    failwith "unimplemented"

  (* TODO: Make more concise, add test caes. *)
  let rec display_portfolio_filesys (portfolio : t) : string list list =
    (* List to be returned. *)
    let result =
      [
        [ "ticker" ];
        [ "quantity" ];
        [ "initial buy date" ];
        [ "buy batches" ];
        [ "sell batches" ];
      ]
    in

    (* All the information in [portfolio]. *)
    let all_info = String_map.bindings portfolio in

    match all_info with
    | [] -> result (* Empty [portfolio]. *)
    | (ticker, stock_info) :: _ ->
        (* Add the head's info to [result] first. *)

        (* Add the ticker. *)
        let new_tickers = List.nth result 0 @ [ ticker ] in
        (* Add the quantity. *)
        let new_quantities =
          List.nth result 1 @ [ string_of_int stock_info.quantity ]
        in
        (* Add the initial buy date. *)
        let new_ibds =
          List.nth result 2 @ [ string_of_float stock_info.initial_buy_date ]
        in
        (* Add the buy batches. *)
        let new_bbs =
          List.nth result 3 @ [ batches_to_string stock_info.buy_batches ]
        in
        (* Add the sell batches. *)
        let new_sbs =
          List.nth result 4 @ [ batches_to_string stock_info.sell_batches ]
        in

        (* Handle the tail now. *)
        let remaining_portfolio = String_map.remove ticker portfolio in
        let tail_list = display_portfolio_filesys remaining_portfolio in

        (* Collect the tail's data. Removal of column titles performed. *)
        let tail_tickers = List.nth tail_list 0 |> List.tl in
        let tail_quantities = List.nth tail_list 1 |> List.tl in
        let tail_ibds = List.nth tail_list 2 |> List.tl in
        let tail_bbs = List.nth tail_list 3 |> List.tl in
        let tail_sbs = List.nth tail_list 4 |> List.tl in

        (* Build up the final data columns. *)
        let final_tickers = new_tickers @ tail_tickers in
        let final_quantities = new_quantities @ tail_quantities in
        let final_ibds = new_ibds @ tail_ibds in
        let final_bbs = new_bbs @ tail_bbs in
        let final_sbs = new_sbs @ tail_sbs in

        (* Output. *)
        [ final_tickers; final_quantities; final_ibds; final_bbs; final_sbs ]

  (* Need to convert [sell_batches] to a string. *)

  (* [batches_to_string batches] is the string representation of [batches]. The
     output has the form: "[{price_1; quantity_1; date_1}; ...; {price_n;
     quantity_n; date_n}]". Returns "[]" if [batches] is empty. *)
  and batches_to_string (batches : batches_element_data list) : string =
    match batches with
    | [] -> "[]"
    | batch :: t ->
        let batch_string =
          "{"
          ^ string_of_float batch.price
          ^ "; "
          ^ string_of_int batch.quantity
          ^ "; " ^ string_of_float batch.date ^ "}"
        in

        let t_string = batches_to_string t in

        begin
          match t_string with
          | "[]" -> "[" ^ batch_string ^ "]"
          | t_string ->
              "[" ^ batch_string ^ "; "
              (* Remove the opening "[". *)
              ^ String.sub t_string 1 (String.length t_string - 1)
        end
end

(* TODO: *)
(* Idea is to evenly split the interval between current time and when the stock
   was most recently added to portfolio to get a series of dates (how many?),
   then ping the API to get the price at each of those dates and output the
   results in a list. Returns the empty list only if the stock is not in the
   portfolio. *)

(* Worry about the cost-basis function later. *)

(* Check that [UserPortfolio] satisfies [PortfolioType]. *)
module _ : PortfolioType = UserPortfolio

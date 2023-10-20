(** The signature of a user's portfolio. *)
module type PortfolioType = sig
  type 'a t

  val empty_portfolio : 'a t
  val contains_stock : 'a t -> string -> bool
  val quantity_stock : 'a t -> string -> int
  val stock_price_over_time : 'a t -> string -> int list
  val add_stock : 'a t -> string -> int -> 'a t
  val remove_stock : 'a t -> string -> 'a t
  val display_portfolio : 'a t -> string
  val cost_basis : 'a t -> string -> int option
end

(** A Map whose keys are strings. *)
module String_map : Map.S with type key = string = Map.Make (struct
  type t = string

  (* Note that [compare] works on the keys, which are strings. *)
  let compare = String.compare
end)

module UserPortfolio : PortfolioType = struct
  (** Representation type: A Map from stock tickers (keys) to data regarding the
      stock: 1) quantity currently held 2) most recent date the stock went from
      being absent to present in the portfolio 3) buy batches 4) sell batches
      stored in a record (values).

      A buy/sell batch contains information regarding quantity, price, date, and
      order type (buy/sell). Separate function to calculate cost-basis (net
      amount of money spent to get that quantity of shares / total number of
      current shares). .*)

  type batches_element_data = {
    price : int;
    quantity : int;
    date : string;
  }

  type stock_data = {
    quantity : int;
    initial_buy_date : string;
    buy_batches : batches_element_data list;
    sell_batches : batches_element_data list;
  }

  (* Doing this to remove compiler errors. *)
  (* let x = { price = 1; quantity = 1; date = "date" }

     let y = { quantity = 2; initial_buy_date = "date2"; buy_batches = [];
     sell_batches = []; } *)

  type 'a t = stock_data String_map.t

  let empty_portfolio = String_map.empty

  let contains_stock (portfolio : 'a t) (stock : string) =
    String_map.mem stock portfolio

  let quantity_stock = failwith "unimplemented"
  let stock_price_over_time = failwith "unimplemented"
  let add_stock = failwith "Unimplemented"
  let remove_stock = failwith "unimplemented"
  let display_portfolio = failwith "unimplemented"
  let cost_basis = failwith "unimplemented"
end

(* TODO: *)
(* Idea is to evenly split the interval between current time and when the stock
   was most recently added to portfolio to get a series of dates (how many?),
   then ping the API to get the price at each of those dates and output the
   results in a list. Returns the empty list only if the stock is not in the
   portfolio. *)

(* Epoch time. to_epoch. *)

(* Worry about the cost-basis function later. *)

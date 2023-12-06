(** The signature of a user's portfolio. *)
module type PortfolioType = sig
  type t
  (** Type representing the data in the portfolio. *)

  val empty_portfolio : t
  (** A completely empty portfolio. *)

  (* NOTE: The stock name input is its ticker name. *)

  val contains_stock : t -> string -> bool
  (** Check whether a stock is held in the portfolio. *)

  val quantity_stock : t -> string -> int
  (** Returns the quantity of a stock held in the portfolio. *)

  (* TODO: Could make this function more general, and just have it display the
     price of some stock from a certain start date to the present date. *)
  val stock_price_over_time : t -> string -> int list
  (** Returns the prices of a stock over time, specifically, from the most
      recent time when the stock went from being absent to present in the
      portfolio, to when this function is called. Returns the empty list if and
      only if the stock is not in the portfolio. *)

  (* TODO: Add an optional argument [time] to add_stock and remove_stock, mainly
     to enable better testing.

     Optional argument [time]: the epoch time associated with the stock addition
     to the portfolio. If an argument not provided, default value is the time
     the function is called. *)

  val add_stock : t -> string -> int -> t
  (** Add a stock and the quantity purchased to the portfolio.

      The time associated with a stock addition to a portfolio is the time this
      function is called.

      Requires: quantity of stoack to be added >= 0.*)

  val remove_stock : t -> string -> int -> t
  (** Remove a certain quantity of a certain stock from the portfolio. If the
      stock is not in the portfolio, returns the unchanged portfolio. If the
      quantity of stock to be removed is equal to the initial quantity, the
      stock is completely removed from the portfolio. If the quantity of stock
      to be removed is greater than the initial quantity, raises [Failure].

      The time associated with a stock removal from a portfolio is the time this
      function is called.

      Requires: quantity of stock to be removed >= 0.*)

  val batches_data : t -> string -> string -> (float * int * float) list
  (** Returns information regarding the individual orders in a buy/sell batch
      for a certain stock.

      Specifically, returns a list of tuples, each of the form (price, quantity,
      epoch time) for each order in a buy/sell batch.

      Tuples should be ordered from the first buy/sell order to the most recent
      buy/sell order.

      Returns the empty list if the stock is not in the portoflio.

      Requires: first argument is either "buy"/"sell". *)

  (* TODO: Also display current portfolio's value. Need a function for that.*)
  val display_portfolio : t -> string
  (** Returns a "pretty-printer" string to display the portfolio. For each stock
      in the portfolio, displays: stock name, quantity, current price, current
      total holding value, initial buy date and time (month/day/year
      hour:min:sec format).*)

  val cost_basis : t -> string -> int option
  (** The cost-basis of a certain stock held in a portfolio. Returns [None] if
      the stock is not in the portfolio. *)

  val display_portfolio_filesys : t -> string list list
  (** [display_portfolio_filesys portfolio] is a list containing exactly all 
  the data stored in the underlying concrete value of [portfolio]. 
      
      Example output: 
      [["ticker: AAPL"]; ["initial_buy_date: 324234.02"]; 
      ["buy_batches: [{pb_1, qb_1, db_1}; ... ;{pb_m,qb_m,db_m}]"]; 
      ["sell_batches: [{ps_1, qs_1, ds_1}; ... {ps_n, qs_n, ds_n}"]]. *)
end

module String_map : Map.S with type key = string
(** A Map whose keys are strings. *)

module UserPortfolio : PortfolioType
(** A user's portfolio. *)

(** [Portfolio] has functions related to how stock and portfolio data is stored
    and retrieved*)

(** The general signature of a user's portfolio. *)
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

  val cost_basis : t -> string -> float option
  (** [cost_basis portfolio stock] is the cost-basis of [stock] in [portfolio]*)

  val display_portfolio : t -> string list
  (** Returns a "pretty-printer" string to display the portfolio. For each stock
      in the portfolio, displays: stock name, quantity, current price, current
      total holding value, initial buy date and time (month/day/year
      hour:min:sec format).*)

  val display_portfolio_filesys : t -> string list list
  (** [display_portfolio_filesys portfolio] is a list containing exactly all the
      data stored in the underlying concrete value of [portfolio], which is, for
      each stock, the ticker name, quantity currently held, initial buy date,
      buy batches and sell batches. Each inner list is a row, with each row
      itself being a list of column entries.

      Example output:

      [[
    ["ticker"; "quantity"; "initial buy date"; "buy batches"; "sell batches"];
    ["AAPL"; "66"; "32423.2"; "[{pb_1, qb_1, db_1}; ... ;{pb_m, qb_m, db_m}]"; 
    "[{ps_1, qs_1, ds_1}; ... ;{ps_n, qs_n, ds_n}]"]
    ]]
      .*)
end

module String_map : Map.S with type key = string
(** A Map whose keys are strings. *)

(** Repreentation type exposed for use in [filesys.ml]. *)
module type UserPortfolioType = sig
  type batches_element_data = {
    price : float;
    quantity : int;
    date : float;
  }

  type stock_data = {
    quantity : int;
    initial_buy_date : float;
    buy_batches : batches_element_data list;
    sell_batches : batches_element_data list;
  }

  include PortfolioType with type t = stock_data String_map.t

  (* [batches_of_string] used in [filesys.ml]. Its spec refers to
     [batches_to_string], which is why the latter is also included below, even
     though it is not used outside of [portfolio.ml]. *)

  val batches_to_string : batches_element_data list -> string
  (** [batches_to_string batches] is the string representation of [batches]. The
      output has the form:
      "[{price_1, quantity_1, date_1}; ...; {price_n, quantity_n, date_n}]".
      Returns "[]" if [batches] is empty. *)

  val batches_of_string : string -> batches_element_data list
  (** [batches_of_string batches_string] converts [batches_string] to a concrete
      value. Requires: [batches_string] is a valid string representation of a
      [batches] value, as defined by [batches_to_string]. *)
end

module UserPortfolio : UserPortfolioType
(** A user's portfolio. *)

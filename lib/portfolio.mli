(** The signature of a user's portfolio. *)
module type PortfolioType = sig
  type 'a t
  (** Type representing the data in the portfolio. *)

  val empty_portfolio : 'a t
  (** A completely empty portfolio. *)

  (* NOTE: The stock name input is its ticker name. *)

  val contains_stock : 'a t -> string -> bool
  (** Check whether a stock is held in the portfolio. *)

  val quantity_stock : 'a t -> string -> int
  (** Returns the quantity of a stock held in the portfolio. *)

  (* TODO: Could make this function more general, and just have it display the
     price of some stock from a certain start date to the present date. *)
  val stock_price_over_time : 'a t -> string -> int list
  (** Returns the prices of a stock over time, specifically, from the most
      recent time when the stock went from being absent to present in the
      portfolio, to when this function is called. Returns the empty list if and
      only if the stock is not in the portfolio. *)

  val add_stock : 'a t -> string -> int -> 'a t
  (** Add a stock and the quantity purchased to the portfolio. The time
      associated with a stock addition to a portfolio is the time this function
      is called.

      Requires: quantity of stoack to be added >= 0.*)

  val remove_stock : 'a t -> string -> int -> 'a t
  (** Remove a certain quantity of a certain stock from the portfolio. If the
      stock is not in the portfolio, returns the unchanged portfolio. If the
      quantity of stock to be removed is equal to the initial quantity, the
      stock is completely removed from the portfolio. If the quantity of stock
      to be removed is greater than the initial quantity, raises [Failure].

      The time associated with a stock removal from a portfolio is the time this
      function is called.

      Requires: quantity of stock to be removed >= 0.*)

  (* TODO: Also display current portfolio's value. Need a function for that.*)
  val display_portfolio : 'a t -> string
  (** Returns a "pretty-printer" string to display the portfolio. For each stock
      in the portfolio, displays: stock name, quantity, current price, current
      total holding value, initial buy date (m/d/y format).*)

  val cost_basis : 'a t -> string -> int option
  (** The cost-basis of a certain stock held in a portfolio. Returns [None] if
      the stock is not in the portfolio. *)
end

module String_map : Map.S with type key = string
(** A Map whose keys are strings. *)

module UserPortfolio : PortfolioType
(** A user's portfolio. *)

(** The signature of a user's portfolio. *)
module type Portfolio = sig 

  (** Type representing the data in the portfolio. *)
  type 'a t 

  (** A completely empty portfolio. *)
  val empty_portfolio

  (* NOTE: The stock name input is its ticker name. *)

  (** Check whether a stock is held in the portfolio. *)
  val contains_stock : 'a t -> string -> bool

  (** Returns the quantity of a stock held in the portfolio. *)
  val quantity_stock : 'a t -> string -> int

  (** Returns the prices of a stock over time, specifically, from the most recent
      time when the stock went from being absent to present in the portfolio, to when this 
      function is called. *)
  
  (** Idea is to evenly split the interval between current time and when the stock was most recently added to portfolio
to get a series of dates (how many?), then ping the API to get the price at each of those dates and output the results in a list. 
      Returns the empty list only if the stock is not in the portfolio. *)

  val stock_price_over_time : 'a t -> string -> int list 

  (** Add a stock and the quantity purchased to the portfolio. *)
  val add_stock : 'a t -> string -> int -> 'a t 

  (** Remove a stock from the portfolio. *)
  val remove_stock : 'a t -> string -> 'a t

  (** Returns a "pretty-printer" string to display the portfolio. *)
  val display_portfolio : 'a t -> string
  
end
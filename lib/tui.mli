(** [Tui] has functions related to the terminal UI for the application*)

type transaction_step
type view_step
type screen
type dir
type stock_data

type state = {
  screen : screen;
  selected : int;
  portfolio : Portfolio.UserPortfolio.t;
  portfolio_display : string list;
  ticker : string;
  quantity : string;
  height : int;
  width : int;
  data : stock_data;
  cash : float;
}

val starting_cash : float
val non_display_lines : int

val sublist : 'a list -> int -> int -> 'a list
(** [sublist l i j] is a sub-list of list [l] from index [i] (inclusive) to
    index [j] (exclusive)*)

val calculate_cash : Portfolio.UserPortfolio.t -> float
(** [calculate_cash p] is the cash that the user has left based on the initial
    starting_cash and the buy/sell data from portfolio [p]*)

val load_or_empty_data : unit -> Portfolio.UserPortfolio.t
(** [load_or_empty_data ()] is the portfolio loaded from "data_dir/data.txt" or
    the empty portfolio if there is no data to load*)

val initial_state : state
(** [initial_state] represents the initial state of the loop when starting the
    application*)

val title : state -> Notty.image
(** [title st] is the Notty image of the title depending on the terminal's
    current width*)

val get_option_bindings : screen -> screen list
(** [get_option_bindings scr] is the list of navigation menus at the top of the
    terminal given the current screen [scr]*)

val render_image : state -> Notty.image
(** [render_image st] is a Notty image to display constructed from state [st]*)

val arrow_clicked : state -> dir -> state
(** [arrow_clicked st dir] is the updated state with a new selected menu option
    given direction [dir] *)

val enter_clicked : state -> state
(** [enter_clicked st] is the updated state after enter button is clicked *)

val character_clicked : state -> string -> state
(** [character_clicked st c] is the updated state after character [c] is clicked *)

val main_loop : Notty_unix.Term.t -> state -> unit
(** [main_loop t st] is the main loop of the application. Given the terminal IO
    abstraction [t] and initial state [st], waits for and then processes an
    interactive event *)

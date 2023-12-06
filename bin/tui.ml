(*I.(<|>) : puts one image after another I.(<->) : puts one image below another
  I.(</>) : puts one image on another.*)

open Notty
open Notty_unix
open Stocks
open Uchar
open Api
open Exceptions
module P = Portfolio.UserPortfolio

type step =
  | Ticker
  | Quantity
  | Success

type screen =
  | Main
  | Display
  | Buy of step
  | Sell of step
  | Error of string
  | Quit

type dir =
  | Left
  | Right

type state = {
  screen : screen;
  selected : int;
  portfolio : P.t;
  ticker : string;
  quantity : string;
}

let initial_state : state =
  {
    screen = Main;
    selected = 0;
    portfolio = P.empty_portfolio;
    ticker = "";
    quantity = "";
  }

(* [get_option_bindings scr] is the list of navigation menus at the top of the
   terminal given the current screen [scr]*)
let get_option_bindings scr =
  match scr with
  | Main -> [ Display; Buy Ticker; Sell Ticker; Quit ]
  | Display -> [ Main ]
  | Buy s -> (
      match s with
      | Ticker -> [ Buy Quantity; Main ]
      | Quantity -> [ Buy Success; Main ]
      | Success -> [ Main ])
  | Sell s -> (
      match s with
      | Ticker -> [ Sell Quantity; Main ]
      | Quantity -> [ Sell Success; Main ]
      | Success -> [ Main ])
  | Error _ -> [ Main ]
  | Quit -> []

(* [render_image st] is a Notty image to display constructed from state [st]*)
let render_image st =
  let create_option s u =
    I.(
      if u then string A.(fg white ++ st underline) s else string A.(fg white) s)
  in
  let rec display_list l =
    match l with
    | [] -> I.empty
    | h :: t -> I.(string A.(fg white) h <-> display_list t)
  in
  match st.screen with
  | Main ->
      I.(
        create_option "Display portfolio" (st.selected = 0)
        <|> void 2 0
        <|> create_option "Buy stock" (st.selected = 1)
        <|> void 2 0
        <|> create_option "Sell stock" (st.selected = 2)
        <|> void 2 0
        <|> create_option "Quit" (st.selected = 3))
  | Display ->
      I.(
        create_option "Main menu" (st.selected = 0)
        <-> void 0 1
        <-> (st.portfolio |> P.display_portfolio |> display_list))
  | Buy Ticker | Sell Ticker ->
      I.(
        create_option "Select ticker" (st.selected = 0)
        <|> void 2 0
        <|> create_option "Main menu" (st.selected = 1)
        <-> void 0 1
        <-> string A.(fg white) "Enter the ticker:"
        <-> (string A.(fg red) ">" <|> string A.(fg white) st.ticker))
  | Buy Quantity | Sell Quantity ->
      I.(
        create_option "Select quantity" (st.selected = 0)
        <|> void 2 0
        <|> create_option "Main menu" (st.selected = 1)
        <-> void 0 1
        <-> string A.(fg green) st.ticker
        <-> string A.(fg white) "Enter the quantity:"
        <-> (string A.(fg red) ">" <|> string A.(fg white) st.quantity))
  | Buy Success ->
      I.(
        create_option "Main menu" (st.selected = 0)
        <-> void 0 1
        <-> string A.(fg white) "Congratulations, stock has been bought."
        <-> (st.portfolio |> P.display_portfolio |> display_list))
  | Sell Success ->
      I.(
        create_option "Main menu" (st.selected = 0)
        <-> void 0 1
        <-> string A.(fg white) "Congratulations, stock has been sold."
        <-> (st.portfolio |> P.display_portfolio |> display_list))
  | Error e ->
      I.(
        create_option "Main menu" (st.selected = 0)
        <-> void 0 1
        <-> string A.(fg red) "ERROR"
        <-> string A.(fg white) e)
  | Quit -> I.empty

(* [arrow_clicked st dir] is the updated state with a new selected menu option
   given direction [dir] *)
let arrow_clicked st dir =
  let max = (st.screen |> get_option_bindings |> List.length) - 1 in
  let sel =
    match dir with
    | Left -> if st.selected = 0 then 0 else st.selected - 1
    | Right -> if st.selected = max then max else st.selected + 1
  in
  { st with selected = sel }

(* [enter_clicked st] is the updated state after enter button is clicked *)
let enter_clicked st =
  match List.nth (get_option_bindings st.screen) st.selected with
  | Buy Success ->
      let error = ref "" in
      let updated_portfolio =
        try P.add_stock st.portfolio st.ticker (int_of_string st.quantity) with
        | NoResultsFound ->
            error := st.ticker ^ " was not found";
            st.portfolio
        | InvalidJSONFormat ->
            error :=
              "There was an issue with the API (Invalid JSON format error)";
            st.portfolio
        | e ->
            error := "There was an unresolved error: " ^ Printexc.to_string e;
            st.portfolio
      in
      {
        screen =
          (if st.portfolio = updated_portfolio then Error !error
           else Buy Success);
        selected = 0;
        portfolio = updated_portfolio;
        ticker = "";
        quantity = "";
      }
  | Sell Success ->
      let error = ref "" in
      let updated_portfolio =
        try
          P.remove_stock st.portfolio st.ticker (int_of_string st.quantity)
        with
        | NoResultsFound ->
            error := st.ticker ^ " was not found";
            st.portfolio
        | InvalidJSONFormat ->
            error :=
              "There was an issue with the API (Invalid JSON format error)";
            st.portfolio
        | e ->
            error := "There was an unresolved error: " ^ Printexc.to_string e;
            st.portfolio
      in
      {
        screen =
          (if st.portfolio = updated_portfolio then Error !error
           else Sell Success);
        selected = 0;
        portfolio = updated_portfolio;
        ticker = "";
        quantity = "";
      }
  | to_scr -> { st with screen = to_scr; selected = 0 }

(* [character_clicked st c] is the updated state after character [c] is
   clicked *)
let character_clicked st c =
  match st with
  | { screen = Buy Ticker; ticker } | { screen = Sell Ticker; ticker } ->
      {
        st with
        ticker =
          (if c = "back" then String.sub ticker 0 (String.length ticker - 1)
           else ticker ^ c);
      }
  | { screen = Buy Quantity; quantity } | { screen = Sell Quantity; quantity }
    ->
      {
        st with
        quantity =
          (if c = "back" then String.sub quantity 0 (String.length quantity - 1)
           else quantity ^ c);
      }
  | _ -> st

(* [main_loop t st] is the main loop of the application. Given the terminal IO
   abstraction [t] and initial state [st], waits for and then processes an
   interactive event *)
let rec main_loop (t : Term.t) (st : state) =
  Term.image t (render_image st);
  match Term.event t with
  | `End | `Key (`Escape, []) -> ()
  | `Key (`Arrow `Left, []) | `Key (`Arrow `Up, []) ->
      main_loop t (arrow_clicked st Left)
  | `Key (`Arrow `Right, []) | `Key (`Arrow `Down, []) ->
      main_loop t (arrow_clicked st Right)
  | `Key (`Enter, []) | `Key (`ASCII ' ', []) -> (
      match enter_clicked st with
      | { screen = Quit; selected = _; portfolio = _ } -> ()
      | new_st -> main_loop t new_st)
  | `Key (`ASCII c, []) -> main_loop t (character_clicked st (Char.escaped c))
  | `Key (`Backspace, []) -> main_loop t (character_clicked st "back")
  | _ -> main_loop t st

let () =
  let t = Term.create () in
  main_loop t initial_state;
  Term.release t

(* let () = Api.print_json_results "MSFT" () *)

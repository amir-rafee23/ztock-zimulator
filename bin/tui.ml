(*I.(<|>) : puts one image after another I.(<->) : puts one image below another
  I.(</>) : puts one image on another.*)

open Notty
open Notty_unix
open Stocks
open Uchar
open Api
open Exceptions
module F = Filesys.FileSys
module P = Portfolio.UserPortfolio

type step =
  | Ticker
  | Quantity
  | Success

type screen =
  | Main
  | Display
  | DisplayPaginate of int
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
  portfolio_display : string list;
  ticker : string;
  quantity : string;
  height : int;
  width : int;
}

(* [sublist l i j] is a sub-list of list [l] from index [i] (inclusive) to index
   [j] (exclusive)*)
let sublist l i j =
  let rec take l n =
    if n = 0 then []
    else
      match l with
      | [] -> []
      | h :: t -> h :: take t (n - 1)
  in
  let rec drop l n =
    if n = 0 then l
    else
      match l with
      | [] -> []
      | h :: t -> drop t (n - 1)
  in
  take (drop l i) (j - i)

let load_or_empty_data () =
  try F.to_user_portfolio "data_dir/data.txt"
  with Sys_error _ -> P.empty_portfolio

let initial_state : state =
  let p = load_or_empty_data () in
  {
    screen = Main;
    selected = 0;
    portfolio = p;
    portfolio_display = P.display_portfolio p;
    ticker = "";
    quantity = "";
    height = 0;
    width = 0;
  }

let title st =
  I.(
    void 0 3
    <-> (void ((st.width / 2) - 9) 0
        <|> string A.(fg blue) "ZAZ STOCK SIMULATOR")
    </>
    let line =
      char A.(fg black ++ bg blue) '|' 1 1
      <|> char A.(fg black ++ bg blue) '-' (st.width - 2) 1
      <|> char A.(fg black ++ bg blue) '|' 1 1
    in
    line
    <-> (char A.(fg black ++ bg blue) '|' 1 5
        <|> void (st.width - 2) 0
        <|> char A.(fg black ++ bg blue) '|' 1 5)
    <-> line <-> void 0 1)

(* [get_option_bindings scr] is the list of navigation menus at the top of the
   terminal given the current screen [scr]*)
let get_option_bindings scr =
  match scr with
  | Main -> [ Display; Buy Ticker; Sell Ticker; Quit ]
  | Display -> [ Main ]
  | DisplayPaginate i ->
      [ DisplayPaginate (i - 1); DisplayPaginate (i + 1); Main ]
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

(* let display_portfolio st = let to_display = st.portfolio |>
   P.display_portfolio in let lines = st.height - 10 in if lines >= List.length
   to_display then display_list to_display (st.height - 10) else I.empty *)

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
  I.(
    title st
    <->
    match st.screen with
    | Main ->
        create_option "Display portfolio" (st.selected = 0)
        <|> void 2 0
        <|> create_option "Buy stock" (st.selected = 1)
        <|> void 2 0
        <|> create_option "Sell stock" (st.selected = 2)
        <|> void 2 0
        <|> create_option "Quit" (st.selected = 3)
    | Display ->
        create_option "Main menu" (st.selected = 0)
        <-> void 0 1
        <-> display_list (P.display_portfolio st.portfolio)
    | DisplayPaginate i ->
        let lines = st.height - 12 in
        let pages = (List.length st.portfolio_display / lines) + 1 in
        let paginated_display =
          sublist st.portfolio_display
            ((i - 1) * lines)
            (((i - 1) * lines) + lines)
        in
        create_option "Back" (st.selected = 0)
        <|> void 2 0
        <|> create_option "Next" (st.selected = 1)
        <|> void 2 0
        <|> create_option "Main menu" (st.selected = 2)
        <-> void 0 1
        <-> display_list paginated_display
        <-> void 0 1
        <-> string
              A.(fg white)
              ("<Page " ^ (i |> string_of_int) ^ "/" ^ (pages |> string_of_int)
             ^ ">")
    | Buy Ticker | Sell Ticker ->
        create_option "Select ticker" (st.selected = 0)
        <|> void 2 0
        <|> create_option "Main menu" (st.selected = 1)
        <-> void 0 1
        <-> string A.(fg white) "Enter the ticker:"
        <-> (string A.(fg red) ">" <|> string A.(fg white) st.ticker)
    | Buy Quantity | Sell Quantity ->
        create_option "Select quantity" (st.selected = 0)
        <|> void 2 0
        <|> create_option "Main menu" (st.selected = 1)
        <-> void 0 1
        <-> string A.(fg green) st.ticker
        <-> string A.(fg white) "Enter the quantity:"
        <-> (string A.(fg red) ">" <|> string A.(fg white) st.quantity)
    | Buy Success ->
        create_option "Main menu" (st.selected = 0)
        <-> void 0 1
        <-> string A.(fg white) "Congratulations, stock has been bought."
        (* <-> display_list (st.portfolio |> P.display_portfolio) (st.height -
           11) *)
    | Sell Success ->
        create_option "Main menu" (st.selected = 0)
        <-> void 0 1
        <-> string A.(fg white) "Congratulations, stock has been sold."
        (* <-> display_list (st.portfolio |> P.display_portfolio) (st.height -
           11) *)
    | Error e ->
        create_option "Main menu" (st.selected = 0)
        <-> void 0 1
        <-> string A.(fg red) "ERROR"
        <-> string A.(fg white) e
    | Quit -> empty)

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
      let int_quantity =
        match int_of_string_opt st.quantity with
        | Some i ->
            if i > 0 then i
            else (
              error := "Quantity must be greater than or equal to 0!";
              -1)
        | None ->
            error := "Quantity must be an integer!";
            -1
      in
      let updated_portfolio =
        if !error = "" then (
          try P.add_stock st.portfolio st.ticker int_quantity with
          | NoResultsFound ->
              error := st.ticker ^ " was not found!";
              st.portfolio
          | InvalidJSONFormat ->
              error :=
                "There was an issue with the API (Invalid JSON format error)";
              st.portfolio
          | e ->
              error := "There was an unresolved error: " ^ Printexc.to_string e;
              st.portfolio)
        else st.portfolio
      in
      {
        st with
        screen =
          (if st.portfolio = updated_portfolio then Error !error
           else Buy Success);
        selected = 0;
        portfolio = updated_portfolio;
        portfolio_display = P.display_portfolio updated_portfolio;
        ticker = "";
        quantity = "";
      }
  | Sell Success ->
      let error = ref "" in
      let int_quantity =
        match int_of_string_opt st.quantity with
        | Some i ->
            if i > 0 then i
            else (
              error := "Quantity must be greater than or equal to 0!";
              -1)
        | None ->
            error := "Quantity must be an integer!";
            -1
      in
      let updated_portfolio =
        if !error = "" then (
          try P.remove_stock st.portfolio st.ticker int_quantity with
          | TickerNotHeld ->
              error := st.ticker ^ " is not in your portfolio!";
              st.portfolio
          | ExceededQuantity ->
              error := "You cannot sell more shares than you own!";
              st.portfolio
          | NoResultsFound ->
              error := st.ticker ^ " was not found!";
              st.portfolio
          | InvalidJSONFormat ->
              error :=
                "There was an issue with the API (Invalid JSON format error)";
              st.portfolio
          | e ->
              error := "There was an unresolved error: " ^ Printexc.to_string e;
              st.portfolio)
        else st.portfolio
      in
      {
        st with
        screen =
          (if st.portfolio = updated_portfolio then Error !error
           else Sell Success);
        selected = 0;
        portfolio = updated_portfolio;
        portfolio_display = P.display_portfolio updated_portfolio;
        ticker = "";
        quantity = "";
      }
  | Display ->
      if st.portfolio |> P.display_portfolio |> List.length <= st.height - 10
      then { st with screen = Display; selected = 0 }
      else { st with screen = DisplayPaginate 1; selected = 0 }
  | DisplayPaginate i ->
      let pages = (List.length st.portfolio_display / (st.height - 11)) + 1 in
      {
        st with
        screen =
          (if i < 1 then DisplayPaginate 1
           else if i > pages then DisplayPaginate pages
           else DisplayPaginate i);
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
  | `Resize (width, height) -> main_loop t { st with width; height }
  | `End | `Key (`Escape, []) ->
      F.update_file "data_dir/data.txt" st.portfolio |> ignore
  | `Key (`Arrow `Left, []) | `Key (`Arrow `Up, []) ->
      main_loop t (arrow_clicked st Left)
  | `Key (`Arrow `Right, []) | `Key (`Arrow `Down, []) ->
      main_loop t (arrow_clicked st Right)
  | `Key (`Enter, []) | `Key (`ASCII ' ', []) -> (
      match enter_clicked st with
      | { screen = Quit; selected = _; portfolio = _ } ->
          F.update_file "data_dir/data.txt" st.portfolio |> ignore
      | new_st -> main_loop t new_st)
  | `Key (`ASCII c, []) -> main_loop t (character_clicked st (Char.escaped c))
  | `Key (`Backspace, []) -> main_loop t (character_clicked st "back")
  | _ -> main_loop t st

let () =
  let t = Term.create () in
  let size = Term.size t in
  main_loop t { initial_state with height = size |> snd; width = size |> fst };
  Term.release t

(* let () = Api.print_json_results "MSFT" () *)

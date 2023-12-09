open Notty
open Notty_unix
open Uchar
open Api
open Exception
module F = Filesys.FileSys
module P = Portfolio.UserPortfolio

type transaction_step =
  | Ticker
  | Quantity
  | Success

type view_step =
  | Ticker
  | Success

type screen =
  | Main
  | Display
  | DisplayPaginate of int
  | View of view_step
  | Buy of transaction_step
  | Sell of transaction_step
  | Error of string
  | Reset of bool
  | Quit

type dir =
  | Left
  | Right

type stock_data = { price : float }

type state = {
  screen : screen;
  selected : int;
  portfolio : P.t;
  portfolio_display : string list;
  ticker : string;
  quantity : string;
  height : int;
  width : int;
  data : stock_data;
  cash : float;
}

let starting_cash = 10000.
let non_display_lines = 9

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

let calculate_cash p =
  let add_values b (d : P.batches_element_data) =
    b +. (d.price *. float_of_int d.quantity)
  in
  Portfolio.String_map.fold
    (fun _ (data : P.stock_data) (current_cash : float) ->
      current_cash
      -. List.fold_left add_values 0. data.buy_batches
      +. List.fold_left add_values 0. data.sell_batches)
    p starting_cash

let load_or_empty_data () =
  try F.to_user_portfolio "data_dir/data.txt"
  with Sys_error _ -> P.empty_portfolio

let initial_state =
  let port = load_or_empty_data () in
  let cash = calculate_cash port in
  {
    screen = Main;
    selected = 0;
    portfolio = port;
    portfolio_display =
      ("Cash: $" ^ string_of_float cash) :: P.display_portfolio port;
    ticker = "";
    quantity = "";
    height = 0;
    width = 0;
    data = { price = 0. };
    cash;
  }

let title st =
  I.(
    void 0 2
    <-> (void ((st.width / 2) - 10) 0
        <|> string A.(fg blue) "Zazzy Ztock Zimulator"
        <-> (void ((st.width / 2) - 18) 0
            <|> string A.(fg white) "Arrows to navigate | Enter to select"))
    </>
    let line =
      char A.(fg black ++ bg blue) '|' 1 1
      <|> char A.(fg black ++ bg blue) '-' (st.width - 2) 1
      <|> char A.(fg black ++ bg blue) '|' 1 1
    in
    line
    <-> (char A.(fg black ++ bg blue) '|' 1 4
        <|> void (st.width - 2) 0
        <|> char A.(fg black ++ bg blue) '|' 1 4)
    <-> line <-> void 0 1)

let get_option_bindings scr =
  match scr with
  | Main -> [ Display; View Ticker; Buy Ticker; Sell Ticker; Reset false; Quit ]
  | Display -> [ Main ]
  | DisplayPaginate i ->
      [ DisplayPaginate (i - 1); DisplayPaginate (i + 1); Main ]
  | View s -> (
      match s with
      | Ticker -> [ View Success; Main ]
      | Success -> [ Buy Quantity; View Ticker; Main ])
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
  | Reset b -> if b then [ Main ] else [ Main; Reset true ]
  | Quit -> []

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
    match st.screen with
    | Main ->
        title st
        <-> (create_option "Display portfolio" (st.selected = 0)
            <|> void 2 0
            <|> create_option "View stock" (st.selected = 1)
            <|> void 2 0
            <|> create_option "Buy stock" (st.selected = 2)
            <|> void 2 0
            <|> create_option "Sell stock" (st.selected = 3)
            <|> void 2 0
            <|> create_option "Reset portfolio" (st.selected = 4)
            <|> void 2 0
            <|> create_option "Quit" (st.selected = 5))
    | Display ->
        title st
        <-> (create_option "Main menu" (st.selected = 0)
            <-> void 0 1
            <-> display_list st.portfolio_display)
    | DisplayPaginate i ->
        let lines = st.height - (non_display_lines + 2) in
        let pages = (List.length st.portfolio_display / lines) + 1 in
        let paginated_display =
          sublist st.portfolio_display
            ((i - 1) * lines)
            (((i - 1) * lines) + lines)
        in
        title st
        <-> (create_option "Back" (st.selected = 0)
            <|> void 2 0
            <|> create_option "Next" (st.selected = 1)
            <|> void 2 0
            <|> create_option "Main menu" (st.selected = 2)
            <-> void 0 1
            <-> display_list paginated_display
            <-> void 0 1
            <-> string
                  A.(fg white)
                  ("<Page " ^ (i |> string_of_int) ^ "/"
                 ^ (pages |> string_of_int) ^ ">"))
    | View Ticker ->
        title st
        <-> (create_option "View stock" (st.selected = 0)
            <|> void 2 0
            <|> create_option "Main menu" (st.selected = 1)
            <-> void 0 1
            <-> string A.(fg white) "Enter the ticker:"
            <-> (string A.(fg red) ">" <|> string A.(fg white) st.ticker))
    | Buy Ticker | Sell Ticker ->
        title st
        <-> (create_option "Select ticker" (st.selected = 0)
            <|> void 2 0
            <|> create_option "Main menu" (st.selected = 1)
            <-> void 0 1
            <-> string A.(fg white) "Enter the ticker:"
            <-> (string A.(fg red) ">" <|> string A.(fg white) st.ticker))
    | Buy Quantity | Sell Quantity ->
        title st
        <-> (create_option "Select quantity" (st.selected = 0)
            <|> void 2 0
            <|> create_option "Main menu" (st.selected = 1)
            <-> void 0 1
            <-> string A.(fg green) st.ticker
            <-> string A.(fg white) "Enter the quantity:"
            <-> (string A.(fg red) ">" <|> string A.(fg white) st.quantity))
    | Buy Success ->
        title st
        <-> (create_option "Main menu" (st.selected = 0)
            <-> void 0 1
            <-> string A.(fg white) "Congratulations, stock has been bought.")
    | Sell Success ->
        title st
        <-> (create_option "Main menu" (st.selected = 0)
            <-> void 0 1
            <-> string A.(fg white) "Congratulations, stock has been sold.")
    | View Success ->
        title st
        <-> (create_option "Buy stock" (st.selected = 0)
            <|> void 2 0
            <|> create_option "Select another ticker" (st.selected = 1)
            <|> void 2 0
            <|> create_option "Main menu" (st.selected = 2)
            <-> (void 0 1
                <-> string A.(fg white) ("Ticker: " ^ st.ticker)
                <-> string
                      A.(fg white)
                      ("Price: $" ^ string_of_float st.data.price)))
    | Error e ->
        title st
        <-> (create_option "Main menu" (st.selected = 0)
            <-> void 0 1
            <-> string A.(fg red) "ERROR"
            <-> string A.(fg white) e)
    | Reset false ->
        title st
        <-> (create_option "Main menu" (st.selected = 0)
            <|> void 2 0
            <|> create_option "Confirm Reset" (st.selected = 1)
            <-> void 0 1
            <-> string
                  A.(fg red)
                  "Are you sure you want to completely reset your portfolio?")
    | Reset true ->
        title st
        <-> (create_option "Main menu" (st.selected = 0)
            <-> void 0 1
            <-> string A.(fg green) "Portfolio has been fully reset!")
    | Quit -> empty)

let arrow_clicked st dir =
  let max = (st.screen |> get_option_bindings |> List.length) - 1 in
  let sel =
    match dir with
    | Left -> if st.selected = 0 then 0 else st.selected - 1
    | Right -> if st.selected = max then max else st.selected + 1
  in
  { st with selected = sel }

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
      let cash = calculate_cash updated_portfolio in

      if cash >= 0. then
        {
          st with
          screen =
            (if st.portfolio = updated_portfolio then Error !error
             else Buy Success);
          selected = 0;
          portfolio = updated_portfolio;
          portfolio_display =
            ("Cash: $" ^ string_of_float cash)
            :: P.display_portfolio updated_portfolio;
          cash;
          ticker = "";
          quantity = "";
        }
      else
        {
          st with
          screen = Error "You do not have enough cash!";
          selected = 0;
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
      let cash = calculate_cash updated_portfolio in
      {
        st with
        screen =
          (if st.portfolio = updated_portfolio then Error !error
           else Sell Success);
        selected = 0;
        portfolio = updated_portfolio;
        portfolio_display =
          ("Cash: $" ^ string_of_float cash)
          :: P.display_portfolio updated_portfolio;
        cash;
        ticker = "";
        quantity = "";
      }
  | Display ->
      if st.portfolio_display |> List.length <= st.height - non_display_lines
      then { st with screen = Display; selected = 0 }
      else { st with screen = DisplayPaginate 1; selected = 0 }
  | DisplayPaginate i ->
      let pages =
        List.length st.portfolio_display
        / (st.height - (non_display_lines + 1))
        + 1
      in
      {
        st with
        screen =
          (if i < 1 then DisplayPaginate 1
           else if i > pages then DisplayPaginate pages
           else DisplayPaginate i);
      }
  | Main -> { st with screen = Main; selected = 0; ticker = "" }
  | View Ticker -> { st with screen = View Ticker; selected = 0; ticker = "" }
  | View Success ->
      let price = try Api.get_price st.ticker with NoResultsFound -> -1. in
      if price = -1. then
        { st with screen = Error (st.ticker ^ " was not found!"); selected = 0 }
      else { st with screen = View Success; selected = 0; data = { price } }
  | Reset true ->
      F.update_file "data_dir/data.txt" P.empty_portfolio |> ignore;
      {
        st with
        screen = Reset true;
        selected = 0;
        portfolio = P.empty_portfolio;
        portfolio_display =
          ("Cash: $" ^ string_of_float starting_cash)
          :: P.display_portfolio P.empty_portfolio;
        cash = starting_cash;
      }
  | to_scr -> { st with screen = to_scr; selected = 0 }

let character_clicked st c =
  match st with
  | { screen = Buy Ticker; ticker }
  | { screen = Sell Ticker; ticker }
  | { screen = View Ticker; ticker } ->
      {
        st with
        ticker =
          (if c = "back" && String.length ticker >= 0 then
             String.sub ticker 0 (String.length ticker - 1)
           else ticker ^ String.uppercase_ascii c);
      }
  | { screen = Buy Quantity; quantity } | { screen = Sell Quantity; quantity }
    ->
      {
        st with
        quantity =
          (if c = "back" && String.length quantity >= 0 then
             String.sub quantity 0 (String.length quantity - 1)
           else quantity ^ String.uppercase_ascii c);
      }
  | _ -> st

let rec main_loop t st =
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

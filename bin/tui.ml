open Notty
open Notty_unix

(*I.(<|>) : puts one image after another I.(<->) : puts one image below another
  I.(</>) : puts one image on another.*)

(* let square = "\xe2\x96\xaa"

   let rec sierp n = if n > 1 then let ss = sierp (pred n) in I.(ss <-> (ss <|>
   ss)) else I.(string A.(fg magenta) square |> hpad 1 0)

   let img (double, n) = let s = sierp n in if double then I.(s </> vpad 1 0 s)
   else s

   let rec update t state = Term.image t (img state); loop t state

   and loop t ((double, n) as state) = match Term.event t with | `Key (`Enter,
   _) -> () | `Key (`Arrow `Left, _) -> update t (double, max 1 (n - 1)) | `Key
   (`Arrow `Right, _) -> update t (double, min 8 (n + 1)) | `Key (`ASCII ' ', _)
   -> update t (not double, n) | `Resize _ -> update t state | _ -> loop t state

   let t = Term.create ()

   let () = update t (false, 1); Term.release t *)
open Notty
open Notty_unix
open Stocks
open Uchar
module P = Portfolio.UserPortfolio

type step =
  | Ticker
  | Quantity
  | Success

type screen =
  | Main
  | Display
  | Buy of step
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

let inital_state : state =
  {
    screen = Main;
    selected = 0;
    portfolio = P.empty_portfolio;
    ticker = "";
    quantity = "";
  }

let get_max_options scr =
  match scr with
  | Main -> 2
  | Display -> 0
  | Buy Ticker -> 1
  | Buy Quantity -> 1
  | Buy Success -> 0
  | Quit -> -1

let get_option_bindings scr =
  match scr with
  | Main -> [ Display; Buy Ticker; Quit ]
  | Display -> [ Main ]
  | Buy s -> (
      match s with
      | Ticker -> [ Buy Quantity; Main ]
      | Quantity -> [ Buy Success; Main ]
      | Success -> [ Main ])
  | Quit -> []

let render_screen st =
  let create_option s u =
    I.(
      if u then string A.(fg white ++ st underline) s else string A.(fg white) s)
  in
  match st.screen with
  | Main ->
      I.(
        create_option "Display portfolio" (st.selected = 0)
        <|> void 2 0
        <|> create_option "Add stock" (st.selected = 1)
        <|> void 2 0
        <|> create_option "Quit" (st.selected = 2))
  | Display ->
      I.(
        create_option "Main menu" (st.selected = 0)
        <-> void 0 1
        <-> string A.(fg white) (P.display_portfolio st.portfolio))
  | Buy Ticker ->
      I.(
        create_option "Select ticker" (st.selected = 0)
        <|> void 2 0
        <|> create_option "Main menu" (st.selected = 1)
        <-> void 0 1
        <-> string A.(fg white) "Enter the ticker:"
        <-> (string A.(fg red) ">" <|> string A.(fg white) st.ticker))
  | Buy Quantity ->
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
        <-> string A.(fg white) "P.display_portfolio st.portfolio")
  | Quit -> I.empty

let arrow_clicked st dir =
  let max = get_max_options st.screen in
  let sel =
    match dir with
    | Left -> if st.selected = 0 then 0 else st.selected - 1
    | Right -> if st.selected = max then max else st.selected + 1
  in
  { st with selected = sel }

let enter_clicked st =
  match List.nth (get_option_bindings st.screen) st.selected with
  | Buy Success ->
      let updated_portfolio =
        P.add_stock st.portfolio st.ticker (int_of_string st.quantity)
      in
      {
        screen = Buy Success;
        selected = 0;
        portfolio = updated_portfolio;
        ticker = "";
        quantity = "";
      }
  | to_scr -> { st with screen = to_scr; selected = 0 }

let character_clicked st c =
  match st with
  | { screen = Buy Ticker; ticker } ->
      {
        st with
        ticker =
          (if c = "back" then String.sub ticker 0 (String.length ticker - 1)
           else ticker ^ c);
      }
  | { screen = Buy Quantity; quantity } ->
      {
        st with
        quantity =
          (if c = "back" then String.sub quantity 0 (String.length quantity - 1)
           else quantity ^ c);
      }
  | _ -> st

let rec main_loop (t : Term.t) (st : state) =
  Term.image t (render_screen st);
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
  main_loop t inital_state;
  Term.release t

open Stocks
module P = Portfolio.UserPortfolio

let portfolio = ref P.empty_portfolio
let view_portfolio () = print_endline (P.display_portfolio !portfolio)

let get_ticker () =
  print_endline "Enter the stock ticker:";
  print_string "> ";
  read_line ()

let get_quantity () =
  print_endline "Enter the quantity:";
  print_string "> ";
  read_line ()

let buy_stock () =
  let ticker = get_ticker () in
  let quantity = get_quantity () in
  portfolio := P.add_stock !portfolio ticker (int_of_string quantity)

let sell_stock () =
  let ticker = get_ticker () in
  let quantity = get_quantity () in
  portfolio := P.remove_stock !portfolio ticker (int_of_string quantity)

let rec get_command () =
  print_endline "What would you like to do?";
  print_endline
    "| (1) View portfolio \n\
     | (2) View stock \n\
     | (3) Buy stock \n\
     | (4) Sell stock";
  print_string "> ";
  match read_line () with
  | "1" ->
      print_endline "~~ PORTFOLIO ~~";
      view_portfolio ()
  | "2" -> print_endline "~~ View stock ~~"
  (* get_mul () *)
  | "3" ->
      print_endline "~~ Buy stock ~~";
      buy_stock ()
  | "4" ->
      print_endline "~~ Sell stock ~~";
      sell_stock ()
  | _ ->
      print_endline "Not a valid command, please try again";
      get_command ()

let main () =
  print_endline "\nWelcome to the Zaz Stock Simulator";
  for i = 1 to 5 do
    get_command ()
  done

let () = main ()

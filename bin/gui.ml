open Bogue
open Stocks
module W = Widget
module L = Layout
module P = Portfolio.UserPortfolio

let width = 800
let height = 600

let border =
  let open Style in
  mk_line ~color:Draw.(opaque black) ~width:1 ~style:Solid ()
  |> mk_border |> of_border |> L.style_bg

let user_portfolio = ref P.empty_portfolio

let get_main _ =
  let portfolio_print = W.text_display (P.display_portfolio !user_portfolio) in
  let ticker = W.text_input ~prompt:"Stock ticker" ~max_size:5 () in

  let display_ticker = W.text_display "No ticker selected" in
  let shares_input = W.text_input ~prompt:"# of shares" ~max_size:5 () in
  let buy_btn =
    W.button
      ~action:(fun _ ->
        let text = W.get_text ticker in
        if String.length text > 0 then
          user_portfolio :=
            P.add_stock !user_portfolio text
              (W.get_text shares_input |> int_of_string);
        W.set_text portfolio_print (P.display_portfolio !user_portfolio))
      "Buy"
  in
  let sell_btn =
    W.button
      ~action:(fun _ ->
        let text = W.get_text ticker in
        if String.length text > 0 then
          user_portfolio :=
            P.remove_stock !user_portfolio (W.get_text ticker)
              (W.get_text shares_input |> int_of_string);
        W.set_text portfolio_print (P.display_portfolio !user_portfolio))
      "Sell"
  in
  let controls =
    L.flat
      [
        L.resident ~background:border shares_input;
        L.resident ~w:50 buy_btn;
        L.resident ~w:50 sell_btn;
      ]
  in

  let title =
    W.text_display ~w:(width - 200) ~h:50 "Stock Simulation" |> L.resident
  in

  let search =
    W.button
      ~action:(fun _ ->
        W.set_text display_ticker ("Ticker: " ^ W.get_text ticker))
      "Search"
  in
  L.tower
    [
      title;
      L.resident ~background:border ticker;
      L.resident search;
      L.resident display_ticker;
      controls;
      L.resident portfolio_print;
    ]

let area = L.tower [ get_main () ]
let board = [ area ] |> L.superpose ~w:width ~h:height |> Bogue.of_layout
let () = Bogue.run board

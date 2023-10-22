open Bogue
module W = Widget
module L = Layout

let width = 800
let height = 600

let get_main _ =
  let title = W.text_display ~w:(width - 200) ~h:50 "Stock Simulation" in
  let text = W.text_input ~prompt:"Stock ticker" ~max_size:5 () in
  let search = W.button "Search" in
  L.tower_of_w ~w:width [ title; text; search ]

let get_add_sell _ =
  let shares_input =
    W.text_input ~prompt:"# of shares" ~max_size:5 () |> L.resident
  in
  let buy_btn = W.button "Buy" |> L.resident ~w:50 in
  L.flat [ shares_input; buy_btn ]

let area = L.tower [ get_main (); get_add_sell () ]
let board = [ area ] |> L.superpose ~w:width ~h:height |> Bogue.of_layout
let () = Bogue.run board

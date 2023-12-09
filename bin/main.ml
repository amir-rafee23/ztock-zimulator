open Notty_unix
open Stocks

let () =
  let t = Term.create () in
  let size = Term.size t in
  Tui.main_loop t
    { Tui.initial_state with height = size |> snd; width = size |> fst };
  Term.release t

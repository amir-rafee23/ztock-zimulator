(* Example from discussion page *)

open Bogue
module W = Widget
module L = Layout

let n = 15 (* number of discs *)
let radius = 20
let width = 800
let height = 600
let c = Draw.find_color "#e5b92c"
let cb = Draw.find_color "#7b6b35"

let disc_style =
  Style.(
    create
      ~border:
        (mk_border ~radius
           (mk_line ~color:Draw.(opaque c) ~width:1 ~style:Solid ()))
      ~background:(color_bg Draw.(opaque cb))
      ())

let background =
  L.style_bg
    Style.(of_bg (gradient ~angle:45. Draw.[ opaque grey; opaque black ]))

let fg = Draw.(opaque white)

let create_disc i (x, y) =
  let w = (2 * radius) + 1 in
  W.label ~fg (string_of_int i)
  |> L.resident ~background:(L.style_bg disc_style) ~x:(x - radius)
       ~y:(y - radius) ~w ~h:w

let move_disc (x, y) d =
  let x0, y0 = (L.xpos d, L.ypos d) in
  L.animate_x d (Avar.fromto x0 x);
  L.animate_y d (Avar.fromto y0 y)

let random_center _ =
  ( radius + Random.int (width - (2 * radius)),
    radius + Random.int (height - (2 * radius)) )

let area =
  let sdlw = W.sdl_area ~w:width ~h:height () in
  let sdla = W.get_sdl_area sdlw in
  let centers = Array.init n random_center in
  let color = Draw.(opaque grey) in
  let draw_lines renderer =
    let open Draw in
    for i = 0 to n - 2 do
      let x0, y0 = to_pixels centers.(i) in
      let x1, y1 = to_pixels centers.(i + 1) in
      line renderer ~color ~thick:6 ~x0 ~y0 ~x1 ~y1
    done
  in
  Sdl_area.add sdla draw_lines;
  let discs = Array.mapi create_disc centers |> Array.to_list in
  (* move the disc when click on it *)
  List.iteri
    (fun i d ->
      W.on_click
        ~click:(fun _ ->
          centers.(i) <- random_center 0;
          Sdl_area.update sdla;
          let x, y = centers.(i) in
          move_disc (x - radius, y - radius) d)
        (L.widget d))
    discs;
  L.superpose ~w:width ~h:height ~background (L.resident sdlw :: discs)

let board = Bogue.of_layout area
let () = Bogue.run board

open Bogue
module W = Widget
module L = Layout

let width = 800
let height = 600
let radius = 5
let c = Draw.find_color "#e5b92c"
let cb = Draw.find_color "#7b6b35"
let fg = Draw.(opaque white)

(* let background = L.style_bg Style.(of_bg (gradient ~angle:45. Draw.[ opaque
   grey; opaque black ])) *)

let disc_style =
  Style.(
    create
      ~border:
        (mk_border ~radius
           (mk_line ~color:Draw.(opaque c) ~width:1 ~style:Solid ()))
      ~background:(color_bg Draw.(opaque cb))
      ())

let create_disc (x, y) =
  let w = (2 * radius) + 1 in
  W.label ~fg ""
  |> L.resident ~background:(L.style_bg disc_style) ~x:(x - radius)
       ~y:(y - radius) ~w ~h:w

let get_point_coords l = l

let area =
  let sdlw = W.sdl_area ~w:width ~h:height () in
  let sdla = W.get_sdl_area sdlw in
  let centers = [ (100, 500); (500, 100) ] |> get_point_coords in
  let color = Draw.(opaque black) in
  let draw_lines renderer =
    Draw.line renderer ~color ~thick:3 ~x0:10 ~y0:10 ~x1:10 ~y1:590;
    Draw.line renderer ~color ~thick:3 ~x0:10 ~y0:590 ~x1:790 ~y1:590
    (* for i = 0 to n - 2 do let x0, y0 = to_pixels centers.(i) in let x1, y1 =
       to_pixels centers.(i + 1) in line renderer ~color ~thick:6 ~x0 ~y0 ~x1
       ~y1 done *)
  in
  Sdl_area.add sdla draw_lines;
  let discs = List.map create_disc centers in

  L.superpose ~w:width ~h:height (L.resident sdlw :: discs)

let create_graph data = ()

(* let build_axis d r = let area = W.sdl_area ~w:width ~h:height () in Draw.line
   ~color:Draw.(opaque black) ~x0:0 ~y0:0 ~x1:width ~y1:height

   let board = (build_axis dim (W.box ())) |> L.resident |> Bogue.of_layout let
   () = Bogue.run board *)
(* let board = Bogue.of_layout area let () = Bogue.run board *)

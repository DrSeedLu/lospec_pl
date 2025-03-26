open Claudius


(* This generates a grid-based pattern inspired by Vera Molnár *)
let structured_grid t s =
  let width, height = Screen.dimensions s in
  let grid_size = 5 in  (* 5x5 grid *)
  let cell_w, cell_h = width / grid_size, height / grid_size in
  let shapes = List.concat (
    List.init grid_size (fun x ->
      List.init grid_size (fun y ->
        let xo, yo = x * cell_w, y * cell_h in
        let col = (t + x + y) mod ((Palette.size (Screen.palette s)) - 1) in
        if (x + y + t / 30) mod 2 = 0 then
          Primitives.Rect ({ x = xo + 10; y = yo + 10 }, { x = xo + cell_w - 10; y = yo + cell_h - 10 }, col)
        else
          Primitives.Line ({ x = xo; y = yo }, { x = xo + cell_w; y = yo + cell_h }, col)
      )
    )
  ) in
  shapes

(* The Tick function generates a structured movement *)
let tick t s p _i =
  let shapes = structured_grid t s in
  Framebuffer.render p shapes;
  p

let () =
  Palette.of_list (0x000000 :: (List.rev (Palette.to_list (Palette.generate_plasma_palette 31)))) |> (* More structured palette *)
  Screen.create 800 800 1 |>
  Base.run "Seed Lu's Structured Geometry" None tick

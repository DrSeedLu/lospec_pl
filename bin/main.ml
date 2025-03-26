open Claudius


let structured_grid t s =
  let width, height = Screen.dimensions s in
  let grid_size = 8 in  (* 8x8 grid *)
  let cell_w, cell_h = width / grid_size, height / grid_size in
  let shapes = List.concat (
    List.init grid_size (fun x ->
      List.init grid_size (fun y ->
        let xo, yo = x * cell_w, y * cell_h in
        let col = (t + x + y) mod ((Palette.size (Screen.palette s)) - 1) in
        if (x + y + t / 30) mod 2 = 0 then
          
          Primitives.Rect (
            { x = xo + 4; y = yo + 4 },
            { x = xo + cell_w - 4; y = yo + cell_h - 4 },
            col
          )
        else
          
          Primitives.Polygon (
            [
              { x = xo + (cell_w / 2); y = yo };                      (* top *)
              { x = xo + cell_w; y = yo + (cell_h / 2) };             (* right *)
              { x = xo + (cell_w / 2); y = yo + cell_h };             (* bottom *)
              { x = xo; y = yo + (cell_h / 2) }                       (* left *)
            ],
            col
          )
      )
    )
  ) in
  shapes

(* This tick function generates a structured movement *)
let tick t s p _i =
  let shapes = structured_grid t s in
  Framebuffer.render p shapes;
  p

(* A this point I load a Lospec .hex palette file and convert it to a Palette.t *)
let load_lospec_hex_palette (filename : string) : Palette.t =
  let lines = In_channel.with_open_bin filename In_channel.input_lines in
  let parse_hex line =
    let line = String.trim line in
    let hex =
      if String.length line = 6 then line
      else if String.length line = 7 && line.[0] = '#' then String.sub line 1 6
      else ""
    in
    if hex <> "" then
      match int_of_string_opt ("0x" ^ hex) with
      | Some n -> Some n
      | None -> None
    else
      None
  in
  let color_list = List.filter_map parse_hex lines in
  if color_list = [] then failwith "No valid colors found in HEX file";
  Palette.of_list color_list

let () =
  load_lospec_hex_palette "endesga-soft-16.hex" |>
  Screen.create 800 800 1 |>
  Base.run "Seed Lu's Structured Shapes" None tick

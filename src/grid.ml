(*
 * What we call a `grid` is infact a graph of blocs. The vertices are the blocs
 * and the edges are the relations between the blocs and the other reachable
 * blocs, indexed by a way to `choose` a neighbor.
 *)

type colors =
    { foreground : Jraphics.color
    ; background : Jraphics.color
    ; focus      : Jraphics.color
    ; water      : Jraphics.color }

type t =
    { mutable focus     : int * int
    ;         matrix    : Bloc.t array array
    ; mutable bloc_size : int
    ; mutable top_left  : int * int
    ; mutable colors    : colors }

exception Parse_error of string

let matrix_of_string str =
  let grid = ref [] in
  let line = ref [] in
  let c1 = ref 'E' in
  let c2 = ref ' ' in
  let flush_chars () =
    let bloc =
      try
	Bloc.of_chars (!c1) (!c2)
      with
      | Bloc.Parse_error s ->
	 raise (Parse_error (Format.sprintf "Couldn't parse bloc: %c%c" (!c1) (!c2)))
    in
    line := bloc :: !line;
    c1 := 'E';
    c2 := ' '
  in
  let flush_line () =
    grid := !line :: !grid;
    line := []
  in
  for i = 0 to (String.length str) - 1 do
    match str.[i] with
    | 'E' | 'P' | 'B' | 'L' | 'F' | 'C' -> c1 := str.[i]
    | ' ' -> flush_chars ()
    | '\n' -> flush_chars (); flush_line ()
    | c -> c2 := c
  done;
  { focus = 0, 0
  ; matrix = Array.of_list (List.rev_map (fun l -> Array.of_list (List.rev l)) !grid) }

let make str =
  { focus     = 0, 0
  ; matrix    = matrix_of_string str
  ; top_left  = 0, 0
  ; bloc_size = 40
  ; { foreground = Jraphics.black
    ; background = Jraphics.white
    ; focus      = Jraphics.lightgray
    ; water      = Jraphics.blue } }

let get_colors grid = grid.colors
let set_colors grid colors = grid.colors <- colors
    
let to_image grid =
  let height = Array.length grid in
  let width  = Array.length grid.(0) in
  let image = Array.make_matrix (n * height) (n * width) grid.colors.background in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let bloc_image =
	Bloc.to_image
	  ~size:(n)
	  ~color:(color)
	  grid.(i).(j)
      in
      for ib = 0 to n - 1 do
	for jb = 0 to n - 1 do
	  image.(i * n + ib).(j * n + jb) <- bloc_image.(ib).(jb)
	done
      done
    done
  done;
  image

(* partial function, you still have to give the left-corner coordinates *)
let draw
      ?block_size:(n=40)
      ?color:(color=Jraphics.black)
      grid =
  Jraphics.draw_image (to_image ~block_size:(n) ~color:(color) grid)


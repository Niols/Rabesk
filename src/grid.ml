type t = Bloc.t array array

exception Parse_error of string

let of_string str =
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
  Array.of_list (List.rev_map (fun l -> Array.of_list (List.rev l)) !grid)
    
			      
let to_image
      ?block_size:(n=40)
      ?color:(color=Jraphics.black)
      grid =
  let height = Array.length grid in
  let width  = Array.length grid.(0) in
  let image = Array.make_matrix (n * height) (n * width) Jraphics.transp in
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


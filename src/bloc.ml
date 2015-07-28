module List = struct
  include List

  let memi e l =
    (** Like mem but instead of returning a boolean, returns the indice. *)
    let rec memi_aux i = function
      | []                 -> None
      | t :: q when t == e -> Some i
      | t :: q             -> memi_aux (i+1) q
    in
    memi_aux 0 l

  let choose l = List.nth l (Random.int (List.length l))
end

module Utils = struct
  let rec repeat f n x =
    if n <= 0 then x
    else repeat f (n-1) (f x)
end


		
module Card = struct
  type t = N | S | E | W
  let turn_cw  = function | N -> E | E -> S | S -> W | W -> N
  let turn_ccw = function | N -> W | W -> S | S -> E | E -> N
end

type t =
    { mutable links : Card.t list ; }

let turn_cw bloc  = bloc.links <- List.map Card.turn_cw  bloc.links
let turn_ccw bloc = bloc.links <- List.map Card.turn_ccw bloc.links
  
let create links =
  { links = links }

exception Parse_error of string

let of_chars c1 c2 =
  let links =
    match c1 with
    | 'E' -> []
    | 'P' -> [Card.N]
    | 'B' -> [Card.N; Card.E]
    | 'L' -> [Card.N; Card.S]
    | 'F' -> [Card.N; Card.E; Card.S]
    | 'C' -> [Card.N; Card.E; Card.S; Card.W]
    | c -> raise (Parse_error (Format.sprintf "Unexpected char `%c`" c))
  in
  let rot =
    match c2 with
    | ' ' -> Random.int 4
    | n ->
       try
	 int_of_string (String.make 1 c2)
       with
	 Failure _ ->
	 raise (Parse_error (Format.sprintf "Couldn't parse `%c` as an int" c2))
  in
  let bloc = create links in
  for i = 1 to rot do
    turn_cw bloc
  done;
  bloc

let of_string str =
  match String.length str with
  | 1 -> of_chars str.[0] ' '
  | 2 -> of_chars str.[0] str.[1]
  | s -> raise (Parse_error (Format.sprintf "Unexpected size for a bloc: %d" s))

let to_string ?random_orientation:(rndori=false) bloc =
  assert false

let print bloc = print_string (to_string bloc)

let to_image
      ?size:(n=20) (* must be pair *)
      ?color:(c=Jraphics.black)
      bloc =
  let n2 = n / 2 in
  let image = Array.make_matrix n n Jraphics.transp in
  let add_card card =
    let i0,i1,j0,j1 =
      begin
	match card with
	| Card.N ->      0 ,    n2 , n2 - 1 ,    n2
	| Card.E -> n2 - 1 ,    n2 , n2 - 1 , n - 1
	| Card.S -> n2 - 1 , n - 1 , n2 - 1 ,    n2
	| Card.W -> n2 - 1 ,    n2 ,      0 ,    n2
      end
    in
    for i = i0 to i1 do
      for j = j0 to j1 do
	image.(i).(j) <- c
      done
    done
  in
  List.iter add_card bloc.links;
  if List.length bloc.links = 1 then
    for i = n2 - 3 to n2 + 2 do
      for j = n2 - 3 to n2 + 2 do
	image.(i).(j) <- c
      done
    done;
  image

(* partial function, you still have to give the left-corner coordinates *)
let draw bloc = Jraphics.draw_image (to_image bloc)

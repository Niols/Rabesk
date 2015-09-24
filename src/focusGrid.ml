type action =
  | Button      of int * int
  | Key_pressed of char

type 'a element =
    { i      : int
    ; j      : int
    ; w      : int
    ; h      : int
    ; img    : bool -> Jraphics.image
    ; elmt   : 'a
    ; action : action -> unit }

type 'a t =
    { mutable focus    : 'a
    ;         grid     : ('a, 'a element) Hashtbl.t
    ;         actions  : ('a, (action, 'a) Hashtbl.t) Hashtbl.t } (* 'a -> action -> 'a *)
			  
let make_element (elmt, i, j, w, h, img, action) =
  { i      = i
  ; j      = j
  ; w      = w
  ; h      = h
  ; img    = img
  ; elmt   = elmt
  ; action = action }

let make ?size:(size=8) elmt_tuple =
  let element = make_element elmt_tuple in
  let grid = Hashtbl.create size in
  Hashtbl.add grid element.elmt element;
  let actions = Hashtbl.create size in
  Hashtbl.add actions element.elmt (Hashtbl.create 4);
  { focus    = element.elmt
  ; grid     = grid
  ; actions  = actions }
    
let find grid elmt =
  let _ = Hashtbl.find grid.actions elmt in (*let Not_found pass*)
  Hashtbl.find grid.grid elmt

let mem grid elmt =
  Hashtbl.mem grid.grid elmt && Hashtbl.mem grid.actions elmt

let add grid elmt_tuple =
  let element = make_element elmt_tuple in
  if mem grid element.elmt then
    raise (Invalid_argument "add");
  Hashtbl.add grid.grid element.elmt element;
  Hashtbl.add grid.actions element.elmt (Hashtbl.create 4)

let has_focus grid elmt =
  grid.focus = elmt

let draw grid =
  (*TODO: check if size OK*)
  Hashtbl.iter
    (fun elmt element -> Jraphics.draw_image (element.img (has_focus grid element.elmt)) element.i element.j)
    grid.grid

let refresh_elmt ?background:(bg=Jraphics.background) grid elmt =
  let element = find grid elmt in
  (*let c = Jraphics.get_color () in*)
  Jraphics.set_color bg;
  Jraphics.fill_rect element.i element.j element.w element.h;
  (*Jraphics.set_color c;*)
  Jraphics.draw_image (element.img (has_focus grid elmt)) element.i element.j

let refresh grid =
  Hashtbl.iter
    (fun elmt element -> refresh_elmt grid elmt)
    grid.grid

let set_focus grid elmt =
  let _ = find grid elmt in (*let Not_found pass*)
  grid.focus <- elmt

let get_focus grid =
  grid.focus

let add_focus_change grid elmt1 action elmt2 =
  let action_tbl = Hashtbl.find grid.actions elmt1 in (*let Not_found pass*)
  Hashtbl.add action_tbl action elmt2

let get_new_focus grid elmt action =
  let action_tbl = Hashtbl.find grid.actions elmt in (*let Not_found pass*)
  try
    Hashtbl.find action_tbl action (*but catch this one to raise Invalid_argument*)
  with
  | Not_found -> raise (Invalid_argument "get_new_focus")
		       
let change_focus grid action =
  let elmt1 = grid.focus in
  let elmt2 = get_new_focus grid elmt1 action in
  grid.focus <- elmt2;
  refresh grid elmt1;
  refresh grid elmt2

let wait_next_event () =
  let s =
    Jraphics.wait_next_event
      [ Jraphics.Button_up
      ; Jraphics.Button_down
      ; Jraphics.Key_pressed ]
  in
  let action =
    (
      if s.button then
	Button (s.mouse_i, s.mouse_j)
      else if 

let show_menu () =
  assert false
	 
let prog_open name =
  Jraphics.open_graph "";
  Jraphics.set_window_title name;
  Jraphics.resize_window 600 800

let prog_close i=
  Jraphics.close_graph ();
  exit i


let _ =
  let grid = Grid.of_string
	       "P2 B1 B1 B0 E0
P2 L0 F0 C0 P3
B3 F1 P3 F2 P0
P2 B0 F1 C0 B3
F1 B0 L1 L1 L1
L1 B3 F0 C0 B3
B0 B0 P1 B3 P2
B1 C0 B0 P3 P1
F0 F2 P2 L0 L0
L0 L0 B3 F2 B3
B3 B2 E0 E0 E0
E0 E0 P0 L1 P0
"
  in
  
  let i0, j0 = 10, 10 in
  let block_size = 40 in
  
  prog_open "Rabesk v0.1";
  
  let fill_block ?color:(color=Jraphics.white) i j =
    Jraphics.set_color color;
    Jraphics.fill_rect (i0 + i * block_size) (j0 + j * block_size) block_size block_size
  in
  
  let curr_i = ref 0 in
  let curr_j = ref 0 in
  
  let move_curr i j =
    fill_block (!curr_i) (!curr_j);
    if i <> -1 then curr_i := i;
    if j <> -1 then curr_j := j;
    fill_block ~color:(Jraphics.light_gray) (!curr_i) (!curr_j)
  in

  move_curr 0 0;
  
  while true do
    Grid.draw ~block_size:(block_size) grid i0 j0;
    
    let status = Jraphics.wait_next_event [Jraphics.Button_down; Jraphics.Key_pressed] in
    if status.Jraphics.keypressed then
      (
	if status.Jraphics.key = 'q' then
	  prog_close 0
	else if status.Jraphics.key = 'j' && !curr_j > 0 then
	  move_curr (-1) ((!curr_j) - 1)
	else if status.Jraphics.key = 'k' && !curr_i > 0 then
	  move_curr ((!curr_i) - 1) (-1)
	else if status.Jraphics.key = 'l' && !curr_i < (Array.length grid) - 1 then
	  move_curr ((!curr_i) + 1) (-1)
	else if status.Jraphics.key = 'm' && !curr_j < (Array.length grid.(0)) - 1 then
	  move_curr (-1) ((!curr_j) + 1)
	else if status.Jraphics.key = ' ' then
	  (
	    move_curr (-1) (-1);
	    Bloc.turn_cw (grid.(!curr_i).(!curr_j))
	  )
      )
    else
      (
	let m_i = status.Jraphics.mouse_i in
	let m_j = status.Jraphics.mouse_j in
	let i = (m_i - 10) / 40 in
	let j = (m_j - 10) / 40 in
	(*Format.printf "Mouse pressed: %d * %d (%d * %d)@." m_i m_j i j;*)
	Bloc.turn_cw (grid.(i).(j));
	move_curr i j
      )
  done

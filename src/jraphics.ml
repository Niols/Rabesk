(**
   Jraphics : Graphics, version Jeannerod

   - line * column coordinates instead of abscissa * ordinate
   - all CSS colors
   - images are usable directly as color array array
 *)

(* should not appear in the signature *)
let i2y i = (Graphics.size_y ()) - i - 1
let j2x j = j
let x2j = j2x
let y2i = i2y

let di2dy di = -di
let dj2dx dj = dj
let dx2dj = dj2dx
let dy2di = di2dy

let ij2xy (i, j) = (j2x j, i2y i)
let xy2ij (x, y) = (y2i y, x2j x)
let ijij2xyxy (i, j, i', j') = (j2x j, i2y i, j2x j', i2y i')

let dij2dxy (di, dj) = (dj2dx dj, di2dy di)
let dxy2dij (dx, dy) = (dy2di dy, dx2dj dx)
				 
exception Jraphics_failure of string

let open_graph               = Graphics.open_graph
let close_graph              = Graphics.close_graph
let set_window_title         = Graphics.set_window_title
let resize_window            = Graphics.resize_window
let clear_graph              = Graphics.clear_graph

let size_i                   = Graphics.size_y
let size_j                   = Graphics.size_x

type color                   = Graphics.color
let rgb                      = Graphics.rgb
let set_color                = Graphics.set_color
let background               = Graphics.background
let foreground               = Graphics.foreground

(* All CSS colors: http://www.w3schools.com/cssref/css_colornames.asp *)
let alice_blue               = 0xF0F8FF
let antique_white            = 0xFAEBD7
let aqua                     = 0x00FFFF
let aquamarine               = 0x7FFFD4
let azure                    = 0xF0FFFF
let beige                    = 0xF5F5DC
let bisque                   = 0xFFE4C4
let black                    = 0x000000
let blanched_almond          = 0xFFEBCD
let blue                     = 0x0000FF
let blue_violet              = 0x8A2BE2
let brown                    = 0xA52A2A
let burly_wood               = 0xDEB887
let cadet_blue               = 0x5F9EA0
let chartreuse               = 0x7FFF00
let chocolate                = 0xD2691E
let coral                    = 0xFF7F50
let cornflower_blue          = 0x6495ED
let cornsilk                 = 0xFFF8DC
let crimson                  = 0xDC143C
let cyan                     = 0x00FFFF
let dark_blue                = 0x00008B
let dark_cyan                = 0x008B8B
let dark_golden_rod          = 0xB8860B
let dark_gray                = 0xA9A9A9
let dark_green               = 0x006400
let dark_khaki               = 0xBDB76B
let dark_magenta             = 0x8B008B
let dark_olive_green         = 0x556B2F
let dark_orange              = 0xFF8C00
let dark_orchid              = 0x9932CC
let dark_red                 = 0x8B0000
let dark_salmon              = 0xE9967A
let dark_sea_green           = 0x8FBC8F
let dark_slate_blue          = 0x483D8B
let dark_slate_gray          = 0x2F4F4F
let dark_turquoise           = 0x00CED1
let dark_violet              = 0x9400D3
let deep_pink                = 0xFF1493
let deep_sky_blue            = 0x00BFFF
let dim_gray                 = 0x696969
let dodger_blue              = 0x1E90FF
let fire_brick               = 0xB22222
let floral_white             = 0xFFFAF0
let forest_green             = 0x228B22
let fuchsia                  = 0xFF00FF
let gainsboro                = 0xDCDCDC
let ghost_white              = 0xF8F8FF
let gold                     = 0xFFD700
let golden_rod               = 0xDAA520
let gray                     = 0x808080
let green                    = 0x008000
let green_yellow             = 0xADFF2F
let honey_dew                = 0xF0FFF0
let hot_pink                 = 0xFF69B4
let indian_red               = 0xCD5C5C
let indigo                   = 0x4B0082
let ivory                    = 0xFFFFF0
let khaki                    = 0xF0E68C
let lavender                 = 0xE6E6FA
let lavender_blush           = 0xFFF0F5
let lawn_green               = 0x7CFC00
let lemon_chiffon            = 0xFFFACD
let light_blue               = 0xADD8E6
let light_coral              = 0xF08080
let light_cyan               = 0xE0FFFF
let light_golden_rod_yellow  = 0xFAFAD2
let light_gray               = 0xD3D3D3
let light_green              = 0x90EE90
let light_pink               = 0xFFB6C1
let light_salmon             = 0xFFA07A
let light_sea_green          = 0x20B2AA
let light_sky_blue           = 0x87CEFA
let light_slate_gray         = 0x778899
let light_steel_blue         = 0xB0C4DE
let light_yellow             = 0xFFFFE0
let lime                     = 0x00FF00
let lime_green               = 0x32CD32
let linen                    = 0xFAF0E6
let magenta                  = 0xFF00FF
let maroon                   = 0x800000
let medium_aqua_marine       = 0x66CDAA
let medium_blue              = 0x0000CD
let medium_orchid            = 0xBA55D3
let medium_purple            = 0x9370DB
let medium_sea_green         = 0x3CB371
let medium_slate_blue        = 0x7B68EE
let medium_spring_green      = 0x00FA9A
let medium_turquoise         = 0x48D1CC
let medium_violet_red        = 0xC71585
let midnight_blue            = 0x191970
let mint_cream               = 0xF5FFFA
let misty_rose               = 0xFFE4E1
let moccasin                 = 0xFFE4B5
let navajo_white             = 0xFFDEAD
let navy                     = 0x000080
let old_lace                 = 0xFDF5E6
let olive                    = 0x808000
let olive_drab               = 0x6B8E23
let orange                   = 0xFFA500
let orange_red               = 0xFF4500
let orchid                   = 0xDA70D6
let pale_golden_rod          = 0xEEE8AA
let pale_green               = 0x98FB98
let pale_turquoise           = 0xAFEEEE
let pale_violet_red          = 0xDB7093
let papaya_whip              = 0xFFEFD5
let peach_puff               = 0xFFDAB9
let peru                     = 0xCD853F
let pink                     = 0xFFC0CB
let plum                     = 0xDDA0DD
let powder_blue              = 0xB0E0E6
let purple                   = 0x800080
let rebecca_purple           = 0x663399
let red                      = 0xFF0000
let rosy_brown               = 0xBC8F8F
let royal_blue               = 0x4169E1
let saddle_brown             = 0x8B4513
let salmon                   = 0xFA8072
let sandy_brown              = 0xF4A460
let sea_green                = 0x2E8B57
let sea_shell                = 0xFFF5EE
let sienna                   = 0xA0522D
let silver                   = 0xC0C0C0
let sky_blue                 = 0x87CEEB
let slate_blue               = 0x6A5ACD
let slate_gray               = 0x708090
let snow                     = 0xFFFAFA
let spring_green             = 0x00FF7F
let steel_blue               = 0x4682B4
let tan                      = 0xD2B48C
let teal                     = 0x008080
let thistle                  = 0xD8BFD8
let tomato                   = 0xFF6347
let turquoise                = 0x40E0D0
let violet                   = 0xEE82EE
let wheat                    = 0xF5DEB3
let white                    = 0xFFFFFF
let white_smoke              = 0xF5F5F5
let yellow                   = 0xFFFF00
let yellow_green             = 0x9ACD32

let transp                   = Graphics.transp

let plot i j                 = Graphics.plot (j2x j) (i2y i)
let plots pt_arr             = Graphics.plots (Array.map ij2xy pt_arr)
let point_color i j          = Graphics.point_color (j2x j) (i2y i)
let moveto i j               = Graphics.moveto (j2x j) (i2y i)
let rmoveto di dj            = Graphics.rmoveto dj (-di)
let current_i ()             = y2i (Graphics.current_y ())
let current_j ()             = x2j (Graphics.current_x ())
let current_point ()         = xy2ij (Graphics.current_point ())
let lineto i j               = Graphics.lineto (j2x j) (i2y i)
let rlineto di dj            = Graphics.rlineto dj (-di)
let curveto b c d            = Graphics.curveto (ij2xy b) (ij2xy c) (ij2xy d)
let draw_rect i j w h        = Graphics.draw_rect (j2x j) ((i2y i) - h - 1) w h (* top left corner *)
let draw_poly_line pt_arr    = Graphics.draw_poly_line (Array.map ij2xy pt_arr)
let draw_poly pt_arr         = Graphics.draw_poly (Array.map ij2xy pt_arr)
let draw_segments seg_arr    = Graphics.draw_segments (Array.map ijij2xyxy seg_arr)
let draw_arc i j ri rj a1 a2 = Graphics.draw_arc (j2x j) (i2y i) rj ri a1 a2
let draw_ellipse i j ri rj   = Graphics.draw_ellipse (j2x j) (i2y i) rj ri
let draw_circle i j r        = Graphics.draw_circle (j2x j) (i2y i) r
let set_line_width           = Graphics.set_line_width

let set_font                 = Graphics.set_font
let set_text_size            = Graphics.set_text_size
let text_size                = Graphics.text_size
let draw_string s =
  let _, h = text_size s in
  rmoveto h 0;
  Graphics.draw_string s;
  rmoveto (-h) 0
let draw_char c =
  draw_string (String.make 1 c)

let fill_rect i j w h        = Graphics.fill_rect (j2x j) ((i2y i) - h - 1) w h (* top left corner *)
let fill_poly pt_arr         = Graphics.fill_poly (Array.map ij2xy pt_arr)
let fill_arc i j ri rj a1 a2 = Graphics.fill_arc (j2x j) (i2y i) rj ri a1 a2
let fill_ellipse i j ri rj   = Graphics.fill_ellipse (j2x j) (i2y i) rj ri
let fill_circle i j r        = Graphics.fill_circle (j2x j) (i2y i) r

type image                   = color array array

let image_height img =
  match Array.length img with
  | 0 -> raise (Jraphics_failure "null height")
  | h -> h

let image_width img =
  let _ = image_height img in (* to check the not empty *)
  match Array.length img.(0) with
  | 0 -> raise (Jraphics_failure "null width")
  | w ->
     Array.iter
       (fun line ->
	if Array.length line <> w then
	  raise (Jraphics_failure "different line widths"))
       img;
     w

let draw_image img i j =
  Graphics.draw_image
    (Graphics.make_image img)
    (j2x j)
    ((i2y i) - (image_height img) - 1) (* top left corner *)

let get_image i j w h = Graphics.get_image (j2x j) ((i2y i) - h - 1) w h (* top left corner *)

type status =
    { mouse_i : int ;
      mouse_j : int ;
      button : bool ;
      keypressed : bool ;
      key : char }

let gstatus_to_jstatus e =
  { mouse_i    = y2i e.Graphics.mouse_y ;
    mouse_j    = x2j e.Graphics.mouse_x ;
    button     = e.Graphics.button      ;
    keypressed = e.Graphics.keypressed  ;
    key        = e.Graphics.key           }

let jstatus_to_gstatus e =
  { Graphics.mouse_x    = j2x e.mouse_j ;
    Graphics.mouse_y    = i2y e.mouse_i ;
    Graphics.button     = e.button      ;
    Graphics.keypressed = e.keypressed  ;
    Graphics.key        = e.key           }

type event =
  | Button_down
  | Button_up
  | Key_pressed
  | Mouse_motion
  | Poll

let gevent_to_jevent = function
  | Graphics.Button_down  -> Button_down
  | Graphics.Button_up    -> Button_up
  | Graphics.Key_pressed  -> Key_pressed
  | Graphics.Mouse_motion -> Mouse_motion
  | Graphics.Poll         -> Poll

let jevent_to_gevent = function
  | Button_down  -> Graphics.Button_down
  | Button_up    -> Graphics.Button_up
  | Key_pressed  -> Graphics.Key_pressed
  | Mouse_motion -> Graphics.Mouse_motion
  | Poll         -> Graphics.Poll
      
let wait_next_event el =
  gstatus_to_jstatus (Graphics.wait_next_event (List.map jevent_to_gevent el))

let loop_at_exit el f =
  Graphics.loop_at_exit el (fun s -> f (gstatus_to_jstatus s))

let mouse_pos () = xy2ij (Graphics.mouse_pos ())
let button_down = Graphics.button_down
let read_key = Graphics.read_key
let key_pressed = Graphics.key_pressed

(* let sound = Graphics.sound (* Who wants sound like that?! *) *)

let auto_synchronize = Graphics.auto_synchronize
let synchronize = Graphics.synchronize
let display_mode = Graphics.display_mode
let remember_mode = Graphics.remember_mode

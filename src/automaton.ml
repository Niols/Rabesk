(**
 * Automaton
 *
 * This class represents finite-state automatons.
 *)

(*-
 * TODO list
 *
 * - Add a way to define automatons with a state stream. That would be really
 *   usefull when we don't care about the state definition (the int stream would
 *   be a good example). That could be done in an other file.
 *
 * - Add an other module defining deterministic automatons.
 *)

type ('s, 'l) t =
    { states        : 's list
    ; start_states  : 's list
    ; accept_states : 's list
    ; transitions   : ('s * 'l * 's) list }
(** The type of automatons with states' type 's and letters' type 'l. *)

exception Error of string

let empty =
  { states        = []
  ; start_states  = []
  ; accept_states = []
  ; transitions   = [] }
(** The empty automaton. *)

(* val add_state : ('s, 'l) t -> 's -> ('s, 'l) t *)
let add_state a s =
  if List.mem s a.states then
    raise (Error "state exists");
  { a with states = s :: a.states }

let del_state a s =
  if not (List.mem s a.states) then
    raise (Error "state doesn't exist");
  let not_s s' = s' != s in
  { states        = List.filter not_s a.states
  ; start_states  = List.filter not_s a.start_states
  ; accept_states = List.filter not_s a.accept_states
  ; transitions   =
      List.filter (fun (s', l, s'') -> s' != s && s'' != s) a.transitions }

let add_start_state a s =
  if not (List.mem s a.states) then
    raise (Error "state doesn't exist");
  if List.mem s a.start_states then
    raise (Error "start state exists");
  { a with start_states = s :: a.start_states }

let del_start_state a s =
  if not (List.mem s a.start_states) then
    raise (Error "start state doesn't exist");
  { a with start_states = List.filter (fun s' -> s' != s) a.start_states }

let add_accept_state a s =
  if not (List.mem s a.states) then
    raise (Error "state doesn't exist");
  if List.mem s a.accept_states then
    raise (Error "accept state doesn't exist");
  { a with accept_states = s :: a.accept_states }

let del_accept_state a s =
  if not (List.mem s a.accept_states) then
    raise (Error "accept state doesn't exist");
  { a with accept_states = List.filter (fun s' -> s' != s) a.accept_states }

let add_transition a s1 l s2 =
  if not (List.mem s1 a.states) then       (* To do them faster, these two    *)
    raise (Error "state 1 doesn't exist"); (* tests could be done at the same *)
  if not (List.mem s2 a.states) then       (* time with a List.iter raising a *)
    raise (Error "state 2 doesn't exist"); (* non-caught exception.           *)
  if List.mem (s1, l, s2) a.transitions then
    raise (Error "transition exists");
  if d && List.exists (fun (s1',l',s2') -> s1=s1' && l=l') a.transitions then
    raise (Error "transition exists");












			   (*

let create_node v =
  { value = v
  ; neighbors = Hashtbl.create 0 }

let node_value n =
  n.value

let create () = []
let card g = List.length g

let add g n = g.nodes <- n :: g.nodes
let add_value g v = add g (create_node v)

let del g n = g.nodes <- List.filter (fun n' -> n != n') g.nodes

let mem g n = List.mem n g.nodes
let mem_value g v = List.exists (fun n -> n.value = v) g

let add_edge g n1 i n2 =
  if not (mem g n1) then
    raise (Invalid_argument "add_edge: origin"); (* origin doesn't exist in graph *)
  if Hashtbl.mem n1.neighbors i then
    raise (Invalid_argument "add_edge: indice"); (* indice already exist in graph for this node *)
  if not (mem g n2) then
    raise (Invalid_argument "add_edge: destination"); (* destination doesn't exist in graph *)
  Hashtbl.add n1.neighbors i n2

let del_edge g n1 i =
  if not (mem g n1) then
    raise (Invalid_argument "del_edge: origin"); (* origin doesn't exist in graph *)
  if not (Hashtbl.mem n1.neighbors i) then
    raise (Invalid_argument "del_edge: indice"); (* indice doesn't exist in graph for this origin *)
  Hashtbl.remove n1.neighbors i

let find_neighbor g n1 i =
  if not (mem g n1) then
    raise (Invalid_argument "find_neighbor: origin"); (* origin doesn't exist in graph *)
  if not (Hashtbl.mem n1.neighbors i) then
    raise (Invalid_argument "find_neighbor: indice"); (* indice doesn't exist in graph for this origin *)
  Hashtbl.find n1.neighbors i


 *)

(*
  Program interaktywny do rysowania grafow. Projekt na programowanie funkcyjne 2015/2016.
  Grzegorz Jurdzinski, II UWr 2016
*)

open GraphDrawing;;
open Graphics;;

module MouseMode (GD : GRAPH_DRAWING with module V = V) =
struct
  (* rysuje przyciski "RESET", "NEW GRAPH" i "CHANGE FILLING" *)
  let draw_buttons () =
    let draw_string_center s c =
      let sx, sy = Graphics.text_size s in
      Graphics.moveto (fst c - sx/2) (snd c - sy/2);
      Graphics.draw_string s
    in
    Graphics.set_text_size 12;
    Graphics.set_color Graphics.black;
    Graphics.draw_rect 0 0 100 50;
    draw_string_center "RESET" (50, 25);
    Graphics.draw_rect 100 0 100 50;
    draw_string_center "NEW GRAPH" (150, 25);
    Graphics.draw_rect 200 0 100 50;
    draw_string_center "CHANGE FILLING" (250, 25)

  (* oczekiwanie na akcje *)
  let rec base_mouse_mode gd =
    draw_buttons ();
    let status = Graphics.wait_next_event
      [Graphics.Button_down; Graphics.Key_pressed] in
    if status.keypressed then
      (
	if status.key = 'q' then
	  GD.close_window ()
	else if status.key = 'r' then
	  ( GD.reset gd; base_mouse_mode gd )
	else if status.key = 'f' then
	  ( GD.change_filling gd (not (GD.get_filling gd)); base_mouse_mode gd )
	else
	  base_mouse_mode gd
      )
    else
      (* szukamy wierzcholkow oddalonych od myszki o mniej niz promien wirzcholka *)
      let vs = GD.G.find_v (GD.get_graph gd)
	(fun v ->
	  let vx, vy = GD.V.coords v and mx, my = (status.mouse_x, status.mouse_y) in
	  let x = vx - mx and y = vy - my in
	  x * x + y * y >= (GD.get_vertex_r gd) * (GD.get_vertex_r gd)) in
      match vs with
      (* jesli nie zlapalismy zadnego wierzcholka, to moze jakis przycisk *)
      | [] ->
	 if status.mouse_y <= 50 && status.mouse_x <= 300 then (* ktorys z przyciskow *)
	   (
	     if status.mouse_x <= 100 then (* reset *)
	       ( GD.reset gd; base_mouse_mode gd )
	     else if status.mouse_x <= 200 then (* new graph *)
	       GD.close_window ()
	     else (* change filling *)
	       ( GD.change_filling gd (not (GD.get_filling gd)); base_mouse_mode gd )
	   )
	 else
	   base_mouse_mode gd
      (* jesli zlapie sie kilka wierzch., to chce przesunac tylko jeden, bo inaczej wszystki znajda
	 sie w tym samym miejscu i myszka juz ich nie rozdzielimy - dlatego tylko v, a nie vs *)
      | v :: _ -> wait_mouse_released gd v
  (* jesli "zlapany" zostal jakis wierzcholek, to czekamy, az zostanie upuszczony w nowym miejscu *)
  and wait_mouse_released gd v =
    let status = Graphics.wait_next_event [Graphics.Button_up] in
    GD.move_v_to gd v (status.mouse_x, status.mouse_y);
    base_mouse_mode gd

  let start_mouse_mode gd =
    base_mouse_mode gd
end;;

module MM = MouseMode(GD);;
module DMM = MouseMode(DGD);;


(* potrzebne do Str.split *)
open Str;;

let string_to_color str =
  match str with
  | "black" -> Graphics.black
  | "white" -> Graphics.white
  | "red" -> Graphics.red
  | "green" -> Graphics.green
  | "blue" -> Graphics.blue
  | "yellow" -> Graphics.yellow
  | "cyan" -> Graphics.cyan
  | "magenta" -> Graphics.magenta
  | _ -> failwith "Unknown color!"
;;

(* wczytuje liste krawedzi grafu prostego *)
let rec read_simple_edges_list_e vs m =
  match m with
  | 0 -> []
  | _ ->
     match split (regexp " ") (read_line ()) with
     | [v; w; label; color] ->
	(E.create
	   (List.nth vs (int_of_string v))
	   (List.nth vs (int_of_string w))
	   ~lab:label
	   ~col:(string_to_color color)
	   ()) :: (read_simple_edges_list_e vs (m-1))
     | _ -> failwith "Wrong input formatting!"
;;
(* wczytuje liste sasiedztwa dla grafu prostego *)
let read_simple_edges_list_v vs =
  let read_for_one_v v =
    let line = split (regexp " ") (read_line ()) in
    let rec parse_line line =
      match line with
      | [] -> []
      | w :: label :: color :: tail ->
	 print_endline ("Adding edge from " ^ (V.label v) ^ " to " ^ label ^ " (" ^ w ^ ") with color " ^ color);
	 (E.create
	    v
	    (List.nth vs (int_of_string w))
	    ~lab:label
	    ~col:(string_to_color color)
	    ()) :: (parse_line tail)
      | _ -> failwith "Wrong input formatting!"
    in parse_line line
  in let res = ref []
  in
  for i = 0 to (List.length vs) - 1 do
    res := !res @ (read_for_one_v (List.nth vs i))
  done;
  !res
;;

(* wczytuje liste krawedzi grafu skierowanego *)
let rec read_directed_edges_list_e vs m =
  match m with
  | 0 -> []
  | _ ->
     match split (regexp " ") (read_line ()) with
     | [v; w; label; color] ->
	(DE.create
	   (List.nth vs (int_of_string v))
	   (List.nth vs (int_of_string w))
	   ~lab:label
	   ~col:(string_to_color color)
	   ()) :: (read_directed_edges_list_e vs (m-1))
     | _ -> failwith "Wrong input formatting!"
;;
(* wczytuje liste sasiedztwa dla grafu skierowanego *)
let rec read_directed_edges_list_v vs =
  let read_for_one_v v =
    let line = split (regexp " ") (read_line ()) in
    let rec parse_line line =
      match line with
      | [] -> []
      | w :: label :: color :: tail ->
	 (DE.create
	    v
	    (List.nth vs (int_of_string w))
	    ~lab:label
	    ~col:(string_to_color color)
	    ()) :: (parse_line tail)
      | _ -> failwith "Wrong input formatting!"
    in
    parse_line line
  in
  let rec read_aux vs =
    match vs with
    | [] -> []
    | h :: t -> read_for_one_v h @ (read_aux t)
  in read_aux vs
;;

(* wczytuje liste wierzcholkow *)
let rec read_vertices n =
  match n with
  | 0 -> []
  | _ ->
      match split (regexp " ") (read_line ()) with
      | [label; color] -> (V.create ~lab:label ~col:(string_to_color color) ()) :: (read_vertices (n-1))
      | _ -> failwith "Wrong input formatting!"
;;

(* wczytywanie grafu prostego *)
let load_graph () =
  (* najpierw sposob podania grafu ("list_v" (listy sasiedztwa) lub "list_e" (lista krawedzi),
     a potem l. wierzch. *)
  print_string (" Choose the way you'll define edges:\n" ^
		"   adjacency list - \"list_v\"\n" ^
                "   list od edges - \"list_e\"\n");
  let in_typ = read_line () in
  print_string " Type number of vertices:\n";
  let n = read_int () in
  print_string (" Define all vertices - in nth line give\n" ^
		" label and color of nth vertex\n");
  let vs = read_vertices n in
  let g = List.fold_left (fun g v -> G.add_v g v) (G.empty) vs in
  let es =
    if in_typ = "list_v" then
      (
	print_string (" In the nth line type numbers of neighbours\n" ^
		      " of nth vertex (vertices are numbered from 0),\n" ^
		      " each followed by edge label and color:\n");
	read_simple_edges_list_v vs
      )
    else
      (
	print_string " Type number of edges:\n";
	let m = read_int () in
	print_string (" Define edges. In nth line give numbers of two\n" ^
		      " vertices, then label and color of edge:\n");
	read_simple_edges_list_e vs m
      )
  in List.fold_left (fun g e -> G.add_e g e) g es
;;

(* wczytywanie grafu skierowanego *)
let load_directed_graph () =
  (* najpierw sposob podania grafu ("list_v" (listy sasiedztwa) lub "list_e" (lista krawedzi),
     a potem l. wierzch. *)
  print_string (" Choose the way you'll define edges:\n" ^
		"   adjacency list - \"list_v\"\n" ^
                "   list od edges - \"list_e\"\n");
  let in_typ = read_line () in
  print_string " Number of vertices:\n";
  let n = read_int () in
  print_string (" Define all vertices - in nth line give\n" ^
		" label and color of nth vertex\n");
  let vs = read_vertices n in
  let g = List.fold_left (fun g v -> DG.add_v g v) (DG.empty) vs in
  let es =
    if in_typ = "list_v" then
      (
	print_string (" In the nth line type numbers of neighbours\n" ^
		      " of nth vertex (vertices are numbered from 0),\n" ^
		      " each followed by edge label and color:\n");
	read_directed_edges_list_v vs
      )
    else
      (
	print_string " Number of edges:\n";
	let m = read_int () in
	print_string (" Define edges. In nth line give numbers of two\n" ^
		      " vertices, then label and color of edge:\n");
	read_directed_edges_list_e vs m
      )
  in
  List.fold_left (fun g e -> DG.add_e g e) g es
;;

(* uruchamia podstawowa petle programu *)
let start_program_with_input () =
  let quit = ref false in
  print_string (" Graph type:\n" ^
                "   directed - \"directed\" or \"d\"\n" ^
                "   undirected - \"undirected\" or \"u\"\n" ^
                "   to quit program - \"quit\" or \"q\"\n");
  while not !quit do
    let graph_type = read_line () in
    if graph_type = "quit" || graph_type = "q" then
      quit := true
    else if graph_type = "directed" || graph_type = "d" then
      let gd = DGD.create (load_directed_graph ()) () in
      DGD.draw gd;
      DMM.start_mouse_mode gd
    else if graph_type = "undirected" || graph_type = "u" then
      let gd = GD.create (load_graph ()) () in
      GD.draw gd;
      MM.start_mouse_mode gd
    else
      print_endline "Unknown command! Try again."
  done
;;


 (* ---------- URUCHOMIENIE ----------  *)

print_endline "        Welcome to Graph Drawer!
 Follow instructions to define and draw a graph.
 When graph is drawn you can move vertices by
 clicking and dragging them over drawing area.
 You can reset (button \"RESET\" or key \"r\")
 the drawing (put vertices back on the circle),
 define a new graph (button \"NEW GRAPH\" or key
 \"q\") or change verices filling (button \"CHANGE
 FILLING\" or key \"f\")."
;;
start_program_with_input ();;

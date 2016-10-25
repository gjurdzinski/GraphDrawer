(*
  Bibioteka do rysowania grafow.
  Grzegorz Jurdzinski, II UWr 2016
*)

(* -------------------- MODULY ABSTRAKCYJNEJ REPREZENTACJI GRAFOW -------------------- *)
module type VERTEX =
sig
  type t
  type label
  type color
  type coords

  val equal : t -> t -> bool
  val create : ?lab:label -> ?col:color -> ?coords:coords -> unit -> t
  val label : t -> label
  val color : t -> color
  val coords : t -> coords
  val set_label : t -> label -> unit
  val set_color : t -> color -> unit
  val set_coords : t -> coords -> unit
end;;

module type EDGE =
sig
  type t
  type label
  type color
  type vertex

  val equal : t -> t -> bool
  val create : vertex -> vertex -> ?lab:label -> ?col:color -> unit -> t
  val label : t -> label
  val color : t -> color
  val adj : t -> vertex * vertex
  val set_label : t -> label -> unit
  val set_color : t -> color -> unit
end;;

module type GRAPH =
sig
  (* typ reprezentacji grafu *)
  type t

  module V : VERTEX
  type vertex = V.t

  module E : EDGE with type vertex = vertex

  type edge = E.t

  (* funkcje wyszukiwania *)
  val mem_v : t -> vertex -> bool
  val mem_e : t -> edge -> bool
  val mem_e_v : t -> vertex -> vertex -> bool
  val find_e : t -> vertex -> vertex -> edge
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list
  val find_v : t -> (vertex -> bool) -> vertex list
  val find_e_f : t -> (edge -> bool) -> edge list

  (* funkcje modyfikacji *)
  val empty : t
  val add_e : t -> edge -> t
  val add_v : t -> vertex -> t
  val rem_e : t -> edge -> t
  val rem_v : t -> vertex -> t

  (* iteratory (dzialanie analogiczne do funkcji z modulu List) *)
  val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_v_2 : (vertex -> 'a -> 'b -> 'b) -> t -> 'a list -> 'b -> 'b
  val fold_e_2 : (edge -> 'a -> 'b -> 'b) -> t -> 'a list -> 'b -> 'b
  val iter_v : (vertex -> unit) -> t -> unit
  val iter_e : (edge -> unit) -> t -> unit
  val iter_v_2 : (vertex -> 'a -> unit) -> t -> 'a list -> unit
  val iter_e_2 : (edge -> 'a -> unit) -> t -> 'a list -> unit

  (* inne *)
  (* liczba wierzcholkow *)
  val num_v : t -> int
  (* liczba krawedzi *)
  val num_e : t -> int
  (* czy graf jest grafem skierowanym *)
  val directed : t -> bool
end;;

(* --------------- MODULY --------------- *)
(* szczegoly opisane przy sygnaturze *)
module Vertex : VERTEX with type label = string and type color = Graphics.color and type coords = (int * int) =
struct
  type t = { id : int; label : string ref; color : Graphics.color ref; coords : (int * int) ref }
  type label = string
  type color = Graphics.color
  type coords = (int * int)

  let num = ref 0

  let equal v w = v.id = w.id
  let create ?lab:(l="") ?col:(c=Graphics.black) ?coords:(co=(0,0)) () =
    num := !num + 1;
    { id = !num; label = ref l; color = ref c; coords = ref co }
  let label v = !(v.label)
  let color v = !(v.color)
  let coords v = !(v.coords)
  let set_label v new_l = v.label := new_l
  let set_color v new_c = v.color := new_c
  let set_coords v coords = v.coords := coords
end;;

(* szczegoly opisane przy sygnaturze *)
module Edge (V : VERTEX) : (EDGE with
				   type vertex = V.t
				 and type label = string
				 and type color = Graphics.color) =
struct
  type t = { adj : V.t * V.t; label : string ref; color : Graphics.color ref }
  type vertex = V.t
  type label = string
  type color = Graphics.color

  let equal e1 e2 = V.equal (fst e1.adj) (fst e2.adj) && V.equal (snd e1.adj) (snd e2.adj) ||
                    V.equal (fst e1.adj) (snd e2.adj) && V.equal (snd e1.adj) (fst e2.adj)
  let create u v ?lab:(l="") ?col:(c=0) () = { adj = (u,v); label = ref l; color = ref c }
  let label e = !(e.label)
  let color e = !(e.color)
  let adj e = e.adj
  let set_label e new_l = e.label := new_l
  let set_color e new_c = e.color := new_c
end;;

(* szczegoly opisane przy sygnaturze *)
module DirectedEdge (V : VERTEX) : (EDGE with
				   type vertex = V.t
				 and type label = string
				 and type color = Graphics.color) =
struct
  type t = { adj : V.t * V.t; label : string ref; color : Graphics.color ref }
  type vertex = V.t
  type label = string
  type color = Graphics.color

  let equal e1 e2 = V.equal (fst e1.adj) (fst e2.adj) && V.equal (snd e1.adj) (snd e2.adj)
  let create u v ?lab:(l="") ?col:(c=0) () = { adj = (u,v); label = ref l; color = ref c }
  let label e = !(e.label)
  let color e = !(e.color)
  let adj e = e.adj
  let set_label e new_l = e.label := new_l
  let set_color e new_c = e.color := new_c
end;;

(*
   Graf prosty - funkcje pred, succ, ... nie roznia sie od siebie, zwracaja odpowiednio listy
   wierzcholkow i krawedzi sasiadujacych z zadanym wierzcholkiem.
   Szczegoly opisane przy sygnaturze.
*)
module Graph
  (V : VERTEX)
  (E : EDGE with type vertex = V.t) (* and type label = string) *)
  : (GRAPH with module V = V and module E = E) =
struct
  module V = V
  module E = E

  type vertex = V.t
  type edge = E.t

  type t = { vertices : vertex list; edges : edge list }

  let mem_v g v = List.exists (V.equal v) g.vertices
  let mem_e g e = List.exists (E.equal e) g.edges
  let mem_e_v g v w = List.exists (fun e -> (E.adj e) = (v,w) || (E.adj e) = (w,v)) g.edges
  let find_e g v w = List.find (fun e -> (E.adj e) = (v,w) || (E.adj e) = (w,v)) g.edges
  let succ g v = List.filter (mem_e_v g v) g.vertices
  let pred = succ
  let succ_e g v = List.filter (fun e -> fst (E.adj e) = v || snd (E.adj e) = v) g.edges
  let pred_e = succ_e
  (* zwraca liste wierzcholkow spelniajacych predykat f *)
  let find_v g f = List.filter (fun v -> not (f v)) g.vertices
  let find_e_f g f = List.filter (fun e -> not (f e)) g.edges

  let empty = { vertices = [] ; edges = [] }
  let add_v g v =
    if mem_v g v then
      (
	print_endline "Warning: adding existing vertex, skipping";
	g
      )
    else
      { edges = g.edges ; vertices = v :: g.vertices }
  let add_e g e =
    if mem_e g e then
      (
	print_endline "Warning: adding existing edge, skipping";
	g
      )
    else
      let (v,w) = E.adj e in
      if not (mem_v g v && (mem_v g w)) then
	(
	  print_endline "Warning: adding edge between vertices that are not in graph, skipping";
	  g
	)
      else
        { edges = e :: g.edges ; vertices = g.vertices }
  let rem_e g e = { edges = List.filter (fun e2 -> not (E.equal e e2)) g.edges ; vertices = g.vertices }
  let rem_v g v = { edges = List.filter
                              (fun e -> let (w,u) = E.adj e in not (V.equal w v || V.equal u v))
			      g.edges ;
		    vertices = List.filter (fun w -> not (V.equal v w)) g.vertices }

  let fold_v f g a = List.fold_right f g.vertices a
  let fold_e f g a = List.fold_right f g.edges a
  let fold_v_2 f g xs b = List.fold_right2 f g.vertices xs b
  let fold_e_2 f g xs b = List.fold_right2 f g.edges xs b
  let iter_v f g = List.iter f g.vertices
  let iter_e f g = List.iter f g.edges
  let iter_v_2 f g xs = List.iter2 f g.vertices xs
  let iter_e_2 f g xs = List.iter2 f g.edges xs

  let num_v g = List.length g.vertices
  let num_e g = List.length g.edges
  let directed g = false
end;;

(*
   Graf skierowany
*)
module DirectedGraph
  (V : VERTEX)
  (E : EDGE with type vertex = V.t)
  : (GRAPH with module V = V and module E = E) =
struct
  module V = V
  module E = E

  type vertex = V.t
  type edge = E.t

  type t = { vertices : vertex list; edges : edge list }

  let mem_v g v = List.exists (V.equal v) g.vertices
  let mem_e g e = List.exists (E.equal e) g.edges
  let mem_e_v g v w = List.exists (fun e -> (E.adj e) = (v,w)) g.edges
  let find_e g v w = List.find (fun e -> (E.adj e) = (v,w)) g.edges
  let succ g v = List.filter (mem_e_v g v) g.vertices
  let pred g v = List.filter (fun w -> mem_e_v g w v) g.vertices
  let succ_e g v = List.filter (fun e -> fst (E.adj e) = v) g.edges
  let pred_e g v = List.filter (fun e -> snd (E.adj e) = v) g.edges
  (* zwraca liste wierzcholkow spelniajacych predykat f *)
  let find_v g f = List.filter (fun v -> not (f v)) g.vertices
  let find_e_f g f = List.filter (fun e -> not (f e)) g.edges

  let empty = { vertices = [] ; edges = [] }
  let add_v g v =
    if mem_v g v then
      (
	print_endline "Warning: adding existing vertex, skipping";
	g
      )
    else
      { edges = g.edges ; vertices = v :: g.vertices }
  let add_e g e =
    if mem_e g e then
      (
	print_endline "Warning: adding existing vertex, skipping";
	g
      )
    else
      let (v,w) = E.adj e in
      if not (mem_v g v && (mem_v g w)) then
        (
	  print_endline "Warning: adding edge between vertices that are not in graph, skipping";
	  g
	)
      else
        { edges = e :: g.edges ; vertices = g.vertices }
  let rem_e g e = { edges = List.filter (fun e2 -> not (E.equal e e2)) g.edges ; vertices = g.vertices }
  let rem_v g v = { edges = List.filter
                              (fun e -> let (w,u) = E.adj e in not (V.equal w v || V.equal u v))
			      g.edges ;
		    vertices = List.filter (fun w -> not (V.equal v w)) g.vertices }

  let fold_v f g a = List.fold_right f g.vertices a
  let fold_e f g a = List.fold_right f g.edges a
  let fold_v_2 f g xs b = List.fold_right2 f g.vertices xs b
  let fold_e_2 f g xs b = List.fold_right2 f g.edges xs b
  let iter_v f g = List.iter f g.vertices
  let iter_e f g = List.iter f g.edges
  let iter_v_2 f g xs = List.iter2 f g.vertices xs
  let iter_e_2 f g xs = List.iter2 f g.edges xs

  let num_v g = List.length g.vertices
  let num_e g = List.length g.edges

  let directed g = true
end;;



(* -------------------- MODULY SLUZACE DO RYSOWANIA -------------------- *)

(*
   Rozne funkcje i stale matematyczne, ktorych nie ma w ocamlu.
   Tworze osobny modol, zeby uniknac ryzyka konfliktu nazw.
*)
module Math =
struct
  let pi = 4. *. atan 1.
  let sgn x = if x > 0 then 1 else if x = 0 then 0 else -1
  let add_pair (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
end;;

module type GRAPH_DRAWING =
sig
  type t

  module V : VERTEX
  type vertex = V.t

  module E : EDGE with type vertex = vertex
  type edge = E.t

  module G : GRAPH with module V = V and module E = E
  type graph = G.t

  (* 
     tworzy wartosc typu t reprezentujaca rysunek
     v_r - promien okregu reprezentujacego wierzcholki
     r - primien okregu, na ktorym rozstawiane sa wierzcholki
     size - rozmiar okna
     filling - czy wypelniamy wierzcholki kolorem
     line_width - szerokosc rysowanych lini
     font - rozmiar czcionki
     set_coords - czy ustawiac wierzcholki na okregu na poczatku
  *)
  val create : graph -> ?v_r:int -> ?r:int -> ?size:(int * int) -> ?filling:bool
    -> ?line_width:int -> ?font:int -> ?set_coords:bool -> unit -> t

  (* rysuje zadany rysunek grafu *)
  val draw : t -> unit
  (* rysuje od nowa *)
  val redraw : t -> unit
  (* czysci obszar rysowania *)
  val clear : unit -> unit
  (* ustawia wierzcholki na okregu i rysuje graf od nowa *)
  val reset : t -> unit
  (* zamyka okno z obszarem rysowania *)
  val close_window : unit -> unit

  (* zmienia promien okregu reprezentujacego wierzcholek *)
  val change_vertex_radius : t -> int -> t
  (* zmienia rysowany graf *)
  val change_graph : t -> graph -> t
  (* zmienia wypelnianie wirzcholkow *)
  val change_filling : t -> bool -> unit
  (* zmienia szerokosc rysowanych lini *)
  val change_line_width : t -> int -> t
  (* zmienia rozmiar napisow *)
  val change_font_size : t -> int -> t
  (* zmienia rozmiar okna *)
  val change_window_size : int * int -> unit

  (* przesuwa zadany wierzcholek na zadana pozycje *)
  val move_v_to : t -> vertex -> V.coords -> unit
  (* przesuwa zadany wierzcholek o zadany wektor *)
  val move_v_by : t -> vertex -> int * int -> unit
  (* zmienia kolor wierzcholka *)
  val change_v_color : t -> vertex -> V.color -> unit
  (* zmienia kolor krawedzi *)
  val change_e_color : t -> edge -> E.color -> unit
  (* zmienia etykiete wierzcholka *)
  val change_v_label : t -> vertex -> V.label -> unit
  (* zmienia etykiete krawedzi *)
  val change_e_label : t -> edge -> E.label -> unit

  (* dodaje wierzcholej *)
  val add_v : t -> vertex -> t
  (* dodaje krawedz *)
  val add_e : t -> edge -> t
  (* usuwa wierzcholek *)
  val rem_v : t -> vertex -> t
  (* usuwa krawedz *)
  val rem_e : t -> edge -> t

  (* zwraca graf *)
  val get_graph : t -> graph
  (* zwraca promien kola, na ktorym rozstawiane sa wierzcholki *)
  val get_r : t -> int
  (* zwraca promien kola reprezentujacego wierzcholki *)
  val get_vertex_r : t -> int
  (* zwraca watosc mowiaca, czy wypelniamy wierzcholki kolorem *)
  val get_filling : t -> bool
  (* zwraca szerokosc rysowanych linii *)
  val get_line_width : t -> int
  (* zwraca rozmiar napisow *)
  val get_font : t -> int

  (* zwraca wierzcholki o zadanej etykiecie *)
  val find_v_by_label : t -> V.label -> V.t list
  (* zwraca krawedzie o zadanej etykiecie *)
  val find_e_by_label : t -> E.label -> E.t list
  (* zwraca krawedz pomiedzy zadanymi wierzcholkami *)
  val find_e_by_vertices : t -> V.t -> V.t -> E.t
end;;

module GraphDrawing
  (V : VERTEX with type coords = (int * int) and type color = Graphics.color and type label = string)
  (E : EDGE with type vertex = V.t and type color = Graphics.color and type label = string)
  (G : GRAPH with module V = V and module E = E)
  : (GRAPH_DRAWING with module V = V and module E = E and module G = G) =
struct
  module V = V
  module E = E
  module G = G

  type vertex = V.t
  type edge = E.t
  type graph = G.t

  (* 
     graph - graf
     r - promien okregu, na ktorym domyslnie rozstawiane sa wierzcholki
     vertex_r - promien okregu reprezentujacego wierzcholki
     filling - czy wierzcholki maja byc rysowane jako kola (true) czy okregi (false)
     line_width - glubosc rysowanych lini (l. pikseli)
     font - rozmiar czcionki (dotyczy etykiet)
  *)
  type t = { graph : graph; r : int; vertex_r : int; filling : bool ref; line_width : int; font : int }
  
  (* mozemy miec tylko jedne otwarte okno, wiec chce miec wartosc, do ktorej bedzie mogl
     sie odwolywac kazdy "rysunek" *)
  let window_opened = ref false
  let window_size = ref (900,700)

  (* zwraca funkcje rysujaca kolo lub okreg (w zaleznosci od tego, czy wypelniamy wirzcholki) *)
  let draw_circle gd =
    Graphics.set_line_width gd.line_width;
    if !(gd.filling) then Graphics.fill_circle else Graphics.draw_circle
  (* rysuje odcinek miedzy p1 a p2 *)
  let draw_line gd p1 p2 =
    Graphics.set_line_width gd.line_width;
    Graphics.moveto (fst p1) (snd p1);
    Graphics.lineto (fst p2) (snd p2)
  (* potrzebuje opcjonalnego argumentu, aby latwo pisac na bialo po czarnym wierzcholku
     (a nigdzie indziej tego nie potrzebuje) *)
  (* poza tym rysuje napis o lewym, dolnym rogu w zadanym punkcie *)
  let draw_string gd ?(col = Graphics.black) s p =
    Graphics.set_color col;
    Graphics.set_text_size gd.font;
    Graphics.moveto (fst p) (snd p);
    Graphics.draw_string s
  (* rysuje napis o srodku w zadanym punkcie *)
  let draw_string_center gd ?(col = Graphics.black) s c =
    Graphics.set_text_size gd.font;
    let sx, sy = Graphics.text_size s in
    draw_string gd ~col:col s (fst c - sx/2, snd c - sy/2)
  (* rysuje napis o lewym, gornym srodku w zadanym punkcie *)
  let draw_string_leftup gd ?(col = Graphics.black) s c =
    Graphics.set_text_size gd.font;
    let _, sy = Graphics.text_size s in
    draw_string gd ~col:col s (fst c, snd c - sy)
    

  (* zwraca liste wpolrzednych n punktow rozmieszczonych rownomiernie na okregu o srodku
     (xs, yc) i promieniu r *)
  let points_on_circle xc yc n r =
    let r_float = float r in
    let alpha = 2. *. Math.pi /. (float n) in
    let rec points_aux xc yc k acc =
      if k = 0 then
	acc
      else
	points_aux xc yc (k-1)
	  ((truncate (sin (float k *. alpha) *. r_float) + xc,
	   truncate (cos (float k *. alpha) *. r_float) + yc) :: acc)
    in points_aux xc yc n []
  let set_points_on_circle g xc yc rad = 
    G.iter_v_2 V.set_coords g (points_on_circle xc yc (G.num_v g) rad)

  (* dla zadanego grafu zwraca wartosc typu t opisujaca rysunek grafu. Jesli set_coords ma
     wartosc true, tu funkcja ustawi wierzcholki na okregu o promieniu r. *)
  let create g ?(v_r = 10) ?(r = -1) ?(size = (900,700)) ?(filling = false)
      ?(line_width = 2) ?(font = 12) ?(set_coords = true) () =
    let rad =
      if r = -1 then
	min (fst size / 2) (snd size / 2) - v_r - 20
      else
	r
    in
    if set_coords then set_points_on_circle g (fst size / 2) (snd size / 2) rad;
    window_size := size;
    { graph = g; r = rad; vertex_r = v_r; filling = ref filling; line_width = line_width; font = font }

  (* rysuje zadany wierzcholek *)
  let draw_vertex gd v =
    if not !window_opened then
      (
	Graphics.open_graph (" " ^ (string_of_int (fst !window_size)) ^ "x" ^ (string_of_int (snd !window_size)));
	window_opened := true
      );
    Graphics.set_color (V.color v);
    draw_circle gd (fst (V.coords v)) (snd (V.coords v)) gd.vertex_r;
    draw_string_center gd
      ~col:(if !(gd.filling) && ((V.color v) = Graphics.black || (V.color v) = Graphics.blue) then Graphics.white else Graphics.black)
      (V.label v)
      (V.coords v)

  (* rysuje zadana krawedz *)
  let draw_edge gd e =
    if not !window_opened then
      (
	Graphics.open_graph (" " ^ (string_of_int (fst !window_size)) ^ "x" ^ (string_of_int (snd !window_size)));
	window_opened := true
      );
    let v1, v2 = E.adj e in
    let v1x, v1y = V.coords v1 and v2x, v2y = V.coords v2 in
    let x = v1x - v2x and y = v1y - v2y in
    (* odl. miedzy wierzcholkami na rys. *)
    let c = sqrt (float (x * x + y * y)) in
    (* x' i y' mowia, o ile chcemy skrocic z kazdej strony rys. kraw. (aby nie nachodzil na wierzch.) *)
    let x' = truncate ( float (x * gd.vertex_r) /. c ) and y' = truncate ( float (y * gd.vertex_r) /. c ) in
    (* (p1x, p1y) i (p2x, p2y) to punkty, pomiedzy ktorymi chcemy rysowac krawedz *)
    let p1x = v1x - x' and p1y = v1y - y' and p2x = v2x + x' and p2y = v2y + y' in
    Graphics.set_color (E.color e);
    draw_line gd (p1x, p1y) (p2x, p2y);
    (* rysowanie etykiety *)
    (* wektor wyznaczony przez krawedz (taki, aby y >= -1 (-1 a nie zero, bo przy tak malym przechyle traktujemy jak poziome)) *)
    let vx, vy = if y < -1 || (-1 <= y && y <= 1 && x > 0) then (-x, -y) else (x, y) in
    let dist = gd.line_width + 2 in (* odl. poczatku napisu od krawedzi *)
    (* (sx, sy) to wektor prostopadly do kraw. o dlugosci dist *)
    let sx, sy = (float (vy * dist) /. c, float (-vx * dist) /. c) and cx, cy = ((p1x + p2x) / 2, (p1y + p2y) / 2) in
    (* jesli \ to chcemy ustalic lewo-dol, jak / to lewo-gora *)
    (if vx <= 1 then draw_string else draw_string_leftup) gd (E.label e) (truncate sx + cx, truncate sy + cy);
    (* rysowanie strzalki w przypadku grafu skierowanego *)
    if G.directed gd.graph then
      (
	let angle165 = 5. *. Math.pi /. 12. +. Math.pi /. 2. and
	    angle195 = 7. *. Math.pi /. 12. +. Math.pi /. 2. and
	    (* wektor wyznaczony przez krawedz, (x, y) jest skierowany w zla strone, a nie chce po raz n-ty zmieniac wszytkiego *)
	    xa, ya = (float (-x), float (-y)) and
	    (* dlugosc ramienia strzalki *)
	    arr_len = float (gd.line_width * 10) in
	(* wyznaczamy wektor przesuniecia i przesuwamy punkt, na ktorym konczy sie kreska, aby wiedziec gdzie konczy sie ramie strzalki *)
	let xa1, ya1 = (truncate ( (xa *. (cos angle165) -. ya *. (sin angle165)) *. arr_len /. c) + p2x,
			truncate ( (xa *. (sin angle165) +. ya *. (cos angle165)) *. arr_len /. c) + p2y) in
	Graphics.set_color (E.color e);
	draw_line gd (p2x, p2y) (xa1, ya1);
	(* tak samo *)
	let xa2, ya2 = (truncate ( (xa *. (cos angle195) -. ya *. (sin angle195)) *. arr_len /. c) + p2x,
			truncate ( (xa *. (sin angle195) +. ya *. (cos angle195)) *. arr_len /. c) + p2y) in
	Graphics.set_color (E.color e);
	draw_line gd (p2x, p2y) (xa2, ya2)
      )

  (* rysuje caly graf *)
  let draw gd =
    if not !window_opened then
      (
	Graphics.open_graph (" " ^ (string_of_int (fst !window_size)) ^ "x" ^ (string_of_int (snd !window_size)));
	window_opened := true
      );
    G.iter_e (draw_edge gd) gd.graph;
    G.iter_v (draw_vertex gd) gd.graph
(*    (* przyciski *)
    Graphics.draw_rect 0 0 100 50;
    draw_string_center gd "RESET" (50, 25);
    Graphics.draw_rect 100 0 100 50;
    draw_string_center gd "NEW GRAPH" (150, 25)*)
  
  (* czysci rysunek *)
  let clear () = if !window_opened then Graphics.clear_graph () else ()
  (* czysci i rysuje *)
  let redraw gd = clear (); draw gd
  (* czysci rysunek i ustawia wierzcholki na okregu i rysuje *)
  let reset gd =
    set_points_on_circle gd.graph (fst !window_size / 2) (snd !window_size / 2) gd.r;
    redraw gd
  (* zamyka okno *)
  let close_window () = window_opened := false; Graphics.close_graph ()

  (* zmienia promien okregu reprezentujacego na rysunku wierzcholki *)
  let change_vertex_radius gd v_r =
    let res = { graph = gd.graph; r = gd.r; vertex_r = v_r; filling = gd.filling; line_width = gd.line_width; font = gd.font } in
    if !window_opened then redraw res;
    res
  (* zmienia graf *)
  let change_graph gd g =
    let res = { graph = g; r = gd.r; vertex_r = gd.vertex_r; filling = gd.filling; line_width = gd.line_width; font = gd.font } in
    if !window_opened then redraw res;
    res
  (* zmienia sposob rysowania wierzcholkow (kolo / okrag) *)
  let change_filling gd f =
    gd.filling := f;
    if !window_opened then redraw gd
  (* zmienia grubosc rysowanych linii *)
  let change_line_width gd w =
    let res = { graph = gd.graph; r = gd.r; vertex_r = gd.vertex_r; filling = gd.filling; line_width = w; font = gd.font } in
    if !window_opened then redraw res;
    res
  (* zmienia rozmiar czcionki (dotyczy etykiet) *)
  let change_font_size gd s =
    let res = { graph = gd.graph; r = gd.r; vertex_r = gd.vertex_r; filling = gd.filling; line_width = gd.line_width; font = s } in
    if !window_opened then redraw res;
    res
  (* zmienia rozmiar okna *)
  let change_window_size size =
    window_size := size;
    if !window_opened then Graphics.resize_window (fst size) (snd size)

  let move_v_to gd v new_coords =
    V.set_coords v new_coords;
    if !window_opened then redraw gd
  let move_v_by gd v vector =
    V.set_coords v (Math.add_pair (V.coords v) vector);
    if !window_opened then redraw gd
  let change_v_color gd v col =
    V.set_color v col;
    if !window_opened then redraw gd
  let change_e_color gd e col =
    E.set_color e col;
    if !window_opened then redraw gd
  let change_v_label gd v lab =
    V.set_label v lab;
    if !window_opened then redraw gd
  let change_e_label gd e lab =
    E.set_label e lab;
    if !window_opened then redraw gd

  let add_v gd v =
    let res = { graph = (G.add_v gd.graph v); r=gd.r; vertex_r=gd.vertex_r; filling=gd.filling; line_width = gd.line_width; font = gd.font } in
    if !window_opened then redraw res;
    res
  let rem_v gd v =
    let res = { graph = (G.rem_v gd.graph v); r=gd.r; vertex_r=gd.vertex_r; filling=gd.filling; line_width = gd.line_width; font = gd.font } in
    if !window_opened then redraw res;
    res
  let add_e gd e =
    let res = { graph = (G.add_e gd.graph e); r=gd.r; vertex_r=gd.vertex_r; filling=gd.filling; line_width = gd.line_width; font = gd.font } in
    if !window_opened then redraw res;
    res
  let rem_e gd e =
    let res = { graph = (G.rem_e gd.graph e); r=gd.r; vertex_r=gd.vertex_r; filling=gd.filling; line_width = gd.line_width; font = gd.font } in
    if !window_opened then redraw res;
    res

  let get_graph gd = gd.graph
  let get_r gd = gd.r
  let get_vertex_r gd = gd.vertex_r
  let get_filling gd = !(gd.filling)
  let get_line_width gd = gd.line_width
  let get_font gd = gd.font

  let find_v_by_label gd lab = G.find_v gd.graph (fun v -> G.V.label v = lab)
  let find_e_by_label gd lab = G.find_e_f gd.graph (fun e -> G.E.label e = lab)
  let find_e_by_vertices gd v w = G.find_e gd.graph v w
end;;

(* modul dla wierzcholkow *)
module V = Vertex;;
(* modul dla krawedzi nieskierowanych *)
module E = Edge(V);;
(* modul dla krawedzi skierowanych *)
module DE = DirectedEdge(V);;
(* modul dla grafow prostych *)
module G = Graph(V)(E);;
(* modul dla grafow skierowanych *)
module DG = DirectedGraph(V)(DE);;
(* modul dla rysunkow grafow prostych *)
module GD = GraphDrawing(V)(E)(G);;
(* modul dla rysunkow grafow skierowanych *)
module DGD = GraphDrawing(V)(DE)(DG);;

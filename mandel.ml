(* Mandelbrot calculation *)
let rec mandel m x y a b i = match (m, a*.a, b*.b, a*.b, i) with
 | (_,aa,bb,_,_) when (aa +. bb) > 4.0 -> i
 | (m,_,_,_,i) when i > m -> (-1)
 | (_,aa,bb,ab,_) -> mandel m x y (aa -. bb +. x) (ab +. ab +. y) (i+1);;

(* Starter for mandebrot function *)
let mandelcalc m x y = mandel m x y x y 1;;

let mandelcalcaa m (x, y) = mandel m x y x y 1;;

let rec find_interesting x y = match (x,y) with
| (x, y) when (mandelcalc 1000 x y) > 950 -> (x, y)
| _ -> find_interesting ((Random.float 4.0)-.2.0) ((Random.float 4.0)-.2.0);;

(* Loop from one point to another in 2D space
 * by a given increment and return a list
 * of all the points *)
let loop (xlow, ylow) (xhigh, yhigh) inc =
 let rec inner_loop x y acc = match (x,y) with
 | (_,y) when y > yhigh -> acc
 | (x,_) when x > xhigh -> inner_loop xlow (y+.inc) acc
 | _ -> inner_loop (x+.inc) y ((x, y)::acc)
 in inner_loop xlow ylow [];;

(* Get the average of a list of colors (For AA) *)
let average_color colors =
 let rec average_helper l acc n = match (l, acc) with
 | ([], (0,0,0)) -> (0,0,0)
 | ([], (r,g,b)) -> ((r/n), (g/n), (b/n))
 | (((x,y,z)::xs), (r,g,b))
    -> average_helper xs (r+x,g+y,b+z) (n+1)
 in average_helper colors (0,0,0) 0;;

let get_color its = match its with
| x when x < 0 -> (0,0,0)
(*
| x when x > 768 -> ((x mod 256), (x mod 256), (x mod 256))
| x when x > 511 -> ((x mod 256), 0, 0)
*)
| x when x > 255 -> ((x mod 256), (x mod 256),(x mod 256))
| x -> ((x mod 256), (x mod 256),(x mod 256));;

(* Loop for list. average_color (List.map get_color (List.map mandelcalc)) *)
let mandelaa m x y inc aa =
 match (inc/.aa, x-.(inc/.2.0)+.((inc/.aa)/.2.0), y-.(inc/.2.0)+.((inc/.aa)/.2.0), x+.(inc/.2.0)-.((inc/.aa)/.2.0), y+.(inc/.2.0)-.((inc/.aa)/.2.0)) with
 | (dx, lx,ly, hx,hy) -> average_color (List.map get_color (List.map (mandelcalcaa m) (loop (lx,ly) (hx,hy) dx)));;

let print_color c = match c with
 | (r,g,b) -> Printf.printf "%d %d %d" r g b;;

(* This prints out an r g b value for each iteration 
let print_color_val x = match x with
| x when x < 0 -> print_string "100 0 0"
| _ -> Printf.printf "%3d %3d %3d" (x mod 255 + 1) (x mod 255 + 1) (x mod 255 + 1);;
 *)

let itermandel (xlow, ylow) (xhigh, yhigh) inc maxIters aa =
 let rec iter x y = match (x, y) with
 | (_, y) when y <= ylow -> print_string "\n"
 | (x, _) when x > xhigh -> print_string "\n"; iter xlow (y-.inc)
 (*| _ -> print_string " "; print_color (get_color (mandelcalc maxIters x y)); iter (x+.inc) y*)
 | _ -> print_string " "; print_color (mandelaa maxIters x y inc aa); iter (x+.inc) y
 (* | _ -> print_string " "; print_int (mandelcalc maxIters x y); iter (x+.inc) y *)
 in iter xlow yhigh;;

let iterstart x1 y1 x2 y2 xsize iters = match (x1, y1, x2, y2) with
| (x1, y1, x2, y2) when x1 <= x2 && y1 <= y2
    -> itermandel (x1, y1) (x2, y2) ((x2-.x1)/.xsize) iters 4.0
| (x1, y1, x2, y2) when x2 <= x1 && y1 <= y2
    -> itermandel (x2, y1) (x1, y2) ((x1-.x2)/.xsize) iters 4.0
| (x1, y1, x2, y2) when x2 <= x1 && y2 <= y1
    -> itermandel (x2, y2) (x1, y1) ((x1-.x2)/.xsize) iters 4.0
 | _ -> itermandel (x1, y2) (x2, y1) ((x2-.x1)/.xsize) iters 4.0;;

let iterstartstart xsize ysize (x, y) zoom = match (x,y,zoom,4.0/.zoom,xsize/.ysize) with
| (_,_,zoom,_,_) when zoom <= 0.0
    -> print_string "You can't have <0 zoom\n"
| (x,y,_,_,_) when x < (-2.0) || y < (-2.0) || x > 2.0 || y > 2.0
    -> print_string "Out of bounds.\n"
| (_,_,_,leng,ratio)
-> iterstart (x-.leng/.2.0) (y-.leng/.2.0/.ratio) (x+.leng/.2.0) (y+.leng/.2.0/.ratio) xsize 1000;;

(* Seed Random number generator *)
Random.self_init ();;
(* Print out PPM file *)
print_string "P3\n";;
print_string "1920 1080\n";;
(* print_string "20 20\n";; *)
print_string "255\n";;
iterstartstart 1920.0 1080.0 (find_interesting 2.0 2.0) ((Random.float 120.0)+.300.0);;
(* iterstartstart 20.0 20.0 (0.0, 0.0) 1.0;; *)
print_string "\n";;

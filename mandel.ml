open Printf;;
(*
open List;;
let foldl = fold_left;;
let foldr = fold_right;;
 *)
(* Gradient
 *  To go from R0 -> R1 in N steps
 *  R = (i * (R1-R0) / (N - 1)) + R0
 *)
(* getGradient <start_color> <end_color> steps iters *)
let getGradient (r0,g0,b0) (r1,g1,b1) n i =
    (int_of_float (float i *. (float (r1-r0) /. float (n-1))) + r0,
    int_of_float (float i *. (float (g1-g0) /. float (n-1))) + g0,
    int_of_float (float i *. (float (b1-b0) /. float (n-1))) + b0);;

let get_color its = match its with
| (-1) -> (0,0,0)
| x when x < 300 -> getGradient (0,100,255) (255,255,255) 300 x
| x when x < 600 -> getGradient (255,255,255) (255,0,0) 300 (x - 300)
| x -> getGradient (255,0,0) (255,255,0) 400 (x-600);;
(* Christmas
| x -> getGradient (0,0,255) (255,255,255) 500 (x-500);;
| x when x < 100 -> getGradient (255,255,255) (255,0,0) 100 x
| x when x < 300 -> getGradient (255,255,255) (0,255,0) 200 (x-100)
| x when x < 600 -> getGradient (255,255,255) (0,0,255) 300 (x-300)
| x -> getGradient (0,0,255) (255,255,255) 400 (x-600);;
*)
(*
| x when x < 700 -> getGradient (255,255,255) (0,100,255) 300 ((x-400) mod 300)
| x -> getGradient (0,100,255) (255,255,255) 300 ((x-700) mod 300);;
*)

(* Mandelbrot calculation *)
let rec mandel m (x, y) (a, b) i = match (m, a*.a, b*.b, a*.b, i) with
 | (_,aa,bb,_,_) when (aa +. bb) > 4.0 -> i
 | (m,_,_,_,i) when i > m -> (-1)
 | (_,aa,bb,ab,_) -> mandel m (x, y) ((aa -. bb +. x), (ab +. ab +. y)) (i+1);;

(* Starter for mandebrot function *)
let mandelcalc m x y = mandel m (x,y) (x,y) 1;;

let mandelcalcaa m (x, y) = mandel m (x, y) (x, y) 1;;

let rec find_interesting x y = match (mandelcalc 1000 x y) with
| (i) when i > 900 && i < 1000 -> (x, y)
| _ -> find_interesting ((Random.float 4.0)-.2.0) ((Random.float 4.0)-.2.0);;

let julia m (x, y) = match (0.003243, 0.636656) with
| (a,b) -> mandel m (a,b) (x, y) 1;;
(*let julia m (x, y) = mandel m ((-0.683149), (-0.306733)) (x, y) 1;;*)

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

(* Loop for list. average_color (List.map get_color (List.map mandelcalc)) *)
let mandelaa m x y inc aa =
 match (inc/.aa, x-.(inc/.2.0)+.((inc/.aa)/.2.0), y-.(inc/.2.0)+.((inc/.aa)/.2.0), x+.(inc/.2.0)-.((inc/.aa)/.2.0), y+.(inc/.2.0)-.((inc/.aa)/.2.0)) with
 | (dx, lx,ly, hx,hy) -> average_color (List.map get_color (List.map (mandelcalcaa m) (loop (lx,ly) (hx,hy) dx)));;

let print_color c = match c with
 | (r,g,b) -> Printf.printf "%d %d %d" r g b;;

let itermandel (xlow, ylow) (xhigh, yhigh) inc maxIters aa =
 let rec iter x y = match (x, y) with
 | (_, y) when y <= ylow -> print_string "\n"
 | (x, _) when x > xhigh -> print_string "\n"; iter xlow (y-.inc)
 | _ -> print_string " "; print_color (mandelaa maxIters x y inc aa); iter (x+.inc) y
 in iter xlow yhigh;;

let appendFile fname s =
    let out = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 fname in
    output_string out s;
    close_out out;;

let iterstart x1 y1 x2 y2 xsize iters aa = match (x1, y1, x2, y2) with
| (x1, y1, x2, y2) when x1 <= x2 && y1 <= y2
    -> itermandel (x1, y1) (x2, y2) ((x2-.x1)/.xsize) iters aa
| (x1, y1, x2, y2) when x2 <= x1 && y1 <= y2
    -> itermandel (x2, y1) (x1, y2) ((x1-.x2)/.xsize) iters aa
| (x1, y1, x2, y2) when x2 <= x1 && y2 <= y1
    -> itermandel (x2, y2) (x1, y1) ((x1-.x2)/.xsize) iters aa
 | _ -> itermandel (x1, y2) (x2, y1) ((x2-.x1)/.xsize) iters aa;;

let iterstartstart xsize ysize (x, y) zoom = match (x,y,zoom,4.0/.zoom,xsize/.ysize) with
| (_,_,zoom,_,_) when zoom <= 0.0
    -> print_string "You can't have <0 zoom\n"
| (x,y,_,_,_) when x < (-2.0) || y < (-2.0) || x > 2.0 || y > 2.0
    -> print_string "Out of bounds.\n"
| (_,_,_,leng,ratio)
-> appendFile "points.log" (sprintf "x:%f y:%f zoom:%f\n" x y zoom);
iterstart (x-.leng/.2.0) (y-.leng/.2.0/.ratio) (x+.leng/.2.0) (y+.leng/.2.0/.ratio) xsize 1000 4.0;;

let () =
    (* Seed Random number generator *)
    Random.self_init ();
    (* Print out PPM file *)
    print_string "P3\n";
    print_string "1920 1080\n";
    (* print_string "20 20\n";; *)
    print_string "255\n";
    iterstartstart 1920.0 1080.0 (find_interesting 2.0 2.0) ((Random.float 120.0)+.1000.0);
(*
    iterstartstart 1920.0 1080.0 (0.0, 0.0) 100.0;;
    iterstartstart 1920.0 1080.0 (-0.394372, -0.587819) 1049.741013;
*)
    (*iterstartstart 1920.0 1080.0 (-1.5018, 0.0) 2000.0;;*)
    (*iterstartstart 1920.0 1080.0 (-1.5018, 0.0) 400.0;;*)
    print_string "\n";;

(* Make stuff look nicer *)
open Printf;;
open List;;
let foldl = fold_left;;
let foldr = fold_right;;

(* To go from R0 -> R1 in N steps
 * R = (i*(R1-R0) / (N - 1)) + R0
 *)
let getGradient (r0, g0, b0) (r1, g1, b1) steps iters =
    match (steps, iters) with
    | (n, i) ->
    (int_of_float (float i *. (float (r1-r0) /. float (n-1))) + r0,
    int_of_float (float i *. (float (g1-g0) /. float (n-1))) + g0,
    int_of_float (float i *. (float (b1-b0) /. float (n-1))) + b0);;

(* Choose gradients/bands of color *)
let getColor its = match its with
| (-1) -> (0,0,0)
| x when x < 400 -> getGradient (0,100,255) (255,255,255) 400 x
| x when x < 700 -> getGradient (255,255,255) (0,100,255) 300 ((x-400) mod 300)
| x -> getGradient (0,100,255) (255,255,255) 300 ((x-700) mod 300);;

(* mandelbrot calculation *)
let rec mandelcalc m (x, y) (a, b) i = match (m, a*.a, b*.b, a*.b, i) with
| (_,aa,bb,_,_) when (aa +. bb) > 4.0 -> i
| (m,_,_,_,i) when i > m -> (-1)
| (_,aa,bb,ab,_) -> mandelcalc m (x, y) ((aa -. bb +. x), (ab +. ab +. y)) (i+1);;

let mandel m (x, y) = mandelcalc m (x,y) (x,y) 1;;

let rec find_interesting m (x, y) = match (mandel m (x,y) ) with
| (i) when i < m && (i > (m - m/10)) -> (x, y)
| _ -> find_interesting m (((Random.float 4.0) -. 2.0), ((Random.float 4.0) -. 2.0));;

let julia m (a,b) (x, y) =
    mandelcalc m (a,b) (x,y) 1;;

let loop (xl, yl) (xh, yh) inc =
 let rec inner_loop x y acc = match (x, y) with
 | (_,y) when y > yh -> acc
 | (x,_) when x > xh -> inner_loop xl (y+.inc) acc
 | _ -> inner_loop (x+.inc) y ((x,y)::acc)
 in inner_loop xl yl [];;

let average_color colors = function
    |[] -> (0,0,0)
    |xs ->
      let sum cols =
          foldl (fun ((ar,ag,ab),i) (r,g,b) -> ((ar+r,ag+g,ab+b),i+1) ) ((0,0,0), 0) cols in
      let div ((a,b,c),n) = (a/n,b/n,c/n)
      in div (sum xs);;

let print_color c = match c with
| (r,g,b) -> printf "%d %d %d" r g b;;


let append_file fname s =
    let out = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 fname
    in output_string out s;
    close_out out;;

let print_ppm xsize ysize = 
    print_string "P3\n";
    print_string ((string_of_int xsize) ^ " " ^ (string_of_int ysize) ^ "\n");
print_string "255\n";
    (*itermandel (float xsize) (float ysize) (find_interesting 2.0 2.0) ((Random.float 120.0)+.1000.0);*)
    print_string "\n";;

let () = 
    Random.self_init ();
    print_ppm 1920 1080;;
    

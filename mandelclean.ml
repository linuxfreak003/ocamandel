open Printf;;
open List;;
let foldl = fold_left;;
let foldr = fold_right;;

let getGradient (r0, g0, b0) (r1, g1, b1) steps iters =
    match (steps, iters) with
    | (n, i) ->
    (int_of_float (float i *. (float (r1-r0) /. float (n-1))) + r0,
    int_of_float (float i *. (float (g1-g0) /. float (n-1))) + g0,
    int_of_float (float i *. (float (b1-b0) /. float (n-1))) + b0);;

let getColor its = match its with
| (-1) -> (0,0,0)
| x when x < 400 -> getGradient (0,100,255) (255,255,255) 400 x
| x when x < 700 -> getGradient (255,255,255) (0,100,255) 300 ((x-400) mod 300)
| x -> getGradient (0,100,255) (255,255,255) 300 ((x-700) mod 300);;

let rec mandel m (x, y) (a, b) i = match (m, a*.a, b*.b, a*.b, i) with
| (_,aa,bb,_,_) when (aa +. bb) > 4.0 -> i
| (m,_,_,_,i) when i > m -> (-1)
| (_,aa,bb,ab,_) -> mandel m (x, y) ((aa -. bb +. x), (ab +. ab +. y)) (i+1);;

let mandelcalc m (x, y) = mandel m (x,y) (x,y) 1;;

let rec find_interesting m (x, y) = match (mandelcalc m (x,y) ) with
| (i) when i > m - m/10 && i < m -> (x, y)
| _ -> find_interesting m (((Random.float 4.0) -. 2.0), ((Random.float 4.0) -.2.0));;

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

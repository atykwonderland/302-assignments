(* Question 1 *)

let mapTree_tests =
  [
    (((fun x -> x), Empty), Empty);
    (((fun x -> x + 1), Node ( Node (Node (Node (Empty, 5, Empty), 6, Empty), 2, Node (Empty, 3, Empty)), 4, Node (Empty, 0, Node (Empty, 1, Empty)) )), Node ( Node (Node (Node (Empty, 6, Empty), 7, Empty), 3, Node (Empty, 4, Empty)), 5, Node (Empty, 1, Node (Empty, 2, Empty)) ))
  ]

let rec mapTree (f, (t: 'a tree)) =
  match t with
  | Empty -> Empty
  | Node(l,n,r) -> Node((mapTree (f, l)), (f n), (mapTree (f, r)))
;;

(* Question 2. *)

let halfint_tests =
  [
    (((fun x -> x), 1.0, -1.0, 0.001), 0.0);
    (((fun x -> x +. 1.0), 1.0, -3.0, 0.001), -1.0)
  ]

let rec halfint ((f: float -> float), (posValue : float), (negValue : float), (epsilon : float)) =
  let half = (posValue +. negValue) /. 2.0 in
  if abs_float(f(half)) < epsilon then half
  else if f(half) > 0.0 then halfint(f, half, negValue, epsilon)
  else halfint(f, posValue, half, epsilon)
;;

(* Question 3. *)

let newton_tests =
  [
    ((sin,5.0,0.0001,0.0001), 9.42477)
  ]

let rec newton ((f: float -> float),(guess:float), (epsilon:float), (dx:float)) =
  let close((x:float), (y:float), (epsilon:float)) = abs_float(x -. y) < epsilon in
  let improve((guess:float),f,(dx:float)) = guess -. ((f guess) /. (deriv(f, dx) guess)) in
  if close((f guess), 0.0, epsilon)
  then
    guess
  else
    newton (f, improve (guess, f, dx), epsilon, dx)
;;

(* Question 4. *)

let indIntegral (f, (dx:float)) = 
  fun (hi) -> integral (f, 0.0, hi, dx)
;;

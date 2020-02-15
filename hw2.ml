(* Q1a TODO: Write your own tests for the pairlists function.
         You should NOT test lists of different lengths.
*)
let pairlists_tests = [
  (* Your test cases go here. *) 
  (([], []), []);
  (([1;2;3],[4;5;6]), [(1, 4); (2, 5); (3, 6)]);
]

(* Q1a TODO: Implement pairlists. *)
let rec pairlists twolists = 
  match twolists with
  | ([],[]) -> []
  | (x::xs, y::ys) -> (x,y) :: pairlists (xs,ys)
;;

(* Q1b TODO: Write your own tests for the w_mean function.
         You should NOT test lists of different lengths.
*)

let w_mean_tests = [
  (* Your test cases go here. *) 
  (([1.0; 1.0; 1.0], [1.0; 1.0; 1.0]), 1.0);
  (([1.0; 1.0; 1.0], [0.0; 0.0; 0.0]), 0.0);
  (([-1.0; -1.0; -1.0], [1.0; 1.0; 1.0]), 1.0);
  (([1.0; 1.5; 2.5; 0.5; 1.5], [10.3; 11.7; 2.0; 5.0; 6.5]), 6.44285714285714217)
]

(* Q1b TODO: Implement w_mean. *)
let w_mean weights data = 
  sumlist (List.map (fun (x, y) -> x *. y) (pairlists (weights, data))) /. sumlist weights
;;

(* Q2 TODO: Write your own tests for the memberof function. *)
let memberof_tests = [
  (* Your test cases go here. *)
  ((0, []), false);
  ((1, [3; 2; 1]), true)
]

(* Q2 TODO: Implement memberof. *)
let rec memberof pair =
  match pair with
  |(x, []) -> false
  |(x, y::ys) -> x == y || memberof (x, ys)
;;

(* Q2 TODO: Write your own tests for the remove function. *)
let remove_tests = [
  (* Your test cases go here. *)
  ((0, []), []);
  ((1, [3; 2; 1]), [3; 2]);
  ((3, [3; 2; 1; 2; 3]), [2; 1; 2]);
  ((1, [2; 3; 4]), [2; 3; 4])
]

(* Q2 TODO: Implement remove. *)
let rec remove (item, lst) =
  match lst with
  | [] -> []
  | x::xs -> 
      if x == item then 
        remove (item, xs) 
      else
        x :: remove (item, xs)
;;

(* Q3 TODO: Write your own tests for the find_max function. *)
let find_max_tests = [
  (* Your test cases go here. *)
  ([1; 6; 3; 2; 6; 1; 7; 2; 3; 5], 7);
  ([1], 1);
  ([-2; -3; -1], -1);
]

(* Q3 TODO: Implement find_max. *)
let find_max l = 
  let rec helper l acc =
    match l with
    | [] -> acc
    | x::xs -> if x > acc then 
          helper xs x
        else 
          helper xs acc
  in
  let x::xs = l in helper l x
;;

(* Q4 TODO: Write your own tests for the selsort function. *)
let selsort_tests = [
  (* Your test cases go here. *)
  ([1; 4; 2; 5; 3; 9; 6; 8; 7], [9; 8; 7; 6; 5; 4; 3; 2; 1]);
  ([],[]);
  ([123], [123]);
  ([-5; -2; -8; 0], [0; -2; -5; -8])
]

(* Q4 TODO: Implement selsort. *)
let rec selsort l = 
  match l with
  |[] -> []
  |x -> (find_max x) :: selsort (remove ((find_max x), x))
;;

(* Question 1. *)

let common_tests = [
  (([1; 3; 2; 4; 1; 5; 6; 3],[3; 9; 8; 2; 11; 21; 3]),[3; 2]);
  (([],[]),[]);
  (([1; 2; 3; 4],[5; 6; 7; 8]),[])
]

let rec common twolists =
  match twolists with
  | ([],[]) -> []
  | ([],_) -> []
  | (_,[]) -> []
  | (x::xs,y) -> if List.mem x y 
      then x::(common (xs, (List.filter (fun a -> a != x) y)))
      else common (xs,y)
;; 

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let split_tests = [
  ([1; 3; 2; 4; 5; 6; 9; 11; 17; 13; 12],([1; 2; 5; 9; 17; 12], [3; 4; 6; 11; 13]));
  ([],([],[]));
  ([1;1],([1],[1]))
]

let rec split l =
  match l with
  | x::y::tail ->
      let a,b = split tail in
      x::a, y::b
  | x::[] -> [x],[]
  | [] -> [],[]
;;

(* Question 3 Here you implement merge. *)

let merge_tests = [
  (([1; 3; 5; 7; 9],[2; 4; 6; 8]),[1; 2; 3; 4; 5; 6; 7; 8; 9]);
  (([],[]),[]);
  (([],[1]),[1]);
  (([-4; -3; -3], [1; 1; 1; 4; 4]),[-4;-3;-3;1;1;1;4;4])
]

let rec merge twolists =
  match twolists with 
  | ([],[]) -> []
  | ([],x) -> x
  | (x,[]) -> x 
  | (x::xs, y::ys) -> if x < y 
      then x::(merge (xs,y::ys))
      else y::(merge (x::xs,ys))
;;


(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  ([10; 2; 8; 5; 1; 4; 3; 9; 7; 6], [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
  ([1; 3; 2; 4; 1; 2; 5; 3],[1; 1; 2; 2; 3; 3; 4; 5]);
  ([],[]);
  ([1],[1]);
]

let rec mergesort l =
  match l with 
  | [] -> [] 
  | [x] -> [x]
  | x::xs -> let (l1,l2) = split l in
      merge (mergesort l1, mergesort l2)
;; 

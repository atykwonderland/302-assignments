(* Q1 Polynomials TODO: Implement the following four functions *)

let multiplyPolyByTerm (Term(c,e), Poly p) =
  if p == [] then raise EmptyList
  else if c == 0.0 && e == 0 then Poly[(0.0,0)]
  else Poly (List.map (fun (a,b) -> (a*.c,b+e)) p)
;;

let rec insert (Term(c,e), Poly p) = 
  match p with
  | [] -> [(c,e)]
  | (x, y)::zs -> if e >= y then (c,e)::p
      else (x,y)::(insert (Term(c,e), Poly zs))
;;

let addTermToPoly (Term(c,e), Poly p) = 
  if p == [] then raise EmptyList 
  else if c==0.0 && e==0 then Poly p
  else
    let same = List.filter(fun (a,b) -> b == e) p in
    let nsame = List.filter(fun (a,b) -> b != e) p in
    match same with 
    | [] -> Poly (insert(Term(c,e), Poly p))
    | [(x,y)] -> Poly (insert(Term(x+.c,e), Poly nsame))
;; 

let addPolys (Poly p1, Poly p2) =
  if p1 == [] || p2 == [] then raise EmptyList 
  else 
    let rec helper (Poly p, Poly acc) = 
      match p with
      | [] -> Poly acc
      | [(x,y)] -> addTermToPoly(Term(x,y), Poly acc)
      | (x,y)::zs -> helper (Poly zs, addTermToPoly(Term(x,y), Poly acc))
    in
    helper(Poly p1, Poly p2) 
;;

let unPoly (Poly p) =
  let up x = x in
  up p
;;

let multPolys (Poly p1, Poly p2) =
  if p1 == [] || p2 == [] then raise EmptyList 
  else
    let rec helper (Poly po1, Poly po2) = 
      match po1 with
      | [(x,y)] -> multiplyPolyByTerm(Term(x,y), Poly po2)
      | (x,y)::zs -> addPolys ((multiplyPolyByTerm(Term(x,y), Poly po2)), 
                               (helper (Poly zs, Poly po2)))
    in
    helper(Poly p1, Poly p2) 
;;

(* Q2 References TODO: implement the `insert` function *) 

let rec insert comp (item: int) (list: rlist) =
  match !list with
  |None -> list:= Some { data = item; next = ref None} 
  |Some c -> if comp(item,c.data) then 
        list:= Some { data = item; next = ref (Some c)}
      else insert comp item c.next
;;
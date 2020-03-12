(* Question 1.1 *)
let rec occurCheck (v: char) (tau: typExp) : bool =
  match tau with 
  | TypInt -> false
  | TypVar c -> if (v = c) then true else false
  | Arrow(x,y) -> (occurCheck v x) || (occurCheck v y)
  | Lst(x) -> (occurCheck v x)
;;

(* Question 1.2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  match tau2 with 
  | TypInt -> TypInt
  | TypVar c when c == v -> tau1
  | TypVar c -> TypVar c
  | Arrow(x,y) -> Arrow((substitute tau1 v x),(substitute tau1 v y))
  | Lst(x) -> Lst(substitute tau1 v x)
;;


(* Question 1.3 *)
let applySubst (sigma: substitution) (tau: typExp) : typExp = 
  let sub (c, t) = substitute t c in
  List.fold_right sub sigma tau 
;;

(* Question 2 *)
let rec unify (tau1: typExp) (tau2:typExp) : substitution = 
  match tau1 with
  |TypInt ->
      (match tau2 with
       |TypInt -> []
       |TypVar x -> [x, TypInt]
       |_ -> failwith "Some message"
      )
  |TypVar x ->
      (match tau2 with
       |TypInt -> [x, TypInt]
       |TypVar x2 -> if (x=x2) then [] else [x2, TypVar x] 
       |Arrow(x2,y2) ->
           if (occurCheck x tau2) then failwith "Some message"
           else [x, Arrow (x2,y2)]
       |Lst _ ->
           if (occurCheck x tau2) then failwith "Some message"
           else [x, tau2] 
      )
  |Arrow (x,y) ->
      (match tau2 with
       |TypInt -> failwith "Some message"
       |TypVar x2 ->
           if (occurCheck x2 tau1) then failwith "Some message"
           else [x2, Arrow (x,y)]
       |Arrow(x2,y2) ->
           let subs1:substitution = (unify x x2) in
           (unify (applySubst subs1 y) (applySubst subs1 y2)) @ subs1
       |Lst _ -> failwith "Some message"
      )
  |Lst x ->
      ( match tau2 with
        |TypInt -> failwith "Some message"
        |TypVar x2 ->
            if (occurCheck x2 tau1) then failwith "Some message"
            else [x2, tau1] 
        |Arrow (_) -> failwith "Some message"
        |Lst x2 -> unify x x2 
      )
;;







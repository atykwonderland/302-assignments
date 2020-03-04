(* Question 1 *)

let makeProtectedAccount ((openingBalance: int), (password: string)) =
  let closed = ref false in
  let balance = ref openingBalance in
  let pw = ref password in
  fun ((p: string), (t: transaction)) ->
    if (closed = ref true) then
      print_string "Account closed."
    else if (!pw = p) then
      match t with
      | Withdraw(m) -> if (!balance >= m)
          then
            ((balance := !balance - m);
             (Printf.printf "The new balance is: %i." !balance))
          else
            print_string "Insufficient funds." 
      | Deposit(m) ->
          ((balance := !balance + m);
           (Printf.printf "The new balance is: %i." !balance)) 
      | CheckBalance -> (Printf.printf "The balance is: %i." !balance)
      | ChangePassword(n) -> 
          ((pw := n);
           (print_string "Password changed."))
      | Close -> 
          ((closed := true);
           (print_string "Account successfully closed."))
    else 
      print_string "Incorrect password." 
;; 
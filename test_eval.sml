open abstract_syntax
open types
open eval

fun string_of_exp (EInt n) = Int.toString n
  | string_of_exp (EBool b) = Bool.toString b
  | string_of_exp (EVar v) = v
  | string_of_exp (ELam (x, t, body)) = 
        "(fn " ^ x ^ " : " ^ types.string_of_typ t ^ " => " ^ string_of_exp body ^")"
  | string_of_exp (EApp (e1, e2)) =
        "(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | string_of_exp (EIf (e1, e2, e3)) =
        "(if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3 ^ ")"
  | string_of_exp (ECoerce (e, t)) =
        "coerce(" ^ string_of_exp e ^ ", " ^ types.string_of_typ t ^ ")"


(* some test expressions *)
val test1 = abstract_syntax.EInt 42
val test2 = abstract_syntax.EBool true
val test3 = abstract_syntax.ELam ("x", TInt, EVar "x")
val test4 = abstract_syntax.EApp (test3, EVar "y")
val test5 = abstract_syntax.EIf (EBool true, EInt 1, EInt 0)

val initial_env : eval.env = Env [("y", VInt 99)]

(* evaluating tests *)
val eval1 = eval (Env [], test1)
val eval2 = eval (Env [], test2)
val eval3 = eval (Env [], test3)
val eval4 = eval (initial_env, test4)
val eval5 = eval (Env [], test5)

(* Prints test cases and their results *)
val () = print ("Test 1: " ^ string_of_exp test1 ^ "\n")
val () = print ("Evaluates to: " ^ eval.string_of_value eval1 ^ "\n\n")

val () = print ("Test 2: " ^ string_of_exp test2 ^ "\n")
val () = print ("Evaluates to: " ^ eval.string_of_value eval2 ^ "\n\n")

val () = print ("Test 3: " ^ string_of_exp test3 ^ "\n")
val () = print ("Evaluates to: " ^ eval.string_of_value eval3 ^ "\n\n")

val () = print ("Test 4: " ^ string_of_exp test4 ^ "\n")
val () = print ("Evaluates to: " ^ eval.string_of_value eval4 ^ "\n\n")

val () = print ("Test 5: " ^ string_of_exp test5 ^ "\n")
val () = print ("Evaluates to: " ^ eval.string_of_value eval5 ^ "\n")

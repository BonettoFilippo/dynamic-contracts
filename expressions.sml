(* the abstract syntax of the source langauge *)
(* contains definitions for types and expressions *)
structure expressions : EXPRESSIONS = struct 

    (* the end keyword is important to allow recursive data types which will be important with refinement types *)
    datatype exp = 
        EInt of int
      | EBool of bool
      | EVar of string
      | ELam of string * types.typ * exp
      | EApp of exp * exp
      | ELet of string * exp * exp
      | EIf of exp * exp * exp
      | ECast of exp * types.typ
      | ECouple of exp * exp
    fun string_of_exp (EInt n) = Int.toString n
      | string_of_exp (EBool b) = Bool.toString b
      | string_of_exp (EVar x) = x 
      | string_of_exp (ELam (x, t, e)) = 
            "(fun " ^ x ^ " : " ^ types.string_of_typ t ^ " => " ^ string_of_exp e ^ ")"
      | string_of_exp (EApp (e1, e2)) = 
	  		"(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
      | string_of_exp (ELet (x, e1, e2)) = 
            "(let " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
      | string_of_exp (EIf (e1, e2, e3)) = 
            "(if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3 ^ ")"
      | string_of_exp (ECast (e, t)) =
            "(" ^ string_of_exp e ^ " : " ^ types.string_of_typ t ^ ")"
      | string_of_exp (ECouple (e1, e2)) =
            "(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"

end;
signature EXPRESSIONS = sig

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

    val string_of_exp : exp -> string
end;
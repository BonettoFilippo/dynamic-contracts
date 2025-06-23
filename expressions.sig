signature EXPRESSIONS = sig

    (* the possible expressions are type literals, variables, lambdas, applications, let, if and cast expressions *)
    datatype exp =
        EInt of int
      | EBool of bool
      | EVar of string
      | ELam of string * exp 
      | EApp of exp * exp
      | ELet of string * exp * exp
      | EIf of exp * exp * exp
      | ECouple of exp * exp

    (* a function that converts expressions into human readable strings *)
    val string_of_exp : exp -> string
end;
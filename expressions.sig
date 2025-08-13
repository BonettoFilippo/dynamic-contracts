signature EXPRESSIONS = sig

    (* the possible expressions are type literals, variables, lambdas, applications, let, if and cast expressions 
    two more instructions used mainly for testing are the Plus1 and the Negation*)
    datatype exp =
        EInt of int
      | EBool of bool
      | EVar of string
      | EPlus1 of exp
      | ENeg of exp
      | ELam of string * exp 
      | EApp of exp * exp
      | ELet of string * exp * exp
      | EIf of exp * exp * exp
      | EPair of exp * exp

    (* a function that converts expressions into human readable strings *)
    val exp_to_string : exp -> string
end;
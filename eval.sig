signature EVAL = sig

    (* exceptions for errors *)
    exception UnboundVariable of string
    exception DynamicTypeError of int * string

    (* the possible values follow the same types shown in the types module.*)
    datatype value =
        VInt of int
      | VBool of bool
      | VClosure of string * expressions.exp * ((string * value) list)
      | VPair of value * value

    (* the environment is a list of pairs of strings and values *)
    type env = (string * value) list

    (* convert a value to its type, used for type checking and coercions *)
    val value_to_type : value -> types.typ

    (* the evaluator takes an environment and an expression, and returns a value *)
    val eval : env -> expressions.exp -> value

    (* an alias to run the evaluator *)
    val run : expressions.exp -> value
end;
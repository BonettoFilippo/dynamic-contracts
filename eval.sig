signature EVAL = sig
    exception UnboundVariable of string
    exception DynamicTypeError of string

    (* the environment is a list of pairs of strings and values *)

    datatype value =
        VInt of int
      | VBool of bool
      | VClosure of string * types.typ * expressions.exp * ((string * value) list)
      | VDynamic of value
      | VError of string
      | VCouple of value * value

    type env = (string * value) list

    val value_to_type : value -> types.typ

    val eval : env -> expressions.exp -> value

    val run : expressions.exp -> value
end;
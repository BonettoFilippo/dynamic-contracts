signature EVAL_ANN = sig
(* very similar to eval, this module implements the evaluation of annotated expressions *)

    (* the possible values follow the same types shown in the types module.*)
    datatype ann_value =
            AVInt of int
          | AVBool of bool
          | AVClosure of string * constraintsyntax.ann_exp * ((string * ann_value) list)
          | AVDynamic of ann_value
          | AVNull
          | AVPair of ann_value * ann_value

    (* the environment is a list of pairs of strings and annotated values *)
    type ann_env = (string * ann_value) list
    
    (* a new exception to signal any breaking of contracts. *)
    exception DynamicTypeContractError of int * ann_env * string * exn list

    (* convert an annotated value to its type, used for type checking and coercions *)
    val ann_value_to_type : ann_value -> types.typ 

    (* the evaluator takes an environment and an expression, and returns a annotated value *)
    val eval_ann : ann_env -> constraintsyntax.ann_exp -> ann_value

    (* an alias to run the evaluator *)
    val run_ann : constraintsyntax.ann_exp -> ann_value
end;
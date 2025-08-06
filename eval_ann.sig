signature EVAL_ANN = sig

    datatype ann_value =
            AVInt of int
          | AVBool of bool
          | AVClosure of string * constraintsyntax.ann_exp * ((string * ann_value) list)
          | AVDynamic of ann_value
          | AVNull
          | AVCouple of ann_value * ann_value
    (* the environment is a list of pairs of strings and values *)
    type ann_env = (string * ann_value) list
    
    exception DynamicTypeContractError of int * ann_env * string * exn list

    val ann_value_to_type : ann_value -> types.typ 

    (* the evaluator takes an environment and an expression, and returns a value *)
    val eval_ann : ann_env -> constraintsyntax.ann_exp -> ann_value

    (* an alias to run the evaluator *)
    val run_ann : constraintsyntax.ann_exp -> ann_value
end;
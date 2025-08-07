signature CONTRACTS =
sig

    (* two new expression to handle any potential error *)
    exception ExpressionNotFound of int
    exception UnexpectedExpression of constraintsyntax.ann_exp

    (* a function that executes a program running its respective evaluator *)
    val execute : expressions.exp -> eval_ann.ann_value

    (* a helper function to get actual type of an expression at runtime *)
    val get_actual_typ : constraintsyntax.ann_exp * eval_ann.ann_env -> types.typ 

    (* a helper function to get the index of an expression *)
    val getidx : constraintsyntax.ann_exp -> int

    (* a helper function to get an expression with a specific index, given the whole program *)
    val findexp : constraintsyntax.ann_exp * int -> constraintsyntax.ann_exp

    (* the main function that handles any Dynamic type error, giving a reason for the error
       this function also proves that some instruction is the origin of the error and reports it *)
    val handle_dyn_type_error : int * constraintsyntax.ann_exp * eval_ann.ann_env * constraintsyntax.constraint list * exn list -> types.typ

end

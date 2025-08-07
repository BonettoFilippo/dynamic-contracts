signature CONTRACTS =
sig

    exception ExpressionNotFound of int
    exception UnexpectedExpression of constraintsyntax.ann_exp

    val execute : expressions.exp -> eval_ann.ann_value

    val get_actual_typ : constraintsyntax.ann_exp * eval_ann.ann_env -> types.typ 

    val getidx : constraintsyntax.ann_exp -> int

    val findexp : constraintsyntax.ann_exp * int -> constraintsyntax.ann_exp

    val handle_dyn_type_error : int * constraintsyntax.ann_exp * eval_ann.ann_env * constraintsyntax.constraint list * exn list -> types.typ

end

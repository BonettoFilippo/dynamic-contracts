signature CONTRACTS =
sig

    exception ExpressionNotFound of int
    exception UnexpectedExpression of constraintsyntax.ann_exp

    val execute : expressions.exp -> eval_ann.ann_value

    (* Function to handle dynamic type errors *)
    
    val getidx : constraintsyntax.ann_exp -> int

    val findexp : constraintsyntax.ann_exp * int -> constraintsyntax.ann_exp

    val handle_dyn_type_error : int * constraintsyntax.ann_exp * constraintsyntax.constraint list * exn list -> unit

end


structure annotatedexpr = struct
    
    datatype ann_exp = AnnExp of {
        exp = expressions.exp, 
        inner: types.typ URef.uref, 
        outer: types.typ option URef.uref};

    fun initial_inner (e: expressions.exp, env: eval.env) = 
        case e of
            expressions.EInt _ => types.TInt
          | expressions.EBool _ => types.TBool
          | expressions.EVar _ => (case List.find (fn (x, t) => x = e) env of
                                      SOME (_, t) => eval.value_to_type t
                                    | NONE => raise (eval.UnboundVariable "Unbound variable"))
          | expressions.ELam (v, t, body) => types.TFun (t, initial_inner (body, env))
          | expressions.EApp (e1, e2) => 
                case eval env e1 of
                    eval.VClosure (v, t, body, env) => 
                        let 
                            arg_val = eval env e2 
                        in
                            initial_inner body ((v, arg_val) :: env) 
                        end
                  | _ => raise (eval.DynamicTypeError "Attempted to apply a non-function")
          | expressions.ELet (x, e1, e2) => 
                let 
                    val v1 = eval env e1
                in
                    initial_inner e2 ((x, v1) :: env)
                end
          | expressions.EIf (cond, e_then, e_else) =>
                let
                    val vthen = initial_inner e_then env
                    val velse = initial_inner e_else env
                in
                    if vthen = velse then vthen
                    else raise (eval.DynamicTypeError "If branches have different types")
                end
          | expressions.ECast (_, t) => t
          | expressions.ECouple (_, _) => ECouple (initial_inner e1 env, initial_inner e2 env)

    fun annotate (env: eval.env) (e: expressions.exp) : ann_exp = 
        AnnExp {exp = e, inner = URef.mkURef (initial_inner (e, env)), outer = URef.mkURef NONE}
end;

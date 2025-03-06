(* interpreter and evaluatior for the syntax *)

open abstract_syntax

structure eval = struct
    
    datatype env = Env of (string * value) list
    and value =
          VInt of int
        | VBool of bool
        | VClosure of string * typ * abstract_syntax.exp * env

    fun fromEnv (Env lst) = lst

    fun lookup (x:string, Env []) =
            raise Fail ("Unbound variable: " ^ x)
      | lookup (x, Env ((y, v)::rest)) = 
            if x = y then v else lookup (x, Env rest)

    fun eval (env: env, e: abstract_syntax.exp) : value =
            case e of 
                abstract_syntax.EInt n => VInt n
              | abstract_syntax.EBool b => VBool b
              | abstract_syntax.EVar v => lookup (v, env)
              | abstract_syntax.ELam (x, t, body) => VClosure (x, t, body, env)
              | abstract_syntax.EApp (e1, e2) =>
                    let 
                        val v1 = eval (env, e1)
                        val v2 = eval (env, e2)
                    in 
                        case v1 of
                            VClosure (x, t1, body, cloEnv) =>
                                eval (Env ((x, v2) :: (fromEnv cloEnv)), body)
                          | _ => raise Fail "Attempted to apply a non-function"
                    end
              | abstract_syntax.EIf (cond, e_then, e_else) =>
                    let 
                        val vcond = eval (env, cond)
                    in
                        case vcond of 
                            VBool true => eval (env, e_then)
                          | VBool false => eval (env, e_else)
                          | _ => raise Fail "Condition is not a boolean"
                    end
              | abstract_syntax.ECoerce (e_inner, t1) =>
                    eval (env, e_inner) (* TODO a stub for coercions *)

    fun string_of_value (VInt n) = Int.toString n
      | string_of_value (VBool b) = Bool.toString b
      | string_of_value (VClosure (_, _, _, _)) = "<closure>"

end;
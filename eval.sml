(* interpreter and evaluatior for the syntax *)

structure eval = struct
    
    (* exceptions for errors *)
    exception UnboundVariable of string
    exception DynamicTypeError of string

    (* the environment is a list of pairs of strings and values *)

    datatype value =
        VInt of int
      | VBool of bool
      | VClosure of string * types.typ * expressions.exp * ((string * value) list)
      | VDynamic of value
      | VError of string

    type env = (string * value) list

    fun eval (env: env) (e: expressions.exp) : value =
        case e of 
            expressions.EVar x => 
                (case List.find (fn (y, _) => x = y) env of
                    SOME (_, v) => v
                  | NONE => raise (UnboundVariable x))
          | expressions.EInt n => VInt n
          | expressions.EBool b => VBool b
          | expressions.ELam (x, t, body) => VClosure (x, t, body, env)
          | expressions.EApp (e1, e2) =>
                let
                    val v1 = eval env e1
                    val v2 = eval env e2
                in
                    (case v1 of
                        VClosure (x, t1, body, cloEnv) =>
                            eval ((x, v2) :: cloEnv) body
                    | _ => raise DynamicTypeError "Attempted to apply a non-function")
                end
          | expressions.EIf (cond, e_then, e_else) =>
                let
                    val vcond = eval env cond
                in
                    (case vcond of
                        VBool true => eval env e_then
                    | VBool false => eval env e_else
                    | _ => raise DynamicTypeError "Condition is not a boolean")
                end
          | expressions.ELet (x, e1, e2) =>
                let
                    val v1 = eval env e1
                in
                    eval ((x, v1) :: env) e2
                end
          | expressions.ECast (e_inner, t1) =>
                eval env e_inner (* TODO a stub for coercions *)

    fun run e = eval [] e
end;
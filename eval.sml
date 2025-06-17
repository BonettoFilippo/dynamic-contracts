(* interpreter and evaluator for the syntax *)

structure eval : EVAL = struct
    
    (* exceptions for errors *)
    exception UnboundVariable of string
    exception DynamicTypeError of string
   
    (* the possible values follow the same types shown in the types module. it also incluedes the error value to express exeptions*)
    datatype value =
        VInt of int
      | VBool of bool
      | VClosure of string * types.typ * expressions.exp * ((string * value) list)
      | VDynamic of value
      | VError of string
      | VCouple of value * value

    (* the environment is a list of pairs of strings and values *)
    type env = (string * value) list

    (* convert a value to its type, used for type checking and coercions *)
    fun value_to_type (v: value) : types.typ =
        case v of
            VInt _ => types.TInt
          | VBool _ => types.TBool
          | VClosure (_, t, _, _) => t
          | VDynamic v => types.TDyn
          | VError _ => raise (DynamicTypeError "Error value has no type")
          | VCouple (v1, v2) => types.TCouple (value_to_type v1, value_to_type v2)

    (* the evaluator takes an environment and an expression, and returns a value *)
    (* recurses the syntax tree up to the leaves to solve inner nodes*)
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
          | expressions.ECouple (e1, e2) =>
                VCouple (eval env e1, eval env e2)

    (* an alias to run the evaluator *)
    fun run e = eval [] e
end;
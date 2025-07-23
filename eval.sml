(* interpreter and evaluator for the syntax *)

structure eval : EVAL = struct
    
    (* exceptions for errors *)
    exception UnboundVariable of string
    exception DynamicTypeError of string
   
    (* the possible values follow the same types shown in the types module. it also incluedes the error value to express exeptions*)
    datatype value =
        VInt of int
      | VBool of bool
      | VClosure of string * expressions.exp * ((string * value) list)
      | VDynamic of value
      | VNull
      | VCouple of value * value

    (* the environment is a list of pairs of strings and values *)
    type env = (string * value) list

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
          | expressions.EPlus1 e1 =>
                let
                    val v1 = eval env e1
                in
                    (case v1 of
                        VInt n => VInt (n + 1)
                      | _ => raise DynamicTypeError 0 "Expected an integer for +1")
                end
          | expressions.ENeg e1 =>
                let
                    val v1 = eval env e1
                in
                    (case v1 of
                        VBool b => VBool (not b)
                      | _ => raise DynamicTypeError 0 "Expected a boolean for negation")
                end
          | expressions.ELam (x, body) => VClosure (x, body, env)
          | expressions.EApp (e1, e2) =>
                let
                    val v1 = eval env e1
                    val v2 = eval env e2
                in
                    (case v1 of
                        VClosure (x, body, cloEnv) =>
                            eval ((x, v2) :: cloEnv) body
                      | _ => raise DynamicTypeError 0 "Attempted to apply a non-function")
                end
          | expressions.EIf (cond, e_then, e_else) =>
                let
                    val vcond = eval env cond
                in
                    (case vcond of
                        VBool true => eval env e_then
                      | VBool false => eval env e_else
                      | _ => raise DynamicTypeError 0 "Condition is not a boolean")
                end
          | expressions.ELet (x, e1, e2) =>
                let
                    val v1 = eval env e1
                in
                    eval ((x, v1) :: env) e2
                end
          | expressions.ECouple (e1, e2) =>
                VCouple (eval env e1, eval env e2)

    (* convert a value to its type, used for type checking and coercions *)
    fun value_to_type (v: value) : types.typ =
        case v of
            VInt _ => types.TInt
          | VBool _ => types.TBool
          | VClosure (input, output, env) => 
                let
                    val inputType =     
                        (case List.find (fn (x, _) => x = input) env of
                            SOME (_, v) => v
                          | NONE => VDynamic (VInt 0)) 
                    val outputType = value_to_type (eval ((input, inputType) :: env) output)
                in
                    types.TFun ((value_to_type inputType), outputType)
                end
          | VDynamic v => types.TDyn
          | VNull => types.TNull
          | VCouple (v1, v2) => types.TCouple (value_to_type v1, value_to_type v2)


    (* an alias to run the evaluator *)
    fun run e = eval [] e
end;
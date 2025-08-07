(* interpreter and evaluator for the syntax *)

structure eval_ann : EVAL_ANN = struct

    (* the possible values follow the same types shown in the types module.*)
    datatype ann_value =
            AVInt of int
          | AVBool of bool
          | AVClosure of string * constraintsyntax.ann_exp * ((string * ann_value) list)
          | AVDynamic of ann_value
          | AVNull
          | AVCouple of ann_value * ann_value

    (* the environment is a list of pairs of strings and annotated values *)
    type ann_env = (string * ann_value) list

    exception DynamicTypeContractError of int * ann_env * string * exn list

    (* the evaluator takes an environment and an expression, and returns a value *)
    (* recurses the syntax tree up to the leaves to solve inner nodes*)
    fun eval_ann (env: ann_env) (e: constraintsyntax.ann_exp) : ann_value =
        case e of 
            constraintsyntax.AVar (x, _, _, _) => 
                (case List.find (fn (y, _) => x = y) env of
                    SOME (_, v) => v
                  | NONE => raise (eval.UnboundVariable x))
          | constraintsyntax.AInt (n, _, _, _) => AVInt n
          | constraintsyntax.ABool (b, _, _, _) => AVBool b
          | constraintsyntax.APlus1 (e1, _, _, idx) =>
                let
                    val v1 = eval_ann env e1
                in
                    (case v1 of
                        AVInt n => AVInt (n + 1)
                      | _ => raise eval.DynamicTypeError (idx, "Expected an integer for +1"))
                end
          | constraintsyntax.ANeg (e1, _, _, idx) =>
                let
                    val v1 = eval_ann env e1
                in
                    (case v1 of
                        AVBool b => AVBool (not b)
                      | _ => raise eval.DynamicTypeError (idx, "Expected a boolean for negation"))
                end
          | constraintsyntax.ALam (x, body, _, _, _) => 
                AVClosure (x, body, env)
          | constraintsyntax.AApp (e1, e2, _, _, idx) =>
                (let
                    val v1 = eval_ann env e1
                    val v2 = eval_ann env e2
                in
                    (case v1 of
                        AVClosure (x, body, cloEnv) =>
                            eval_ann ((x, v2) :: cloEnv) body
                      | _ => raise eval.DynamicTypeError (idx, "Attempted to apply a non-function"))
                end handle
                    eval.DynamicTypeError (id, msg) =>
                        let 
                            val _ = raise DynamicTypeContractError (idx, env, "Contract error, need to assign blame", [eval.DynamicTypeError (id, msg)])
                        in 
                            AVNull
                        end
                  | DynamicTypeContractError (id, env', msg, ex) =>
                        let 
                            val _ = raise DynamicTypeContractError (idx, env, "Contract error, need to assign blame", DynamicTypeContractError (id, env', msg, ex)::ex)
                        in 
                            AVNull
                        end
                  | e => 
                        let
                            val _ = raise e 
                        in 
                            AVNull
                        end )
          | constraintsyntax.AIf (cond, e_then, e_else, _, _, idx) =>
                let
                    val vcond = eval_ann env cond
                in
                    (case vcond of
                        AVBool true => eval_ann env e_then
                      | AVBool false => eval_ann env e_else
                      | _ => raise eval.DynamicTypeError (idx, "Condition is not a boolean"))
                end
          | constraintsyntax.ALet (x, e1, e2, _, _, _) =>
                let
                    val v1 = eval_ann env e1
                in
                    eval_ann ((x, v1) :: env) e2
                end
          | constraintsyntax.ACouple (e1, e2, _, _, _) =>
                AVCouple (eval_ann env e1, eval_ann env e2)

    
    (* convert a annotated value to its type, used for type checking and coercions *)
    fun ann_value_to_type (v: ann_value) : types.typ =
        case v of
            AVInt _ => types.TInt
          | AVBool _ => types.TBool
          | AVClosure (input, output, env) => 
                let
                    val inputType =     
                        (case List.find (fn (x, _) => x = input) env of
                            SOME (_, v) => v
                          | NONE => AVDynamic (AVInt 0)) 
                    val outputType = ann_value_to_type (eval_ann ((input, inputType) :: env) output)
                in
                    types.TFun ((ann_value_to_type inputType), outputType)
                end
          | AVDynamic v => types.TDyn
          | AVNull => types.TNull
          | AVCouple (v1, v2) => types.TCouple (ann_value_to_type v1, ann_value_to_type v2)

    (* an alias to run the evaluator *)
    fun run_ann e = eval_ann [] e
end;
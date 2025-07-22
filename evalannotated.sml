(* interpreter and evaluator for the syntax *)

structure eval_ann : EVAL_ANN = struct

    datatype ann_value =
            AVInt of int
          | AVBool of bool
          | AVClosure of string * constraintsyntax.ann_exp * ((string * ann_value) list)
          | AVDynamic of ann_value
          | AVNull
          | AVCouple of ann_value * ann_value

    (* the environment is a list of pairs of strings and values *)
    type ann_env = (string * ann_value) list

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
          | constraintsyntax.APlus1 (e1, _, _, _) =>
                let
                    val v1 = eval_ann env e1
                in
                    (case v1 of
                        AVInt n => AVInt (n + 1)
                      | _ => raise eval.DynamicTypeError "Expected an integer for +1")
                end
          | constraintsyntax.ANeg (e1, _, _, _) =>
                let
                    val v1 = eval_ann env e1
                in
                    (case v1 of
                        AVBool b => AVBool (not b)
                      | _ => raise eval.DynamicTypeError "Expected a boolean for negation")
                end
          | constraintsyntax.ALam (x, body, _, _, _) => 
                AVClosure (x, body, env)
          | constraintsyntax.AApp (e1, e2, _, _, _) =>
                let
                    val v1 = eval_ann env e1
                    val v2 = eval_ann env e2
                in
                    (case v1 of
                        AVClosure (x, body, cloEnv) =>
                            eval_ann ((x, v2) :: cloEnv) body
                      | _ => raise eval.DynamicTypeError "Attempted to apply a non-function")
                end
          | constraintsyntax.AIf (cond, e_then, e_else, _, _, _) =>
                let
                    val vcond = eval_ann env cond
                in
                    (case vcond of
                        AVBool true => eval_ann env e_then
                      | AVBool false => eval_ann env e_else
                      | _ => raise eval.DynamicTypeError "Condition is not a boolean")
                end
          | constraintsyntax.ALet (x, e1, e2, _, _, _) =>
                let
                    val v1 = eval_ann env e1
                in
                    eval_ann ((x, v1) :: env) e2
                end
          | constraintsyntax.ACouple (e1, e2, _, _, _) =>
                AVCouple (eval_ann env e1, eval_ann env e2)

    (* an alias to run the evaluator *)
    fun run_ann e = eval_ann [] e
end;
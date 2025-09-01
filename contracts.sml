structure contracts : CONTRACTS = struct

    (* two new exceptions to handle edgecases *)
    exception ExpressionNotFound of int
    exception UnexpectedExpression of constraintsyntax.ann_exp

    fun list_in_range (lst: int list, idx: int, idx2: int) : bool =
        case lst of
            [] => false
          | h :: t => if h >= idx andalso h <= idx2 then true else list_in_range (t, idx, idx2)
        
    fun get_outer_type (exp: constraintsyntax.ann_exp) : (types.typ * int list) =
        case exp of
            constraintsyntax.AInt (_, _, tvar, _) => URef.!! tvar
          | constraintsyntax.ABool (_, _, tvar, _) => URef.!! tvar
          | constraintsyntax.AVar (_, _, tvar, _) => URef.!! tvar
          | constraintsyntax.APlus1 (_, _, tvar, _) => URef.!! tvar
          | constraintsyntax.ANeg (_, _, tvar, _) => URef.!! tvar
          | constraintsyntax.ALam (_, _, _, tvar, _) => URef.!! tvar
          | constraintsyntax.AApp (_, _, _, tvar, _) => URef.!! tvar
          | constraintsyntax.ALet (_, _, _, _, tvar, _) => URef.!! tvar
          | constraintsyntax.AIf (_, _, _, _, tvar, _) => URef.!! tvar
          | constraintsyntax.APair (_, _, _, tvar, _) => URef.!! tvar

    (* a function that give a annotated expression, returns its index *)
    fun getidx (exp: constraintsyntax.ann_exp) : int =
        case exp of
            constraintsyntax.AInt (_, _, _, idx) => idx
          | constraintsyntax.ABool (_, _, _, idx) => idx
          | constraintsyntax.AVar (_, _, _, idx) => idx
          | constraintsyntax.APlus1 (_, _, _, idx) => idx
          | constraintsyntax.ANeg (_, _, _, idx) => idx
          | constraintsyntax.ALam (_, _, _, _, idx) => idx
          | constraintsyntax.AApp (_, _, _, _, idx) => idx
          | constraintsyntax.AIf (_, _, _, _, _, idx) => idx
          | constraintsyntax.ALet (_, _, _, _, _, idx) => idx
          | constraintsyntax.APair (_, _, _, _, idx) => idx

    (* a helper function to get an expression with a specific index, given the whole program *)
    (* this function is recursive and will raise an exception if the expression is not found *)
    fun findexp (exp: constraintsyntax.ann_exp, inx: int): constraintsyntax.ann_exp =
        case exp of
            constraintsyntax.AInt (n, _, _, idx) => 
                if idx = inx then exp else raise ExpressionNotFound inx
          | constraintsyntax.ABool (b, _, _, idx) => 
                if idx = inx then exp else raise ExpressionNotFound inx
          | constraintsyntax.AVar (x, _, _, idx) =>
                if idx = inx then exp else raise ExpressionNotFound inx
          | constraintsyntax.APlus1 (e1, _, _, idx) =>
                if idx = inx then exp else findexp (e1, inx)
          | constraintsyntax.ANeg (e1, _, _, idx) =>
                if idx = inx then exp else findexp (e1, inx)
          | constraintsyntax.ALam (x, body, _, _, idx) =>
                if idx = inx then exp else findexp (body, inx)
          | constraintsyntax.AApp (e1, e2, _, _, idx) =>
                if idx = inx then exp else
                    let 
                        val idx2 = getidx e2
                    in
                        if 
                            inx < idx2 
                        then
                            findexp (e1, inx)
                        else
                            findexp (e2, inx)
                    end
          | constraintsyntax.ALet (x, e1, e2, _, _, idx) =>
                if idx = inx then exp else
                    let 
                        val idx2 = getidx e2
                    in
                        if 
                            inx < idx2 
                        then
                            findexp (e1, inx)
                        else
                            findexp (e2, inx)
                    end
          | constraintsyntax.AIf (cond, e_then, e_else, _, _, idx) =>
                if idx = inx then exp else 
                    let 
                        val idx2 = getidx e_then
                        val idx3 = getidx e_else
                    in
                        if 
                            inx < idx2 
                        then
                            findexp (cond, inx)
                        else if 
                            inx < idx3 
                        then
                            findexp (e_then, inx)
                        else
                            findexp (e_else, inx)
                    end
          | constraintsyntax.APair (e1, e2, _, _, idx) =>
                if idx = inx then exp else
                    let 
                        val idx2 = getidx e2
                    in
                        if 
                            inx < idx2 
                        then
                            findexp (e1, inx)
                        else
                            findexp (e2, inx)
                    end

    (* 
        get_actual_typ:
        Given an annotated expression and an environment, this function recursively determines the actual type of the expression at runtime.
        - For literals (integers, booleans), it returns the corresponding type.
        - For variables, it looks up the variable in the environment and returns its type.
        - For unary and binary operations, it returns the expected result type.
        - For lambda abstractions, it constructs a function type from the input variable's type (looked up in the environment) to the type of the body.
            lambdas return the correct type only if it is called by an application
        - For function applications, it evaluates the function and input, then determines the result type by extending the environment with the input.
        - For conditionals, it evaluates the condition and returns the type of the then-branch or else-branch accordingly.
        - For let-bindings, it extends the environment with the bound variable and evaluates the body.
        - For pairs, it returns a tuple type of the two sub-expressions.
        The function may raise exceptions if variables are unbound or if dynamic type errors occur *)
    fun get_actual_typ (exp: constraintsyntax.ann_exp, env: eval_ann.ann_env) : types.typ =
        case exp of
            constraintsyntax.AInt (_, _, _, _) => types.TInt
          | constraintsyntax.ABool (_, _, _, _) => types.TBool
          | constraintsyntax.AVar (x, _, _, _) => 
                (case List.find (fn (y, _) => x = y) env of
                    SOME (_, v) => eval_ann.ann_value_to_type v
                  | NONE => raise (eval.UnboundVariable x))
          | constraintsyntax.APlus1 (_, _, _, _) => types.TInt
          | constraintsyntax.ANeg (_, _, _, _) => types.TBool
          | constraintsyntax.ALam (s, b, _, _, _) => 
                let
                    val inp = 
                    (case List.find (fn (y, _) => s = y) env of
                        SOME (_, v) => eval_ann.ann_value_to_type v
                      | NONE => types.TDyn)
                    val out = get_actual_typ (b, env)
                in
                    types.TFun (inp, out) 
                end
          | constraintsyntax.AApp (f, inp, _, _, i) => 
                let
                    val f' = eval_ann.eval_ann env f
                    val n = (case f' of 
                        eval_ann.AVClosure (x, _, _) => x
                      | _ => raise eval.DynamicTypeError (i, "Expected a function for application"))
                    val inp' = eval_ann.eval_ann env inp
                    val ft = get_actual_typ (f, ((n, inp') :: env))
                in
                    case ft of
                        types.TFun (t1, t2) => t2
                      | _ => raise eval.DynamicTypeError (i, "Expected a function type for application, got " ^ types.typ_to_string ft ^ ".")
                end
          | constraintsyntax.AIf (c, t, e, _, _, _) => 
                let
                    val cond_type = eval_ann.run_ann c
                in
                    case cond_type of
                        eval_ann.AVBool true => 
                            get_actual_typ (t, env)
                      | _ => 
                            get_actual_typ (e, env)
                end
          | constraintsyntax.ALet (s, e1, e2, _, _, _) => get_actual_typ (e2, ((s, eval_ann.run_ann e1) :: env))
          | constraintsyntax.APair (e1, e2, _, _, _) => 
                let 
                    val t1 = get_actual_typ (e1, env)
                    val t2 = get_actual_typ (e2, env)
                in
                    types.TPair (t1, t2)
                end


    fun add_alias (name: string, aliases: string list) =
        if List.exists (fn x => x = name) aliases then aliases else name :: aliases
    
    fun remove_alias (name: string, aliases: string list) =
        List.filter (fn x => x <> name) aliases

    fun count_flows (exp: constraintsyntax.ann_exp, aliases: string list, fault_idx: int, env: eval_ann.ann_env) : int =
        case exp of
            constraintsyntax.AInt (_, _, _, idx) => 0
          | constraintsyntax.ABool (_, _, _, idx) => 0
          | constraintsyntax.AVar (x, _, _, idx) => 0
          | constraintsyntax.APlus1 (e1, _, _, idx) => 
                if idx <= fault_idx then 0 else count_flows (e1, aliases, fault_idx, env)
          | constraintsyntax.ANeg (e1, _, _, idx) => 
                if idx <= fault_idx then 0 else count_flows (e1, aliases, fault_idx, env)
          | constraintsyntax.ALam (x, body, _, _, idx) => 
                if idx <= fault_idx then 0 else count_flows (body, remove_alias (x, aliases), fault_idx, env)
          | constraintsyntax.AApp (e1, e2, _, _, idx) => 
                let 
                    val n = (case (eval_ann.eval_ann_to_var env e1) of
                                SOME v => if aliases = add_alias (v, aliases) then 1 else 0
                              | NONE => 0)
                in 
                    if idx = fault_idx then n
                    else 
                        let 
                            val idx2 = getidx e2
                        in
                            if 
                                fault_idx < idx2 
                            then
                                n + count_flows (e1, aliases, fault_idx, env)
                            else
                                let 
                                    val v = (let
                                                val v1 = eval_ann.eval_ann env e1
                                            in
                                                (case v1 of
                                                    eval_ann.AVClosure (x, body, cloEnv) => x
                                                  | _ => raise eval.DynamicTypeError (idx, "Attempted to apply a non-function"))
                                            end)
                                in
                                    n + count_flows (e2, aliases, fault_idx, env)
                                end
                        end
                end
          | constraintsyntax.ALet (x, e1, e2, _, _, idx) => 
                if idx <= fault_idx then 0 else 
                    let
                        val idx2 = getidx e2 
                        val new_aliases = (
                            case eval_ann.eval_ann_to_var env e1 of
                                SOME v => if aliases = add_alias (v, aliases) then add_alias (x, aliases) else remove_alias (x, aliases)
                              | NONE => aliases)
                    in
                        if fault_idx < idx2 then
                            count_flows (e1, new_aliases, fault_idx, (x, eval_ann.eval_ann env e1)::env)
                        else 
                            count_flows (e2, new_aliases, fault_idx, (x, eval_ann.eval_ann env e1)::env)
                    end
          | constraintsyntax.AIf (cond, e_then, e_else, _, _, idx) => 
                if idx <= fault_idx then 0 else 
                    let 
                        val idx2 = getidx e_then
                        val idx3 = getidx e_else
                    in
                        if 
                            fault_idx < idx2 
                        then
                            count_flows (cond, aliases, fault_idx, env)
                        else if 
                            fault_idx < idx3 
                        then
                            count_flows (e_then, aliases, fault_idx, env)
                        else
                            count_flows (e_else, aliases, fault_idx, env)
                    end
          | constraintsyntax.APair (e1, e2, _, _, idx) => 
                if idx <= fault_idx then 0 else 
                    let 
                        val idx2 = getidx e2
                    in
                        if 
                            fault_idx < idx2 
                        then
                            count_flows (e1, aliases, fault_idx, env)
                        else
                            count_flows (e2, aliases, fault_idx, env)
                    end
        (*
        handle_dyn_type_error:
        This function analyzes dynamic type errors that occur during evaluation of annotated expressions, 
        and attempts to provide detailed, actionable error messages that help the user identify whether the error originated 
            - in the caller (the code providing an argument) 
            - or the callee (the code implementing a function).

        - Given an error index, the full annotated expression, the current environment, the list of constraints, and a list of exceptions, it locates the expression at the error site.
        - For simple operations like APlus1 and ANeg, it assumes the error is always in the caller (the value passed is of the wrong type), and tries to rerun the operation with a correct value to confirm this. If the error persists, it reports that even a correct value does not fix the problem.
        - For function applications (AApp), it distinguishes between errors in the input (caller) and errors in the function body (callee):
            - It computes the expected input and output types using get_actual_typ.
            - It compares the actual types of the input and output to the expected types.
            - If the input type is wrong, it tries rerunning the application with a default value of the correct type. If this fixes the error, the blame is on the caller; if not, it may be on the callee.
            - If the output type is wrong, it tries rerunning with a function that produces the correct output type. If this fixes the error, the blame is on the callee.
            - If both input and output types are wrong, it tries both fixes and reports accordingly.
            - If errors are nested (e.g., the input itself causes a dynamic type error), it recursively analyzes the inner error to provide a chain of blame.
        - For conditionals (AIf), it checks if the condition is of the wrong type, and tries rerunning with a boolean condition to see if the error is resolved.
        - In all cases, the function constructs error messages that include the line (index) of the error, the actual and expected types, and the origin of the problematic value (using URef indices).
        - If the error cannot be classified, it raises an UnexpectedExpression exception this final option should never occur.

        The goal is to help the user understand not just where a dynamic type error occurred, but also whether it was caused by the value provided to an operation (caller) or by the implementation of a function (callee), and to suggest how to fix it.
        Moreover this is the function responsible to handle contracts, generating and enforcing them assigning blame *)
    fun handle_dyn_type_error (idx: int, exp: constraintsyntax.ann_exp, env: eval_ann.ann_env, idx_e: int) =
        let 
            val e = findexp (exp, idx)
        in 
            case e of
                constraintsyntax.AApp (f, v, t1, t2, _) => 
                    let
                        val f' = eval_ann.eval_ann env f
                        val (v, b) = (case f' of 
                            eval_ann.AVClosure (x, body, _) => (x, body)
                          | _ => raise eval.DynamicTypeError (idx, "Attempted to apply a non-function"))
                        val n = count_flows (b, [v], idx_e, env)
                    in
                        if n mod 2 = 0 then
                            raise eval.DynamicTypeError (idx, "the blame is on the callee (function)")
                        else
                            raise eval.DynamicTypeError (idx, "the blame is on the caller (argument)")
                        end

                    (* the function i wrote above (get_actual_typ) ignores any errors. there is a need to check if any errors come up in the input element of the function
                        it should be fairly easy to find them as we can check the idx of the any other error in the list, and check if it is between the execution of the input and the execution of the body *)
                    (* questa sezione di codice deve:
                        generare il contratto
                            PER GENERARE IL CONTRATTO USARE get_actual_typ SU V E SU T1 (DOPO AVER ESTESO L'ENV USANDO IL VALORE DI T1)
                            E' IMPORTANTE ANCHE CONTROLLARE CATCHARE ERRORI
                            CONTROLLARE SE CI SONO ERRORI CHE VENGONO GENERATI IN QUELLA SEZIONE DEL CODICE
                            vedere il tipo di partenza
                            vedere il potenziale tipo di ritorno
                        farlo combaciare con il contratto gia esistente
                        GIA DA QUI E' POSSIBILE VEDERE DOVE CI POSSONO ESSERERE PROBLEMI

                        determinare se il problema e stato generato prima o dopo l'esecuzione (dal chiamante o dal chiamato)
                            se il problema é prima, provare a modificare l'input per sottostare al contratto e dimostrare che non ci sono errori
                            se il problema é dopo 
                                provare a cambiare l'interno per sottostare al contratto
                                se non ci sono errori allora il contratto é infranto
                                    notifica all'utente che il contratto é infranto
                                    FARE UN CONTRATTO ALLA VOLTA, FARE PIU ESECUZIONI E' OK
                                    ricorrere sul prossimo errore della lista per controllare altri eventuali contratti *)
              | _ => raise UnexpectedExpression e
        end

    (*
        execute:
        This function serves as the main entry point for evaluating a source expression with contract and dynamic type error handling.
        - It first generates the annotated expression and the list of constraints using constraintsyntax.generate.
        - It then evaluates the annotated expression using eval_ann.run_ann.
        - If a DynamicTypeError or DynamicTypeContractError is raised during evaluation, it invokes handle_dyn_type_error to analyze the error, print a detailed and actionable message, and returns a default dynamic value.
        - Any other exceptions are propagated.
        This function ensures that all dynamic type errors are caught and explained with as much context as possible, helping the user understand the source and nature of the error, and how to fix it *)
    fun execute (exp: expressions.exp) : eval_ann.ann_value=
        let 
            val (annotated_exp, _) = constraintsyntax.generate exp
        in
            eval_ann.run_ann annotated_exp handle
                eval_ann.DynamicTypeContractError (idx, env, _, id) => let val _ = handle_dyn_type_error (idx, annotated_exp, env, id) in eval_ann.AVInt 0 end
              | e => raise e
        end
        (*
        ci sono 4 posti in cui posso generare un errore dinamico:
        - APlus1 
            - errore banale la colpa è sempre del chiamante (di chi ha generato il tipo del chiamante)
        - ANeg
            - errore banale la colpa è sempre del chiamante (di chi ha generato il tipo del chiamante)
        - AApp
        - AIf
            - non è una funzione, la colpa è sempre di chi ha generato il tipo

        *)

        (* need to give valuable messages when throwing the exceptions. 
        need to be able to refer to the internal and outer types. 
        
        maybe: 
            - change the infer function so that id doesn't generate errors
            - should add something new to the annotated expression type
                - these should be an error parameter, to keep track of who generated the error. 
                - i think that once i find an error i should be able to stop the annotation
                  as i don't need anything beyond that point. 
        
        in this function i need to be able to handle both the worklist (the set
        of constraints) and the annotated tree. 

        then i need to add an equality constraint on that.
        fir first order functions i can just use the already 
        existing contraints and check if there are any dynamic values when generating
        
        i think i also need a new funciton to change the value of a uref. 
        when i unify i most likely go to dynamic, but in this situation i dont want to change but enforce
        i can then check if the value before and after enforcing it to be equal to something are changed
        
        then i can do as i mentioned to fritz: 
            - for simple hardcoded functions like negation or plus1 the problem is always within the caller. 
            - for actual functions if the result value is changed there is a problem within the function
              otherwise it is within the caller.
        i need to keep in mind that if the problem is within the caller, i need to find who generated the value

        VERY IMPORTANT   
        one solution is to add to the uref the line that generated that type
        this means that i need some value line for newly generated types (something like -1?)
        i think that only EInt, EBool, and ELam should be able to generate new values for this field. 
            
            *)

        
end
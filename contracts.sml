(*what this module needs to do:
    - execute the program using the run function
    - see if any exception arise, in particular dynamic type errors
    - try to find out who is at fault? maybe use annotated tree in the process
    - start with first order functions but keep in mind the extension
    *)

structure contracts : CONTRACTS = struct

    exception ExpressionNotFound of int
    exception UnexpectedExpression of constraintsyntax.ann_exp

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
          | constraintsyntax.ACouple (_, _, _, _, idx) => idx

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
          | constraintsyntax.ACouple (e1, e2, _, _, idx) =>
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
                        SOME (_, v) => v
                      | NONE => eval_ann.AVDynamic (eval_ann.AVInt 0))
                    val inp' = eval_ann.ann_value_to_type inp
                    val out = get_actual_typ (b, env)
                in
                    types.TFun (inp', out) 
                end
          | constraintsyntax.AApp (f, inp, _, _, i) => 
                let
                    val f' = eval_ann.eval_ann env f
                    val n = (case f' of 
                        eval_ann.AVClosure (x, _, _) => x
                      | _ => raise eval.DynamicTypeError (i, "Expected a function for application"))
                    val inp' = eval_ann.eval_ann env inp
                in
                    get_actual_typ (f, ((n, inp') :: env))
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
          | constraintsyntax.ACouple (e1, e2, _, _, _) => 
                let 
                    val t1 = get_actual_typ (e1, env)
                    val t2 = get_actual_typ (e2, env)
                in
                    types.TCouple (t1, t2)
                end

    fun handle_dyn_type_error (idx: int, exp: constraintsyntax.ann_exp, env: eval_ann.ann_env , con: constraintsyntax.constraint list, exn_lst: exn list) =

        let 
            val e = findexp (exp, idx)
        in 
            case e of
                constraintsyntax.APlus1 (_, t1, t2, _) => 
                    let 
                        val (t, n) = URef.!! t2
                        val tv1 = URef.uref (types.TInt, [idx + 1])
                        val tv2 = URef.uref (types.TInt, [idx + 1])
                        val e' = constraintsyntax.APlus1 ((constraintsyntax.AInt (1, tv1, tv2, idx + 1)), t1, t2, idx)
                        val result = eval_ann.run_ann e' handle
                            eval.DynamicTypeError (id, _) => 
                                if 
                                    id = idx 
                                then
                                    raise eval.DynamicTypeError (id,  "Even changing the value passed to the function to a integer doesn't solve the error.")
                                else 
                                    raise eval.DynamicTypeError (id, "Changing the value passed to the function to a integer solves the error, but generates a new one at line " ^ Int.toString id ^ ".")
                          | e => raise eval.DynamicTypeError (idx, "Changing the value passed to the function to a integer solves the error, but generates a new one.")
                    in 
                        raise eval.DynamicTypeError (idx, "The error is in the caller of the +1 expression at line " ^ Int.toString idx ^". The value passed has type " ^ types.string_of_typ t ^ " but expected an integer. The value was most likely generated at line " ^ constraintsyntax.print_list n ^ ".")
                    end
              | constraintsyntax.ANeg (_, t1, t2, _) => 
                    let 
                        val (t, n) = URef.!! t2
                        val tv1 = URef.uref (types.TBool, [idx + 1])
                        val tv2 = URef.uref (types.TBool, [idx + 1])
                        val e' = constraintsyntax.ANeg ((constraintsyntax.ABool (true, tv1, tv2, idx + 1)), t1, t2, idx)
                        val result = eval_ann.run_ann e' handle
                            eval.DynamicTypeError (id, _) => 
                                if 
                                    id = idx 
                                then
                                    raise eval.DynamicTypeError (id, "Even changing the value passed to the function to a boolean doesn't solve the error.")
                                else 
                                    raise eval.DynamicTypeError (id, "Changing the value passed to the function to a boolean solves the error, but generates a new one at line " ^ Int.toString id ^ ".")
                          | e => raise eval.DynamicTypeError (idx, "Changing the value passed to the function to a boolean solves the error, but generates a new one.")
                    in 
                        raise eval.DynamicTypeError (idx, "The error is in the caller of the negation expression at line " ^ Int.toString idx ^". The value passed has type " ^ types.string_of_typ t ^ " but expected a boolean. The value was most likely generated at line " ^ constraintsyntax.print_list n ^ ".")
                    end
              | constraintsyntax.AApp (f, v, t1, t2, _) => 
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
                    raise eval.DynamicTypeError (idx, "The error is in the application of a function")
              | constraintsyntax.AIf (_, exp_then, exp_else, t1, t2, _) => 
                    let 
                        val (t, n) = URef.!! t2
                        val tv1 = URef.uref (types.TBool, [idx + 1])
                        val tv2 = URef.uref (types.TBool, [idx + 1])
                        val e' = constraintsyntax.AIf ((constraintsyntax.ABool (true, tv1, tv2, idx + 1)), exp_then, exp_else, t1, t2, idx)
                        val result = eval_ann.run_ann e' handle
                            eval.DynamicTypeError (id, _) => 
                                if 
                                    id = idx 
                                then
                                    raise eval.DynamicTypeError (id, "Even changing the value passed as the condition to the if expression to a boolean doesn't solve the error.")
                                else 
                                    raise eval.DynamicTypeError (id, "Changing the value passed as the condition to the if expression to a boolean solves the error, but generates a new one at line " ^ Int.toString id ^ ".")
                          | e => raise eval.DynamicTypeError (idx, "Changing the value passed as the condition to the if expression to a boolean solves the error, but generates a new one.")
                    in 
                        raise eval.DynamicTypeError (idx, "The error is in the generation of the condition of the if expression at line " ^ Int.toString idx ^". The value passed as a condition has type " ^ types.string_of_typ t ^ " but expected a boolean. The value was most likely generated at line " ^ constraintsyntax.print_list n ^ ".")
                    end
              | _ => raise UnexpectedExpression e
        end

    fun execute (exp: expressions.exp) : eval_ann.ann_value=
        let 
            val (annotated_exp, constraints) = constraintsyntax.generate exp
        in
            eval_ann.run_ann annotated_exp handle
                eval.DynamicTypeError (idx, _) => handle_dyn_type_error (idx, annotated_exp, [], constraints, [])
              | eval_ann.DynamicTypeContractError (idx, env, msg, lst) => handle_dyn_type_error (idx, annotated_exp, env, constraints, lst)
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
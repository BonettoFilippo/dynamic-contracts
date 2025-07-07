structure constraintsyntax : CONSTRAINTSYNTAX = struct
    
    (* unification exception *)
    exception UnifyException of string

    (* imports the uref library with an alias *)
    structure U = URef

    (* tvars are references to types. each element in the syntax tree has its own reference *)
    (* these references will be merged and combined as the program continues *)
    (* the option part is to identify refernces that are yet to be initialized from all the others *)
    type tvar = (types.typ * int) U.uref

    (* the enviroment that saves the mapping between variable names and their reference (and so their types) *)
    type tenv = (string * tvar * tvar * expressions.exp) list

    (* a type to save coercions. these coercions are from an inner to an outer type *)
    datatype constraint = Coerce of tvar * tvar

    (* the worklist is a reference that saves all the coercions found in the program *)
    val worklist : constraint list ref = ref []

    val expr_counter = ref 1


    (* a function to create a new tvar *)
    fun fresh_tvar () : tvar =
        U.uref (types.TNull, 0)

    (* a function to extract the type from a tvar *)
    fun getTyp (v: tvar) : types.typ =
        case U.!! v of
            (ty, _) => ty

    (* a function that defines how the union between tvars should be executed *)
    (* it has different cases:
        - a function checks the input and the output type of a function separately
        - a couple behaves similar to the function but in a different syntax 
        - all other case are treated very simply:
            - if we have two values if they are the same the no unification is needed
            - if they are not the same we need to convert both to dynamic
            - if only one value is given (the other is NONE), the only type is the result of the union
            - if no types are given then a NONE is returned *)
    fun unifyEq (p: tvar, q: tvar) =
        U.unify 
            (fn (pc, qc) =>
                case (pc, qc) of
                    (((types.TFun (a1, b1)), n1), ((types.TFun (a2, b2)), n2)) =>
                        let 
                            val dvar = fresh_tvar ()
                            val cvar = fresh_tvar ()

                            val _ = unifyEq (dvar, U.uref (a1, 0))
                            val _ = unifyEq (dvar, U.uref (a2, 0))
                            val _ = unifyEq (cvar, U.uref (b1, 0))
                            val _ = unifyEq (cvar, U.uref (b2, 0))

                            val finaln = (case (n1, n2) of
                                (0, x) => x
                              | (x, 0) => x
                              | (x, y) => if x = y then x else raise UnifyException "The two functions cannot be unified")

                            val (dvart, _) = U.!! dvar
                            val (cvart, _) = U.!! cvar
                        in
                            ((types.TFun (dvart, cvart)), finaln)
                        end
                  | (((types.TCouple (a1, b1)), n1), ((types.TCouple (a2, b2)), n2)) =>
                        let 
                            val lvar = fresh_tvar ()
                            val rvar = fresh_tvar ()

                            val _ = unifyEq (lvar, U.uref (a1, 0))
                            val _ = unifyEq (rvar, U.uref (b1, 0))
                            val _ = unifyEq (lvar, U.uref (a2, 0))
                            val _ = unifyEq (rvar, U.uref (b2, 0))

                            val finaln = (case (n1, n2) of
                                (0, x) => x
                              | (x, 0) => x
                              | (x, y) => if x = y then x else raise UnifyException "The two couples cannot be unified")
                            
                            val (lvart, _) = U.!! lvar
                            val (rvart, _) = U.!! rvar
                        in
                            ((types.TCouple (lvart, rvart)), finaln)
                        end
                  | ((types.TNull, _), (types.TNull, _)) => (types.TNull, 0)
                  | ((types.TNull, _), (q, n)) => (q, n)
                  | ((p, n), (types.TNull, _)) => (p, n)
                  | ((x, n1), (y, n2)) => 
                        let 
                            val t = (
                                if x = y 
                                    then x
                                    else types.TDyn)
                            val n = (case (n1, n2) of
                                (0, v) => v
                              | (v, 0) => v
                              | (v, w) => if v = w then v else raise UnifyException "The two tvars cannot be unified")
                        in
                            (t, n)
                        end)
            (p, q)
    
    (* records a coercion into the worklist. if the coercion goes from a type to NONE, a new uref for the next element is generated*)
    fun add_coerce (p, q) = 
        case U.!! q of
            (types.TNull, _) => 
                let 
                    val _ = unifyEq (q, U.uref (U.!! p))
                in 
                    worklist := Coerce (p, q) :: !worklist
                end
          | (x, _) => worklist := Coerce (p, q) :: !worklist

    (* expressions with annotated inner and outer types *)
    datatype ann_exp = 
        AInt of int * tvar * tvar * int
      | ABool of bool * tvar * tvar * int
      | AVar of string * tvar * tvar * int
      | APlus1 of ann_exp * tvar * tvar  * int
      | ANeg of ann_exp * tvar * tvar * int
      | ALam of string * ann_exp * tvar * tvar * int
      | AApp of ann_exp * ann_exp * tvar * tvar * int
      | ALet of string * ann_exp * ann_exp * tvar * tvar * int
      | AIf of ann_exp * ann_exp * ann_exp * tvar * tvar * int
      | ACouple of ann_exp * ann_exp * tvar * tvar * int

    (* this functions infers from the context and the syntax subtrees the inner and outer types of each node *)
    (* this follow some rules:
        - EInt:     both inner and outer types are TInt
        - EBool:    both inner and outer types are TBool
        - EVar:     the inner and outer types are the same as the type in the environment
        - EPlus1:   the inner type is the same as the outer type, both should be TInt if not there is a dynamic error
        - ENeg:     the inner type is the same as the outer type, both should be TBool if not there is a dynamic error
        - ELam:     the inner type is a function type that goes from the parameter outer type to the body outer type
        - EApp:     the inner type is the outer type of the body of the function
        - ELet:     the inner type is the outer type of the body of the let expression 
        - EIf:      the inner type is the union of the outer types of both branches of the if statement
        - ECast:    the inner type is the union between the outer type of the expression and the cast type
        - ECouple:  the inner type is a couple of the outer types of both expressions
     *)
    (* when not mentioning the outer type that means that if not already initialized is a copy of the inner type (a copy, not the same reference)*)
    fun infer (e: expressions.exp, tenv: tenv) : ann_exp * tvar * tvar =
        case e of
            expressions.EInt n => 
                let val t = U.uref (types.TInt, !expr_counter)
                    val _ = expr_counter := !expr_counter + 1
                in (AInt (n, t, t, !expr_counter - 1), t, t) end
          | expressions.EBool b => 
                let val t = U.uref (types.TBool, !expr_counter)
                    val _ = expr_counter := !expr_counter + 1
                in (ABool (b, t, t, !expr_counter - 1), t, t) end
          | expressions.EVar v => 
                (case List.find (fn (x, _, _, _) => x = v) tenv of
                    SOME (_, t1, t2, _) => 
                        let val _ = expr_counter := !expr_counter + 1
                        in (AVar (v, t1, t2, !expr_counter - 1), t1, t2)
                        end
                  | NONE => raise (eval.UnboundVariable "Unbound variable"))
          | expressions.EPlus1 e1 =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()
                    val e = !expr_counter
                    val _ = expr_counter := ! expr_counter + 1
                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)
                    val _ = unifyEq (outer_e1, U.uref (types.TInt, 0))
                    val _ = unifyEq (outer_e1, a)

                    val _ = add_coerce (a, b)
                in
                    (APlus1 (e1', a, b, e), a, b) end
          | expressions.ENeg e1 =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()
                    val e = !expr_counter
                    val _ = expr_counter := !expr_counter + 1
                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)
                    val _ = unifyEq (outer_e1, U.uref (types.TBool, 0))
                    val _ = unifyEq (outer_e1, a)

                    val _ = add_coerce (a, b)
                in
                    (ANeg (e1', a, b, e), a, b) end
          | expressions.ELam (v, body) =>
                let 
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()
                    val e = !expr_counter
                    val _ = expr_counter := !expr_counter + 1 
                    val (in_type, out_type) = (case List.find (fn (x, _, _, _) => x = v) tenv of
                        SOME (_, t1, t2, _) => 
                            (t1, t2)
                      | NONE => (U.uref (types.TDyn, e), U.uref (types.TDyn, e)))

                    val tenv' = (v, in_type, out_type, expressions.ELam (v, body)) :: tenv
                    val (body', inner_body, outer_body) = infer (body, tenv')

                    val _ = unifyEq (a, U.uref ((types.TFun (getTyp out_type, getTyp outer_body)), 0))

                    val _ = add_coerce (a, b)
                in 
                    (ALam (v, body', a, b, e), a, b) end
          | expressions.EApp (e1, e2) =>
                let 
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val e = !expr_counter
                    val _ = expr_counter := !expr_counter + 1 

                    val (f', inner_f, outer_f) = infer (e1, tenv)
                    val (arg', inner_arg, outer_arg) = infer (e2, tenv)

                    val _ = (case e1 of
                        expressions.ELam (x, body) => 
                            let 
                                val e' = !expr_counter
                                val (f'', inner_f'', outer_f'') = infer (expressions.ELam (x, body), ((x, inner_arg, outer_arg, expressions.ELam (x, body))::tenv))
                                val _ = (worklist := (case !worklist of
                                                    [] => []
                                                  | _::rest => rest))
                                val _ = expr_counter := e'
                            in
                                case (getTyp outer_f'') of 
                                    types.TFun (_, ret_typ) => 
                                        unifyEq (U.uref (ret_typ, 0), a)
                                  | _ => 
                                        raise eval.DynamicTypeError ("Expected function type in application")
                            end
                      | expressions.EVar v => 
                            (case List.find (fn (x, _, _, _) => x = v) tenv of
                                SOME (_, _, _, evar) => (case evar of
                                    expressions.ELam (x, body) => 
                                        let 
                                            val e' = !expr_counter
                                            val (f'', inner_f'', outer_f'') = infer (expressions.ELam (x, body), ((x, inner_arg, outer_arg, expressions.ELam (x, body))::tenv))
                                            val _ = (worklist := (   case !worklist of
                                                                [] => []
                                                            | _::rest => rest))
                                            val _ = expr_counter := e'
                                        in
                                            case (getTyp outer_f'') of 
                                                types.TFun (_, ret_typ) => 
                                                    unifyEq (U.uref (ret_typ, 0), a)
                                            | _ => 
                                                    raise eval.DynamicTypeError ("Expected function type in application")
                                        end
                                  | expressions.ELet (f, e1', e2') => (case e1' of
                                        expressions.ELam (x, body) => 
                                            let 
                                                val e' = !expr_counter
                                                val (f'', inner_f'', outer_f'') = infer (expressions.ELam (x, body), ((x, inner_arg, outer_arg, expressions.ELam (x, body))::tenv))
                                                val _ = (worklist := (   case !worklist of
                                                                    [] => []
                                                                | _::rest => rest))
                                                val _ = expr_counter := e'
                                            in
                                                case (getTyp outer_f'') of 
                                                    types.TFun (_, ret_typ) => 
                                                        unifyEq (U.uref (ret_typ, 0), a)
                                                | _ => 
                                                        raise eval.DynamicTypeError ("Expected function type in application")
                                            end
                                      | _ => raise eval.DynamicTypeError ("Expected function type in application"))
                                  | _ => raise eval.DynamicTypeError ("Expected function type in application"))
                              | NONE => raise (eval.UnboundVariable "Unbound variable"))
                      | _ => raise eval.DynamicTypeError ("Expected function type in application"))

                    val _ = unifyEq (U.uref ((types.TFun (getTyp outer_arg, getTyp a)), 0), outer_f)

                    val _ = add_coerce (a, b)
                in 
                    (AApp (f', arg', a, b, e), a, b) end
          | expressions.ELet (x, e1, e2) =>
                let 
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()
                    val e = !expr_counter
                    val _ = expr_counter := !expr_counter + 1

                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)
                    val g = fresh_tvar ()
                    val g' = fresh_tvar ()
                    val _ = unifyEq (outer_e1, g)
                    val _ = unifyEq (outer_e1, g')
                    val tenv' = (x, g, g', expressions.ELet (x, e1, e2)) :: tenv

                    val (e2', inner_e2, outer_e2) = infer (e2, tenv')
                    val _ = unifyEq (a, outer_e2)

                    val _ = add_coerce (a, b)
                in 
                    (ALet (x, e1', e2', a, b, e), a, b) end
          | expressions.EIf (cond, e_then, e_else) =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val e = !expr_counter
                    val _ = expr_counter := !expr_counter + 1

                    val (cond', inner_cond, outer_cond) = infer (cond, tenv)
                    val _ = unifyEq (outer_cond, U.uref (types.TBool, 0))

                    val (e_then', inner_then, outer_then) = infer (e_then, tenv)
                    val (e_else', inner_else, outer_else) = infer (e_else, tenv)

                    val _ = unifyEq (outer_then, a)
                    val _ = unifyEq (outer_else, a)

                    val _ = add_coerce (a, b)
                in
                    (AIf (cond', e_then', e_else', a, b, e), a, b) end
          | expressions.ECouple (e1, e2) =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val e = !expr_counter
                    val _ = expr_counter := !expr_counter + 1

                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)
                    val (e2', inner_e2, outer_e2) = infer (e2, tenv)

                    val _ = unifyEq (a, U.uref ((types.TCouple (getTyp outer_e1, getTyp outer_e2)), 0))      

                    val _ = add_coerce (a, b)
                in
                    (ACouple (e1', e2', a, b, e), a, b) end

    (* runs the annotation on an empty worklist *)
    fun generate (e: expressions.exp) : ann_exp * constraint list =
        (worklist := []; expr_counter := 0;
         let 
            val (tree, _, _) = infer (e, [])
         in (tree, List.rev (!worklist)) end)

    (* a function to convert a tvar to a string representation *)
    fun string_of_tvar (v: tvar) : string =
        let
            val (ty_opt, i) = U.!! v
        in
            types.string_of_typ ty_opt ^ " #" ^ Int.toString i
        end
    
    (* pretty printing function for annotated expressions *)
    fun prettyp (e: ann_exp) : string =
        case e of
            AInt (n, t1, t2, _) => Int.toString n ^ " : " ^ string_of_tvar t2 
          | ABool (b, t1, t2, _) => Bool.toString b ^ " : " ^ string_of_tvar t2 
          | AVar (v, t1, t2, _) => v ^ " : " ^ string_of_tvar t2  
          | APlus1 (e1, t1, t2, _) => 
                let
                    val e1_str = prettyp e1
                in
                    "(" ^ e1_str ^ " + 1) : " ^ string_of_tvar t2 ^ "\n"
                end
          | ANeg (e1, t1, t2, _) =>
                let
                    val e1_str = prettyp e1
                in
                    "(!" ^ e1_str ^ ") : " ^ string_of_tvar t2 ^ "\n"
                end
          | ALam (v, body, t1, t2, _) => 
                let
                    val body_str = prettyp body
                in
                    "(λ" ^ v ^ ". " ^ body_str ^ ")  : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
          | AApp (e1, e2, t1, t2, _) =>
                let
                    val e1_str = prettyp e1
                    val e2_str = prettyp e2
                in
                    "(" ^ e1_str ^ " " ^ e2_str ^ ") : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
          | ALet (x, e1, e2, t1, t2, _) =>
                let
                    val e1_str = prettyp e1
                    val e2_str = prettyp e2
                in
                    "(let " ^ x ^ " = " ^ e1_str ^ " in " ^ e2_str ^ ") : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
          | AIf (cond, e_then, e_else, t1, t2, _) =>
                let
                    val cond_str = prettyp cond
                    val then_str = prettyp e_then
                    val else_str = prettyp e_else
                in
                    "(if " ^ cond_str ^ " then " ^ then_str ^ " else " ^ else_str ^ ") : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
          | ACouple (e1, e2, t1, t2, _) =>
                let
                    val e1_str = prettyp e1
                    val e2_str = prettyp e2
                in
                    "(couple " ^ e1_str ^ ", " ^ e2_str ^ ") : " ^ string_of_tvar t2  ^ "\n"
                end
    
    (* pretty preting function for the worklist *)
    fun prettyp_worklist () : string =
        let
            val constraints = List.map (fn Coerce (p, q) => "Coerce: " ^ string_of_tvar p ^ " -> " ^ string_of_tvar q) (!worklist)
        in
            String.concatWith "\n" constraints
        end
end; 



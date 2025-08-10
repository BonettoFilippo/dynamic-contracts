structure constraintsyntax : CONSTRAINTSYNTAX = struct
    
    (* unification exception *)
    exception UnifyException of string

    (* imports the uref library with an alias *)
    structure U = URef

    (* tvars are references to types. each element in the syntax tree has its own reference *)
    (* these references will be merged and combined as the program continues *)
    (* the option part is to identify refernces that are yet to be initialized from all the others *)
    type tvar = (types.typ * int list) U.uref

    (* the enviroment that saves the mapping between variable names and their reference (and so their types) *)
    type tenv = (string * tvar * tvar) list

    (* a type to save coercions. these coercions are most often from an inner to an outer type *)
    datatype constraint = Coerce of tvar * tvar

    (* the worklist is a reference that saves all the coercions found in the program *)
    val worklist : constraint list ref = ref []

    (* a counter to give each expression its unique index number *)
    val expr_counter = ref 1

    (* a function to create a new tvar *)
    fun fresh_tvar () : tvar =
        U.uref (types.TNull, [])

    (* a function to extract the type from a tvar *)
    fun getTyp (v: tvar) : types.typ =
        case U.!! v of
            (ty, _) => ty

    (* a function that defines how the union between tvars should be executed *)
    (* it has different cases:
        - a function checks the input and the output type of a function separately
        - a Pair behaves similar to the function but in a different syntax 
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
                            val (dvart, cvart, finaln) = unifyFunPair (a1, b1, n1, a2, b2, n2, "functions")
                        in
                            ((types.TFun (dvart, cvart)), finaln)
                        end
                  | (((types.TPair (a1, b1)), n1), ((types.TPair (a2, b2)), n2)) =>
                        let 
                            val (lvart, rvart, finaln) = unifyFunPair (a1, b1, n1, a2, b2, n2, "Pairs")
                        in
                            ((types.TPair (lvart, rvart)), finaln)
                        end
                  | ((types.TNull, _), (types.TNull, _)) => (types.TNull, [])
                  | ((types.TNull, _), (q, n)) => (q, n)
                  | ((p, n), (types.TNull, _)) => (p, n)
                  | ((x, n1), (y, n2)) => 
                        let 
                            val t = (
                                if x = y 
                                    then x
                                    else types.TDyn)
                            val n = n1 @ n2
                        in
                            (t, n)
                        end)
            (p, q)

    (* a helper function to handle functions and Pairs whend executing the unify *)
    and unifyFunPair (a1: types.typ, b1: types.typ, n1: int list, a2: types.typ, b2: types.typ, n2: int list, text: string) : types.typ * types.typ * int list =
        let 
            val lvar = fresh_tvar ()
            val rvar = fresh_tvar ()

            val _ = unifyEq (lvar, U.uref (a1, []))
            val _ = unifyEq (rvar, U.uref (b1, []))
            val _ = unifyEq (lvar, U.uref (a2, []))
            val _ = unifyEq (rvar, U.uref (b2, []))

            val finaln = n1 @ n2
            
            val (lvart, _) = U.!! lvar
            val (rvart, _) = U.!! rvar
        in
            (lvart, rvart, finaln)
        end

    (* records a coercion into the worklist. if the coercion goes from a type to NONE, a new uref for the next element is generated*)
    fun add_coerce (p: tvar, q: tvar) = 
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
      | APair of ann_exp * ann_exp * tvar * tvar * int

    (* this functions infers from the context and the syntax subtrees the inner and outer types of each node *)
    (* this follow some rules:
        - EInt:     both inner and outer types are TInt
        - EBool:    both inner and outer types are TBool
        - EVar:     the inner and outer types are the same as the type in the environment
        - EPlus1:   the inner type is the same as the outer type, both should be TInt if not there is a dynamic error
        - ENeg:     the inner type is the same as the outer type, both should be TBool if not there is a dynamic error
        - ELam:     the inner type is a function type that goes from the parameter outer type to the body outer type
                    keep in mind that in some cases it is not possible to infer types for the body, so they are saved as Dyn
        - EApp:     the inner type is the outer type of the body of the function
        - ELet:     the inner type is the outer type of the body of the let expression 
        - EIf:      the inner type is the union of the outer types of both branches of the if statement
        - ECast:    the inner type is the union between the outer type of the expression and the cast type
        - EPair:  the inner type is a Pair of the outer types of both expressions
     *)
    (* when not mentioning the outer type that means that if not already initialized is a copy of the inner type (a copy, not the same reference)*)
    fun infer (e: expressions.exp, tenv: tenv) : ann_exp * tvar * tvar =
        case e of
            expressions.EInt n => 
                let val t = U.uref (types.TInt, [!expr_counter])
                    val t1 = U.uref (types.TInt, [!expr_counter])

                    val _ = add_coerce (t, t1)

                    val _ = expr_counter := !expr_counter + 1
                in (AInt (n, t, t1, !expr_counter - 1), t, t1) end
          | expressions.EBool b => 
                let val t = U.uref (types.TBool, [!expr_counter])
                    val t1 = U.uref (types.TBool, [!expr_counter])

                    val _ = add_coerce (t, t1)

                    val _ = expr_counter := !expr_counter + 1
                in (ABool (b, t, t1, !expr_counter - 1), t, t1) end
          | expressions.EVar v => 
                (case List.find (fn (x, _, _) => x = v) tenv of
                    SOME (_, t1, t2) => 
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

                    val _ = unifyEq (a, U.uref (types.TInt, []))

                    val _ = add_coerce (a, b)

                    val _ = unifyEq (outer_e1, b)

                in
                    (APlus1 (e1', a, b, e), a, b) end
          | expressions.ENeg e1 =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val e = !expr_counter
                    val _ = expr_counter := !expr_counter + 1

                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)

                    val _ = unifyEq (a, U.uref (types.TBool, []))
                    
                    val _ = add_coerce (a, b)

                    val _ = unifyEq (outer_e1, b)               
                in
                    (ANeg (e1', a, b, e), a, b) end
          | expressions.ELam (v, body) =>
                let 
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val e = !expr_counter
                    val _ = expr_counter := !expr_counter + 1 
                    
                    val in_type = U.uref (types.TDyn, [e])
                    val out_type = U.uref (types.TDyn, [e])

                    val tenv' = (v, in_type, out_type) :: tenv

                    val (body', inner_body, outer_body) = infer (body, tenv')

                    val _ = unifyEq (a, U.uref ((types.TFun (getTyp out_type, getTyp outer_body)), []))

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

                    val _ = unifyEq (U.uref ((types.TFun (getTyp outer_arg, getTyp a)), []), outer_f)

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
                    val _ = add_coerce (g, g')
                    
                    val tenv' = (x, g, g') :: tenv

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
                    val _ = unifyEq (outer_cond, U.uref (types.TBool, []))

                    val (e_then', inner_then, outer_then) = infer (e_then, tenv)
                    val (e_else', inner_else, outer_else) = infer (e_else, tenv)

                    val _ = unifyEq (outer_then, a)
                    val _ = unifyEq (outer_else, a)

                    val _ = add_coerce (a, b)
                in
                    (AIf (cond', e_then', e_else', a, b, e), a, b) end
          | expressions.EPair (e1, e2) =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val e = !expr_counter
                    val _ = expr_counter := !expr_counter + 1

                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)
                    val (e2', inner_e2, outer_e2) = infer (e2, tenv)

                    val _ = unifyEq (a, U.uref ((types.TPair (getTyp outer_e1, getTyp outer_e2)), []))      

                    val _ = add_coerce (a, b)
                in
                    (APair (e1', e2', a, b, e), a, b) end

    (* runs the annotation on an empty worklist *)
    fun generate (e: expressions.exp) : ann_exp * constraint list =
        (worklist := []; expr_counter := 1;
         let 
            val (tree, _, _) = infer (e, [])
         in (tree, List.rev (!worklist)) end)

    (* prints a list of integers *)
    fun print_list (l: int list) : string =
        case l of 
            [] => ""
          | (x::xs) => Int.toString x ^ ", " ^ print_list xs

    (* a function to convert a tvar to a string representation *)
    fun string_of_tvar (v: tvar) : string =
        let
            val (ty_opt, i) = U.!! v
        in
            types.string_of_typ ty_opt ^ " #[" ^ print_list i ^ "]"
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
          | APair (e1, e2, t1, t2, _) =>
                let
                    val e1_str = prettyp e1
                    val e2_str = prettyp e2
                in
                    "(Pair " ^ e1_str ^ ", " ^ e2_str ^ ") : " ^ string_of_tvar t2  ^ "\n"
                end
    
    (* pretty preting function for the worklist *)
    fun prettyp_worklist () : string =
        let
            val constraints = List.map (fn Coerce (p, q) => "Coerce: " ^ string_of_tvar p ^ " -> " ^ string_of_tvar q) (!worklist)
        in
            String.concatWith "\n" constraints
        end
end; 



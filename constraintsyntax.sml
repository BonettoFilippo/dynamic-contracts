structure constraintsyntax : CONSTRAINTSYNTAX = struct
    (* imports the uref library with an alias *)
    structure U = URef

    (* tvars are references to types. each element in the syntax tree has its own reference *)
    (* these references will be merged and combined as the program continues *)
    (* the option part is to identify refernces that are yet to be initialized from all the others *)
    type tvar = types.typ option U.uref

    (* the enviroment that saves the mapping between variable names and their reference (and so their types) *)
    type tenv = (string * tvar) list

    (* a type to save coercions. these coercions are from an inner to an outer type *)
    datatype constraint = Coerce of tvar * tvar

    (* the worklist is a reference that saves all the coercions found in the program *)
    val worklist : constraint list ref = ref []

    (* a function to create a new tvar *)
    fun fresh_tvar () : tvar =
        U.uref NONE

    (* a function to extract the type from a tvar *)
    fun getTyp (v: tvar) : types.typ =
        case U.!! v of
            SOME ty => ty
          | NONE    => types.TDyn

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
                    (SOME (types.TFun (a1, b1)), SOME (types.TFun (a2, b2))) =>
                        let 
                            val dvar = fresh_tvar ()
                            val cvar = fresh_tvar ()

                            val _ = unifyEq (dvar, U.uref (SOME a1))
                            val _ = unifyEq (dvar, U.uref (SOME a2))
                            val _ = unifyEq (dvar, U.uref (SOME a1))
                            val _ = unifyEq (cvar, U.uref (SOME b1))
                            val _ = unifyEq (cvar, U.uref (SOME b2))
                            val _ = unifyEq (cvar, U.uref (SOME b1))
                        in
                            SOME (types.TFun (Option.valOf (U.!!dvar), Option.valOf (U.!!cvar)))
                        end
                  | (SOME (types.TCouple (a1, b1)), SOME (types.TCouple (a2, b2))) =>
                        let 
                            val lvar = fresh_tvar ()
                            val rvar = fresh_tvar ()

                            val _ = unifyEq (lvar, U.uref (SOME a1))
                            val _ = unifyEq (rvar, U.uref (SOME b1))
                            val _ = unifyEq (lvar, U.uref (SOME a2))
                            val _ = unifyEq (rvar, U.uref (SOME b2))
                        in
                            SOME (types.TCouple (Option.valOf (U.!!lvar), Option.valOf (U.!!rvar)))
                        end
                  | (SOME x, SOME y) => 
                        if x = y then SOME x
                        else SOME types.TDyn
                  | (NONE, NONE) => NONE
                  | (NONE, SOME q) => SOME q
                  | (SOME p, NONE) => SOME p)
            (p, q)
    
    (* records a coercion into the worklist. if the coercion goes from a type to NONE, a new uref for the next element is generated*)
    fun add_coerce (p, q) = 
        case U.!! q of
            NONE => 
                let 
                    val _ = unifyEq (q, U.uref (U.!! p))
                in 
                    worklist := Coerce (p, q) :: !worklist
                end
          | SOME x => worklist := Coerce (p, q) :: !worklist

    (* expressions with annotated inner and outer types *)
    datatype ann_exp = 
        AInt of int * tvar * tvar
      | ABool of bool * tvar * tvar
      | AVar of string * tvar * tvar
      | ALam of string * types.typ * ann_exp * tvar * tvar
      | AApp of ann_exp * ann_exp * tvar * tvar
      | ALet of string * ann_exp * ann_exp * tvar * tvar
      | AIf of ann_exp * ann_exp * ann_exp * tvar * tvar
      | ACast of ann_exp * types.typ * tvar * tvar
      | ACouple of ann_exp * ann_exp * tvar * tvar

    (* this functions infers from the context and the syntax subtrees the inner and outer types of each node *)
    (* this follow some rules:
        - EInt:     both inner and outer types are TInt
        - EBool:    both inner and outer types are TBool
        - EVar:     the inner and outer types are the same as the type in the environment
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
                let val t = U.uref (SOME types.TInt)
                in (AInt (n, t, t), t, t) end
          | expressions.EBool b => 
                let val t = U.uref (SOME types.TBool)
                in (ABool (b, t, t), t, t) end
          | expressions.EVar v => 
                (case List.find (fn (x, _) => x = v) tenv of
                    SOME (_, t) => 
                        (AVar (v, t, t), t, t)
                  | NONE => raise (eval.UnboundVariable "Unbound variable"))
          | expressions.ELam (v, t, body) =>
                let 
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val tenv' = (v, U.uref (SOME t)) :: tenv
                    val (body', inner_body, outer_body) = infer (body, tenv')

                    val _ = unifyEq (a, U.uref (SOME (types.TFun (t, getTyp outer_body))))

                    val _ = add_coerce (a, b)
                in 
                    (ALam (v, t, body', a, b), a, b) end
          | expressions.EApp (e1, e2) =>
                let 
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val (f', inner_f, outer_f) = infer (e1, tenv)
                    val (arg', inner_arg, outer_arg) = infer (e2, tenv)

                    val _ = (case (getTyp outer_f) of 
                        types.TFun (_, ret_typ) => 
                            unifyEq (U.uref (SOME ret_typ), a)
                      | _ => 
                            raise eval.DynamicTypeError ("Expected function type in application"))

                    val _ = unifyEq (U.uref (SOME (types.TFun (getTyp outer_arg, getTyp a))), outer_f)

                    val _ = add_coerce (a, b)
                in 
                    (AApp (f', arg', a, b), a, b) end
          | expressions.ELet (x, e1, e2) =>
                let 
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)
                    val g = fresh_tvar ()
                    val _ = unifyEq (outer_e1, g)
                    val tenv' = (x, g) :: tenv

                    val (e2', inner_e2, outer_e2) = infer (e2, tenv')
                    val _ = unifyEq (a, outer_e2)

                    val _ = add_coerce (a, b)
                in 
                    (ALet (x, e1', e2', a, b), a, b) end
          | expressions.EIf (cond, e_then, e_else) =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val (cond', inner_cond, outer_cond) = infer (cond, tenv)
                    val _ = unifyEq (outer_cond, U.uref (SOME types.TBool))

                    val (e_then', inner_then, outer_then) = infer (e_then, tenv)
                    val (e_else', inner_else, outer_else) = infer (e_else, tenv)

                    val _ = unifyEq (outer_then, a)
                    val _ = unifyEq (outer_else, a)

                    val _ = add_coerce (a, b)
                in
                    (AIf (cond', e_then', e_else', a, b), a, b) end
          | expressions.ECast (e, t) =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val (e', inner_e, outer_e) = infer (e, tenv)
                    val _ = unifyEq (outer_e, a)
                    val _ = unifyEq (a, U.uref (SOME t))

                    val _ = add_coerce (a, b)
                in
                    (ACast (e', t, a, b), a, b) end
          | expressions.ECouple (e1, e2) =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)
                    val (e2', inner_e2, outer_e2) = infer (e2, tenv)

                    val _ = unifyEq (a, U.uref (SOME (types.TCouple (getTyp outer_e1, getTyp outer_e2))))      

                    val _ = add_coerce (a, b)
                in
                    (ACouple (e1', e2', a, b), a, b) end

    (* runs the annotation on an empty worklist *)
    fun generate (e: expressions.exp) : ann_exp * constraint list =
        (worklist := [];
         let 
            val (tree, _, _) = infer (e, [])
         in (tree, List.rev (!worklist)) end)

    (* a function to convert a tvar to a string representation *)
    fun string_of_tvar (v: tvar) : string =
        let
            val ty_opt = U.!! v
            val ty = case ty_opt of
                SOME t => t
              | NONE => types.TDyn
        in
            types.string_of_typ ty
        end
    
    (* pretty printing function for annotated expressions *)
    fun prettyp (e: ann_exp) : string =
        case e of
            AInt (n, t1, t2) => Int.toString n ^ " : " ^ string_of_tvar t2
          | ABool (b, t1, t2) => Bool.toString b ^ " : " ^ string_of_tvar t2 
          | AVar (v, t1, t2) => v ^ " : " ^ string_of_tvar t2  
          | ALam (v, t, body, t1, t2) => 
                let
                    val param = types.string_of_typ t
                    val body_str = prettyp body
                in
                    "(λ" ^ v ^ " : " ^ param ^ ". " ^ body_str ^ ")  : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
          | AApp (e1, e2, t1, t2) =>
                let
                    val e1_str = prettyp e1
                    val e2_str = prettyp e2
                in
                    "(" ^ e1_str ^ " " ^ e2_str ^ ") : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
          | ALet (x, e1, e2, t1, t2) =>
                let
                    val e1_str = prettyp e1
                    val e2_str = prettyp e2
                in
                    "(let " ^ x ^ " = " ^ e1_str ^ " in " ^ e2_str ^ ") : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
          | AIf (cond, e_then, e_else, t1, t2) =>
                let
                    val cond_str = prettyp cond
                    val then_str = prettyp e_then
                    val else_str = prettyp e_else
                in
                    "(if " ^ cond_str ^ " then " ^ then_str ^ " else " ^ else_str ^ ") : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
          | ACast (e, t, t1, t2) =>
                let
                    val e_str = prettyp e
                in
                    "(cast " ^ e_str ^ " as " ^ types.string_of_typ t ^ ") : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
          | ACouple (e1, e2, t1, t2) =>
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



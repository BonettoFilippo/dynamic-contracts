structure constraintsyntax : CONSTRAINTSYNTAX = struct
    structure U = URef

    type tvar = types.typ option U.uref
    type tenv = (string * tvar) list

    datatype constraint = Coerce of tvar * tvar
    val worklist : constraint list ref = ref []

    fun fresh_tvar () : tvar =
        U.uref NONE

    fun add_coerce (p, q) = 
        worklist := Coerce (p, q) :: !worklist

    fun getTyp (v: tvar) : types.typ =
        case U.!! v of
            SOME ty => ty
          | NONE    => types.TDyn

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
                    val _ = add_coerce (a, b)

                    val tenv' = (v, U.uref (SOME t)) :: tenv
                    val (body', inner_body, outer_body) = infer (body, tenv')

                    val _ = unifyEq (a, U.uref (SOME (types.TFun (t, getTyp inner_body))))
                    val _ = unifyEq (b, U.uref (SOME (types.TFun (t, getTyp outer_body))))
                in 
                    (ALam (v, t, body', a, b), a, b) end
          | expressions.EApp (e1, e2) =>
                let 
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()
                    val _ = add_coerce (a, b)

                    val (f', inner_f, outer_f) = infer (e1, tenv)
                    val (arg', inner_arg, outer_arg) = infer (e2, tenv)

                    val _ = (case (getTyp inner_f) of 
                        types.TFun (_, ret_typ) => 
                            unifyEq (U.uref (SOME ret_typ), a)
                      | _ => 
                            raise eval.DynamicTypeError ("Expected function type in application"))

                    val _ = (case (getTyp outer_f) of 
                        types.TFun (_, ret_typ) => 
                            unifyEq (U.uref (SOME ret_typ), b)
                      | _ => 
                            raise eval.DynamicTypeError ("Expected function type in application"))

                    val _ = unifyEq (U.uref (SOME (types.TFun (getTyp outer_arg, getTyp a))), inner_f)
                    val _ = unifyEq (U.uref (SOME (types.TFun (getTyp outer_arg, getTyp b))), outer_f)
                in 
                    (AApp (f', arg', a, b), a, b) end
          | expressions.ELet (x, e1, e2) =>
                let 
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val _ = add_coerce (a, b)

                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)
                    val g = fresh_tvar ()
                    val _ = unifyEq (outer_e1, g)
                    val tenv' = (x, g) :: tenv

                    val (e2', inner_e2, outer_e2) = infer (e2, tenv')
                    val _ = unifyEq (a, inner_e2)
                    val _ = unifyEq (b, outer_e2)

                in 
                    (ALet (x, e1', e2', a, b), a, b) end
          | expressions.EIf (cond, e_then, e_else) =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val _ = add_coerce (a, b)

                    val (cond', inner_cond, outer_cond) = infer (cond, tenv)
                    val _ = unifyEq (outer_cond, U.uref (SOME types.TBool))

                    val (e_then', inner_then, outer_then) = infer (e_then, tenv)
                    val (e_else', inner_else, outer_else) = infer (e_else, tenv)

                    val _ = unifyEq (inner_then, a)
                    val _ = unifyEq (outer_then, b)
                    val _ = unifyEq (inner_else, a)
                    val _ = unifyEq (outer_else, b)
                in
                    (AIf (cond', e_then', e_else', a, b), a, b) end
          | expressions.ECast (e, t) =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val _ = add_coerce (a, b)

                    val (e', inner_e, outer_e) = infer (e, tenv)
                    val _ = unifyEq (inner_e, a)
                    val _ = unifyEq (outer_e, b)
                    val _ = unifyEq (b, U.uref (SOME t))
                in
                    (ACast (e', t, a, b), a, b) end
          | expressions.ECouple (e1, e2) =>
                let
                    val a = fresh_tvar ()
                    val b = fresh_tvar ()

                    val _ = add_coerce (a, b)

                    val (e1', inner_e1, outer_e1) = infer (e1, tenv)
                    val (e2', inner_e2, outer_e2) = infer (e2, tenv)

                    val _ = unifyEq (a, U.uref (SOME (types.TCouple (getTyp inner_e1, getTyp inner_e2))))
                    val _ = unifyEq (b, U.uref (SOME (types.TCouple (getTyp outer_e1, getTyp outer_e2))))                    
                in
                    (ACouple (e1', e2', a, b), a, b) end

    fun generate (e: expressions.exp) : ann_exp * constraint list =
        (worklist := [];
         let 
            val (tree, _, _) = infer (e, [])
         in (tree, List.rev (!worklist)) end)

    fun string_of_tvar (v: tvar) : string =
        let
            val ty_opt = U.!! v
            val ty = case ty_opt of
                SOME t => t
              | NONE => types.TDyn
        in
            types.string_of_typ ty
        end
    
    fun prettyp (e: ann_exp) : string =
        case e of
            AInt (n, t1, t2) => Int.toString n ^ " : " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2 ^ "\n"
          | ABool (b, t1, t2) => Bool.toString b ^ " : " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
          | AVar (v, t1, t2) => v ^ " : " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
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
                    "(couple " ^ e1_str ^ ", " ^ e2_str ^ ") : \n " ^ string_of_tvar t1 ^ " -> " ^ string_of_tvar t2  ^ "\n"
                end
end; 

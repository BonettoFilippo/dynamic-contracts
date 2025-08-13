structure TestConstraint = struct
    open constraintsyntax
    open types
    open expressions
    open URef

    (* Test unification of two type variables. *)
    fun unify_test desc p_type q_type expected_type =
        let
            val p : tvar = (URef.uref (p_type, ([]: int list)))
            val q : tvar = (URef.uref  (q_type, ([]: int list)))
            val _ = unifyEq (p, q)
            val res_p = getTyp p
            val res_q = getTyp q
            val pass = (res_p = expected_type) andalso (res_q = expected_type)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (expected " ^ types.typ_to_string expected_type ^
                    ", got " ^ types.typ_to_string res_p ^ " and " ^
                    types.typ_to_string res_q ^ ")") ^ "\n")
        in
            ()
        end

    (* Test that add_coerce records a coercion and optionally unifies
     types when the right-hand side is TNull. *)
    fun add_coerce_test_null desc p_type q_type =
        let
            val p : tvar = (URef.uref (p_type, ([]: int list)))
            val q : tvar = (URef.uref (q_type, ([]: int list)))
            val _ = worklist := []
            val _ = add_coerce (p, q)
            val w = !worklist
            val res_p = getTyp p
            val res_q = getTyp q
            val pass = (List.length w = 1) andalso (res_p = p_type) andalso (res_q = p_type)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (worklist length=" ^ Int.toString (List.length w) ^
                    ", p typ=" ^ types.typ_to_string res_p ^
                    ", q typ=" ^ types.typ_to_string res_q ^ ")") ^ "\n")
        in
            ()
        end

    (* Test that add_coerce records a coercion when q is non-null but
     does not change either type. *)
    fun add_coerce_test_non_null desc p_type q_type =
        let
            val p : tvar = (URef.uref (p_type, ([]: int list)))
            val q : tvar = (URef.uref (q_type, ([]: int list)))
            val _ = worklist := []
            val _ = add_coerce (p, q)
            val w = !worklist
            val res_p = getTyp p
            val res_q = getTyp q
            val pass = (List.length w = 1) andalso (res_p = p_type) andalso (res_q = q_type)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (worklist length=" ^ Int.toString (List.length w) ^
                    ", p typ=" ^ types.typ_to_string res_p ^
                    ", q typ=" ^ types.typ_to_string res_q ^ ")") ^ "\n")
        in
            ()
        end

    (* Infer an integer literal. *)
    fun infer_int_test desc n =
        let
            val _ = worklist := []
            val (_, inner_t, outer_t) = infer (EInt n, ([] : tenv))
            val res_inner = getTyp inner_t
            val res_outer = getTyp outer_t
            val wl_len = List.length (!worklist)
            val pass = (res_inner = TInt) andalso (res_outer = TInt) andalso (wl_len = 1)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (inner typ=" ^ types.typ_to_string res_inner ^
                    ", outer typ=" ^ types.typ_to_string res_outer ^
                    ", worklist length=" ^ Int.toString wl_len ^ ")") ^ "\n")
        in
            ()
        end

    (* Infer a boolean literal. *)
    fun infer_bool_test desc b =
        let
            val _ = worklist := []
            val (_, inner_t, outer_t) = infer (EBool b, ([] : tenv))
            val res_inner = getTyp inner_t
            val res_outer = getTyp outer_t
            val wl_len = List.length (!worklist)
            val pass = (res_inner = TBool) andalso (res_outer = TBool) andalso (wl_len = 1)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (inner typ=" ^ types.typ_to_string res_inner ^
                    ", outer typ=" ^ types.typ_to_string res_outer ^
                    ", worklist length=" ^ Int.toString wl_len ^ ")") ^ "\n")
        in
            ()
        end

    (* Infer a unary plus one applied to an integer. *)
    fun infer_plus1_int_test desc n =
        let
            val _ = worklist := []
            val (_, inner_t, outer_t) = infer (EPlus1 (EInt n), ([] : tenv))
            val res_inner = getTyp inner_t
            val res_outer = getTyp outer_t
            val wl_len = List.length (!worklist)
            val pass = (res_inner = TInt) andalso (res_outer = TInt) andalso (wl_len = 2)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (inner typ=" ^ types.typ_to_string res_inner ^
                    ", outer typ=" ^ types.typ_to_string res_outer ^
                    ", worklist length=" ^ Int.toString wl_len ^ ")") ^ "\n")
        in
            ()
        end

    (* Infer a unary plus one applied to a boolean. *)
    fun infer_plus1_bool_test desc =
        let
            val _ = worklist := []
            val (_, inner_t, outer_t) = infer (EPlus1 (EBool true), ([] : tenv))
            val res_inner = getTyp inner_t
            val res_outer = getTyp outer_t
            val wl_len = List.length (!worklist)
            val pass = (res_inner = TInt) andalso (res_outer = TDyn) andalso (wl_len = 2)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (inner typ=" ^ types.typ_to_string res_inner ^
                    ", outer typ=" ^ types.typ_to_string res_outer ^
                    ", worklist length=" ^ Int.toString wl_len ^ ")") ^ "\n")
        in
            ()
        end

    (* Infer a logical negation of a boolean. *)
    fun infer_neg_bool_test desc =
        let
            val _ = worklist := []
            val (_, inner_t, outer_t) = infer (ENeg (EBool true), ([] : tenv))
            val res_inner = getTyp inner_t
            val res_outer = getTyp outer_t
            val wl_len = List.length (!worklist)
            val pass = (res_inner = TBool) andalso (res_outer = TBool) andalso (wl_len = 2)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (inner typ=" ^ types.typ_to_string res_inner ^
                    ", outer typ=" ^ types.typ_to_string res_outer ^
                    ", worklist length=" ^ Int.toString wl_len ^ ")") ^ "\n")
        in
            ()
        end

    (* Infer a logical negation of an integer. *)
    fun infer_neg_int_test desc =
        let
            val _ = worklist := []
            val (_, inner_t, outer_t) = infer (ENeg (EInt 5), ([] : tenv))
            val res_inner = getTyp inner_t
            val res_outer = getTyp outer_t
            val wl_len = List.length (!worklist)
            val pass = (res_inner = TBool) andalso (res_outer = TDyn) andalso (wl_len = 2)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (inner typ=" ^ types.typ_to_string res_inner ^
                    ", outer typ=" ^ types.typ_to_string res_outer ^
                    ", worklist length=" ^ Int.toString wl_len ^ ")") ^ "\n")
        in
            ()
        end

    (* Infer a variable reference. *)
    fun infer_var_test desc =
        let
            val _ = worklist := []
            val t1 : tvar = (URef.uref (TInt, ([]: int list)))
            val t2 : tvar = (URef.uref (TInt, ([]: int list)))
            val tenv1 : tenv = [("x", t1, t2)]
            val (_, inner_t, outer_t) = infer (EVar "x", tenv1)
            val res_inner = getTyp inner_t
            val res_outer = getTyp outer_t
            val wl_len = List.length (!worklist)
            val pass = (res_inner = TInt) andalso (res_outer = TInt) andalso (wl_len = 0)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (inner typ=" ^ types.typ_to_string res_inner ^
                    ", outer typ=" ^ types.typ_to_string res_outer ^
                    ", worklist length=" ^ Int.toString wl_len ^ ")") ^ "\n")
        in
            ()
        end

    (* Test the generate function on a simple integer literal. *)
    fun generate_int_test desc =
        let
            val (ann_exp, constraints) = generate (EInt 3)
            val outer_typ =
                case ann_exp of
                    AInt (_, _, t2, _) => getTyp t2
                | _ => TDyn
            val wl_len = List.length constraints
            val pass = (outer_typ = TInt) andalso (wl_len = 1)
            val _ =
                print (desc ^ ": " ^ (if pass then "PASS" else
                    "FAIL (outer typ=" ^ types.typ_to_string outer_typ ^
                    ", constraints length=" ^ Int.toString wl_len ^ ")") ^ "\n")
        in
            ()
        end

    (* Utility to check if one string is a prefix of another. *)
    fun isPrefix (pre: string) (s: string) : bool =
        let
            val lenPre = String.size pre
        in
            if String.size s < lenPre then false
            else String.substring (s, 0, lenPre) = pre
        end

  (* Run all defined tests.  Each call prints its own result. *)
    fun run_all_tests () =
        (
        unify_test "Unify TInt with TInt" TInt TInt TInt;
        unify_test "Unify TInt with TBool -> TDyn" TInt TBool TDyn;
        unify_test "Unify TNull with TInt" TNull TInt TInt;
        unify_test "Unify TNull with TNull" TNull TNull TNull;
        unify_test "Unify function (TInt->TBool) with (TInt->TInt) -> TFun(TInt,TDyn)"
            (TFun (TInt, TBool)) (TFun (TInt, TInt)) (TFun (TInt, TDyn));
        unify_test "Unify pair (TInt,TBool) with (TInt,TInt) -> TPair(TInt,TDyn)"
            (TPair (TInt, TBool)) (TPair (TInt, TInt)) (TPair (TInt, TDyn));
        add_coerce_test_null "Add coerce with q = TNull" TInt TNull;
        add_coerce_test_non_null "Add coerce with q != TNull" TInt TInt;
        infer_int_test "Infer EInt" 5;
        infer_bool_test "Infer EBool" true;
        infer_plus1_int_test "Infer EPlus1 (int)" 5;
        infer_plus1_bool_test "Infer EPlus1 (bool)";
        infer_neg_bool_test "Infer ENeg (bool)";
        infer_neg_int_test "Infer ENeg (int)";
        infer_var_test "Infer EVar";
        generate_int_test "Generate EInt"
        )

    val _ = run_all_tests ()
end
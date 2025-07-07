structure CS = constraintsyntax
structure U = URef
open CS
open types
open eval
open U
open Ref
open expressions

fun assertTrue (msg, b) = 
    if not b then
        raise Fail ("Assertion failed: " ^ msg)
    else
        ()
fun assertEqual (msg, x, y) =
    if x <> y then
        raise Fail ("Assertion failed: " ^ msg ^ " expected " ^ Int.toString x ^ ", got " ^ Int.toString y)
    else
        ()

fun assertTypeEqual (msg, x, y) =
    if x <> y then
        raise Fail ("Assertion failed: " ^ msg ^ " expected " ^ types.string_of_typ x ^ ", got " ^ types.string_of_typ y)
    else
        ()

(* Test fresh_tvar *)
fun get t = getTyp t

val _ =
    let
        val v1 = fresh_tvar ()
        val v2 = fresh_tvar ()
    in 
        assertTrue ("fresh_tvar should not return the same variable", not (U.eq (v1, v2)));
        assertTrue ("fresh_tvar initial content should be NONE", !! v1 = NONE)
    end

val _ = 
    let 
        val p = fresh_tvar ()
        val q = fresh_tvar ()
    in
        add_coerce (p, q);
        case !worklist of
            (Coerce (p', q')::_) => (
                assertTrue ("add_coerce should add the correct constraint", eq (p, p'));
                assertTrue ("add_coerce should add the correct constraint", eq (q, q')))
          | x => raise Fail ("add_coerce did not add the expected constraint")
    end

val _ =
    let 
        val tv1 = U.uref (SOME TInt)
        val tv2 = U.uref (SOME TBool)
    in 
        assertTypeEqual ("getTyp should return TInt for a tvar with TInt", get tv1, TInt);
        assertTypeEqual ("getTyp should return TDyn for a tvar with NONE", get tv2, TBool)
    end

val _ =
    let 
        val a = U.uref (SOME TInt)
        val b = U.uref (SOME TInt)
        val _ = unifyEq (a, b)
    in
        assertTrue ("unifyEq should unify two equal types", get a = TInt);
        assertTrue ("unifyEq should unify two equal types", get b = TInt);
        assertTypeEqual ("unifyEq should return TInt for both", get a, get b)
    end

val _ =
    let
        val a = U.uref (SOME TInt)
        val b = U.uref (SOME TBool)
        val _ = unifyEq (a, b)
    in
        assertTrue ("unifyEq should unify different types to TDyn", get a = TDyn);
        assertTrue ("unifyEq should unify different types to TDyn", get b = TDyn);
        assertTypeEqual ("unifyEq should return TDyn for both", get a, get b)
    end

val _ =
    let 
        val a = U.uref NONE
        val b = U.uref (SOME TBool)
        val _ = unifyEq (a, b)
    in
        assertTrue ("unifyEq should unify NONE with TBool to TBool", get a = TBool);
        assertTrue ("unifyEq should unify NONE with TBool to TBool", get b = TBool);
        assertTypeEqual ("unifyEq should return TBool for both", get a, get b)
    end

val _ =
    let 
        val a = U.uref (SOME (TFun (TInt, TBool)))
        val b = U.uref (SOME (TFun (TInt, TInt)))
        val _ = unifyEq (a, b)
    in
        assertTrue ("unifyEq should unify two function types", get a = TFun (TInt, TDyn));
        assertTrue ("unifyEq should unify two function types", get b = TFun (TInt, TDyn));
        assertTypeEqual ("unifyEq should return the same function type for both", get a, get b)
    end

val _ =
    let 
        val a = U.uref (SOME (TCouple (TInt, TBool)))
        val b = U.uref (SOME (TCouple (TInt, TInt)))
        val _ = unifyEq (a, b)
    in
        assertTrue ("unifyEq should unify two couple types", get a = TCouple (TInt, TDyn));
        assertTrue ("unifyEq should unify two couple types", get b = TCouple (TInt, TDyn));
        assertTypeEqual ("unifyEq should return the same couple type for both", get a, get b)
    end

val _ =
    let 
        val (exp, cs) = CS.generate (EInt 42)
    in 
        case exp of
            AInt (n, t1, t2) => (
                assertTrue ("AInt should have the correct value", n = 42);
                assertTrue ("The inner and outer tvars should be the same", eq (t1, t2));
                assertTypeEqual ("AInt should have the correct type", get t1, TInt);
                assertTypeEqual ("AInt should have the correct type", get t2, TInt);
                assertTrue ("Constraints should be empty for AInt", (length cs) = 0))
          | _ => raise Fail "Expected AInt expression"
    end

val _ =
    let 
        val (exp, cs) = CS.generate (EBool true)
    in
        case exp of
            ABool (b, t1, t2) => (
                assertTrue ("ABool should have the correct value", b = true);
                assertTrue ("The inner and outer tvars should be the same", eq (t1, t2));
                assertTypeEqual ("ABool should have the correct type", get t1, TBool);
                assertTypeEqual ("ABool should have the correct type", get t2, TBool);
                assertTrue ("Constraints should be empty for ABool", (length cs) = 0))
          | _ => raise Fail "Expected ABool expression"
    end

val _ =
    let 
        val (exp, _, _) = CS.infer ((EVar "x"), [("x", U.uref (SOME TInt), U.uref (SOME TInt), (EVar "x"))])
    in
        case exp of
            AVar (v, t1, t2) => ( 
                assertTrue ("AVar should have the correct variable name", v = "x");
                assertTypeEqual ("AVar should have the correct type", get t1, TInt);
                assertTypeEqual ("AVar should have the correct type", get t2, TInt))
          | _ => raise Fail "Expected AVar expression"
    end

val _ =
    (CS.generate (EVar "y");
        raise Fail "Expected UnboundVariable ")
    handle eval.UnboundVariable(_) => ()

val _ =
    let 
        val (exp, cs) = CS.generate (ELam ("x", TInt, EVar "x"))
    in
        case exp of
            ALam (v, t, body, a, b) => (
                assertTrue ("ALam should have the correct variable name", v = "x");
                assertTypeEqual ("ALam should have the correct type", t, TInt);
                assertTrue ("The inner and outer tvars should be different", not (eq (a, b)));
                assertTypeEqual ("ALam should have the correct type for inner body", get a, TFun (TInt, TInt));
                assertTypeEqual ("ALam should have the correct type for outer body", get b, TFun (TInt, TInt));
                assertTrue ("Constraints should not be empty for ALam", length cs = 1 ))
          | _ => raise Fail "Expected ALam expression"
    end

val _ =
    let 
        val (exp, cs) = CS.generate (EApp (ELam ("x", TInt, EVar "x"), EInt 42))
    in
        case exp of
            AApp (f', arg', a, b) => (
                assertTrue ("The inner and outer tvars should be different", not (eq (a, b)));
                assertTypeEqual ("AApp should have the correct type for inner function", get a, TInt);
                assertTypeEqual ("AApp should have the correct type for outer function", get b, TInt);
                assertTrue ("Constraints should not be empty for AApp", length cs = 2))
          | _ => raise Fail "Expected AApp expression"
    end

val _ =
    let 
        val (exp, cs) = CS.generate (ELet ("x", EInt 42, EVar "x"))
    in
        case exp of
            ALet (x, e1', e2', a, b) => (
                assertTypeEqual ("ALet should have the correct type for e1'", get a, TInt);
                assertTypeEqual ("ALet should have the correct type for e2'", get b, TInt);
                assertTrue ("Constraints should not be empty for ALet", length cs = 1))
          | _ => raise Fail "Expected ALet expression"
    end

val _ =
    let 
        val (exp, cs) = CS.generate (EIf (EBool true, EInt 1, EInt 0))
    in
        case exp of
            AIf (cond, e_then', e_else', a, b) => (
                assertTypeEqual ("AIf should have the correct type for then and else branches", get b, TInt);
                assertTrue ("Constraints should not be empty for AIf", length cs = 1))
          | _ => raise Fail "Expected AIf expression"
    end

val _ =
    let
        val (exp, cs) = CS.generate (ECouple (EInt 1, EBool true))
    in
        case exp of
            ACouple (e1, e2, a, b) => (
                assertTypeEqual ("ACouple should have the correct type for first element", get a, TCouple (TInt, TBool));
                assertTypeEqual ("ACouple should have the correct type for second element", get b, TCouple (TInt, TBool));
                assertTrue ("The inner and outer tvars should be different", not (eq (a, b)));
                assertTrue ("Constraints should not be empty for ACouple", length cs = 1))
          | _ => raise Fail "Expected ACouple expression"
    end

val _ = 
    let
        val (exp, cs) = CS.generate (expressions.EInt 42)
        val s = CS.prettyp exp
    in
        print  ("Program 1 := " ^ s ^ "\n");
        print  ("Coercions: " ^ Int.toString (List.length cs) ^ "\n")
    end


val _ = 
    let
        val (exp, cs) = CS.generate (expressions.EIf(
                        expressions.EBool true,
                        expressions.EInt 1,
                        expressions.EInt 0))
    in
        print  ("Program 2 := " ^ CS.prettyp exp ^ "\n");
        print  ("Coercions: " ^ Int.toString (List.length cs) ^ "\n")
    end

val _ =
    let
        val (exp, cs) = CS.generate (expressions.ELet("x",
                        expressions.EInt 5,
                        expressions.EVar "x"))
    in
        print  ("Program 3 := " ^ CS.prettyp exp ^ "\n");
        print  ("Coercions: " ^ Int.toString (List.length cs) ^ "\n")
    end

val _ =
    let
        val (exp, cs) = CS.generate (expressions.ELam("x", types.TInt,
                        expressions.EVar "x"))
    in
        print  ("Program 4 := " ^ CS.prettyp exp ^ "\n");
        print  ("Coercions: " ^ Int.toString (List.length cs) ^ "\n")
    end

val _ =
    let
        val (exp, cs) = CS.generate (expressions.ECouple(expressions.EInt 3, expressions.EBool false))
    in
        print  ("Program 5 := " ^ CS.prettyp exp ^ "\n");
        print  ("Coercions: " ^ Int.toString (List.length cs) ^ "\n")
    end
    
val _ =
    let
        val (exp, cs) = CS.generate (expressions.ELet("f",
                        (* binding = λx:Int. ⟨x, x⟩ *)
                        expressions.ELam("x", types.TInt,
                        expressions.ECouple(expressions.EVar "x", expressions.EVar "x")
                        ),
                        (* body = f 2 *)
                        expressions.EApp(expressions.EVar "f", expressions.EBool true)))
        val printedwl = CS.prettyp_worklist ()
    in
        print  ("Program 6 := " ^ CS.prettyp exp ^ "\n");
        print  ("Coercions: " ^ Int.toString (List.length cs) ^ "\n");
        print  ("Worklist: " ^ printedwl ^ "\n")
    end
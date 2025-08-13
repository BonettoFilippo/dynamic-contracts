structure TestEvalAnn = struct
  open eval_ann
  open constraintsyntax
  open expressions
  open eval

  (* represent evaluation result as either success with a value or
     error when an exception occurs *)
  datatype ann_test_result = AOk of ann_value | AError

  (* Safely evaluate an annotated expression with a given annotated
     environment, catching runtime exceptions and contract errors. *)
  fun safeEvalAnn (env : ann_env) (e : constraintsyntax.ann_exp) : ann_test_result =
    (AOk (eval_ann env e))
    handle eval.UnboundVariable _ => AError
         | eval.DynamicTypeError _ => AError
         | DynamicTypeContractError _ => AError
         | _ => AError

  (* Equality check between two annotated values. *)
  fun equalAnnValue (AVInt x) (AVInt y) = x = y
    | equalAnnValue (AVBool x) (AVBool y) = x = y
    | equalAnnValue AVNull AVNull = true
    | equalAnnValue (AVPair (x1, x2)) (AVPair (y1, y2)) =
        equalAnnValue x1 y1 andalso equalAnnValue x2 y2
    | equalAnnValue (AVDynamic v1) (AVDynamic v2) = equalAnnValue v1 v2
    | equalAnnValue _ _ = false

  (* Equality check between two test results. *)
  fun equalAnnResult (AOk v1) (AOk v2) = equalAnnValue v1 v2
    | equalAnnResult AError AError = true
    | equalAnnResult _ _ = false

  (* Run a single test case, printing PASS or FAIL with description. *)
  fun run_ann_test (desc, env : ann_env, ann : constraintsyntax.ann_exp, expected : ann_test_result) =
    let
      val actual = safeEvalAnn env ann
    in
      if equalAnnResult actual expected then
        print ("PASS: " ^ desc ^ "\n")
      else
        print ("FAIL: " ^ desc ^ "\n")
    end

  (* Construct lists of test cases for various language features. *)
  val int_literals = List.map (fn i =>
      let
        val (ann, _) = generate (EInt i)
        val env = ([] : ann_env)
      in
        ("int literal " ^ Int.toString i, env, ann, AOk (AVInt i))
      end
    ) [0, ~5, 7]

  val bool_literals = List.map (fn b =>
      let
        val (ann, _) = generate (EBool b)
        val env = ([] : ann_env)
      in
        ("bool literal " ^ Bool.toString b, env, ann, AOk (AVBool b))
      end
    ) [true, false]

  val plus1_success = List.map (fn i =>
      let
        val (ann, _) = generate (EPlus1 (EInt i))
        val env = ([] : ann_env)
      in
        ("plus1 on " ^ Int.toString i, env, ann, AOk (AVInt (i + 1)))
      end
    ) [0, 1, 2, 5]

  val plus1_error = List.map (fn b =>
      let
        val (ann, _) = generate (EPlus1 (EBool b))
        val env = ([] : ann_env)
      in
        ("plus1 on bool " ^ Bool.toString b, env, ann, AError)
      end
    ) [true, false]

  val neg_success = List.map (fn b =>
      let
        val (ann, _) = generate (ENeg (EBool b))
        val env = ([] : ann_env)
      in
        ("negation of " ^ Bool.toString b, env, ann, AOk (AVBool (not b)))
      end
    ) [true, false]

  val neg_error = List.map (fn i =>
      let
        val (ann, _) = generate (ENeg (EInt i))
        val env = ([] : ann_env)
      in
        ("negation of int " ^ Int.toString i, env, ann, AError)
      end
    ) [0, 3]

  val pair_tests = List.map (fn (i, b) =>
      let
        val (ann, _) = generate (EPair (EInt i, EBool b))
        val env = ([] : ann_env)
      in
        ("pair (" ^ Int.toString i ^ ", " ^ Bool.toString b ^ ")", env, ann, AOk (AVPair (AVInt i, AVBool b)))
      end
    ) [(1, true), (0, false)]

  val nested_pair_tests =
    let
      val (ann, _) = generate (EPair (EInt 3, EPair (EBool false, EInt 7)))
      val env = ([] : ann_env)
    in
      [("nested pair", env, ann, AOk (AVPair (AVInt 3, AVPair (AVBool false, AVInt 7))))]
    end

  val let_tests =
    let
      val (ann1, _) = generate (ELet ("x", EInt 4, EVar "x"))
      val (ann2, _) = generate (ELet ("x", EInt 5, EPlus1 (EVar "x")))
      val (ann3, _) = generate (ELet ("x", EBool true, ENeg (EVar "x")))
      val (ann4, _) = generate (ELet ("x", EInt 3, ENeg (EVar "x")))
      val env = ([] : ann_env)
    in
      [
        ("let binding int", env, ann1, AOk (AVInt 4)),
        ("let plus1", env, ann2, AOk (AVInt 6)),
        ("let neg bool", env, ann3, AOk (AVBool false)),
        ("let neg int error", env, ann4, AError)
      ]
    end

  val if_tests =
    let
      val (ann1, _) = generate (EIf (EBool true, EInt 1, EInt 0))
      val (ann2, _) = generate (EIf (EBool false, EInt 1, EInt 0))
      val (ann3, _) = generate (EIf (EInt 1, EInt 1, EInt 0))
      val env = ([] : ann_env)
    in
      [
        ("if true branch", env, ann1, AOk (AVInt 1)),
        ("if false branch", env, ann2, AOk (AVInt 0)),
        ("if non-bool cond", env, ann3, AError)
      ]
    end

  val var_tests =
    let
      val t1 = fresh_tvar ()
      val t2 = fresh_tvar ()
      val t3 = fresh_tvar ()
      val t4 = fresh_tvar ()
      val ann_var_success = AVar ("x", t1, t2, 0)
      val ann_var_error = AVar ("y", t3, t4, 0)
      val env_success = [("x", AVInt 10)] : ann_env
      val env_empty = [] : ann_env
    in
      [
        ("var lookup success", env_success, ann_var_success, AOk (AVInt 10)),
        ("var lookup error", env_empty, ann_var_error, AError)
      ]
    end

  val app_tests =
    let
      val (ann1, _) = generate (EApp (ELam ("x", EVar "x"), EInt 5))
      val (ann2, _) = generate (EApp (ELam ("x", EVar "x"), EBool false))
      val (ann3, _) = generate (EApp (ELam ("x", EPlus1 (EVar "x")), EInt 2))
      val (ann4, _) = generate (EApp (EInt 3, EInt 4))
      val (ann5, _) = generate (EApp (ELam ("x", EPlus1 (EVar "x")), EBool true))
      val env = ([] : ann_env)
    in
      [
        ("apply identity", env, ann1, AOk (AVInt 5)),
        ("apply bool identity", env, ann2, AOk (AVBool false)),
        ("apply plus1 function", env, ann3, AOk (AVInt 3)),
        ("apply non-function", env, ann4, AError),
        ("apply plus1 to bool (contract error)", env, ann5, AError)
      ]
    end

  (* Combine all test cases into one list *)
  val testcases =
    int_literals @ bool_literals @ plus1_success @ plus1_error @
    neg_success @ neg_error @ pair_tests @ nested_pair_tests @
    let_tests @ if_tests @ var_tests @ app_tests

  (* Execute all tests *)
  val _ = List.app run_ann_test testcases
end
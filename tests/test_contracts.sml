structure TestContracts = struct

  open contracts
  open expressions

  structure EA = eval_ann
  structure EV = eval

  (* An expected outcome for a test. *)
  datatype expected =
      ExpVal of EA.ann_value
    | ExpError
    | ExpClosure

  (* Compare two annotated values for equality. *)
  fun equalAnnValue (v1: EA.ann_value, v2: EA.ann_value) : bool =
    case (v1, v2) of
        (EA.AVInt n1, EA.AVInt n2) => n1 = n2
      | (EA.AVBool b1, EA.AVBool b2) => b1 = b2
      | (EA.AVPair (a1, b1), EA.AVPair (a2, b2)) =>
          equalAnnValue (a1, a2) andalso equalAnnValue (b1, b2)
      | (EA.AVClosure _, EA.AVClosure _) => true
      | (EA.AVDynamic _, EA.AVDynamic _) => true
      | _ => false

  (* A result type for safe execution: Ok carries the returned
     annotated value, Error carries an error message. *)
  datatype result = Ok of EA.ann_value | Error of string

  (* Execute a source expression with contract handling. *)
  fun safeExec (e: expressions.exp) : result =
    (Ok (contracts.execute e)) handle
        EV.DynamicTypeError (idx, msg) => Error ("DynamicTypeError: " ^ Int.toString idx ^ " " ^ msg)
      | EA.DynamicTypeContractError (idx, env, msg, lst) => Error ("DynamicTypeContractError: " ^ msg)
      | ExpressionNotFound i => Error ("ExpressionNotFound: " ^ Int.toString i)
      | UnexpectedExpression _ => Error ("UnexpectedExpression")
      | EV.UnboundVariable x => Error ("UnboundVariable: " ^ x)
      | ex => Error ("Other exception raised")

  (* Compare an actual result with an expected outcome.  Returns true
     if they match. *)
  fun matches (actual: result, exp: expected) : bool =
    case (actual, exp) of
        (Ok v, ExpVal e) => equalAnnValue (e, v)
      | (Error _, ExpError) => true
      | (Ok (EA.AVClosure _), ExpClosure) => true
      | _ => false

  (* Pretty print an expected outcome for diagnostic output. *)
  fun expectedToString (exp: expected) : string =
    case exp of
        ExpVal (EA.AVInt n) => "AVInt " ^ Int.toString n
      | ExpVal (EA.AVBool b) => "AVBool " ^ Bool.toString b
      | ExpVal (EA.AVPair _) => "AVPair"
      | ExpVal (EA.AVDynamic _) => "AVDynamic"
      | ExpVal (EA.AVClosure _) => "AVClosure"
      | ExpError => "error"
      | ExpClosure => "closure"

  (* Pretty print an actual result for diagnostic output. *)
  fun resultToString (res: result) : string =
    case res of
        Ok (EA.AVInt n) => "AVInt " ^ Int.toString n
      | Ok (EA.AVBool b) => "AVBool " ^ Bool.toString b
      | Ok (EA.AVPair _) => "AVPair"
      | Ok (EA.AVDynamic _) => "AVDynamic"
      | Ok (EA.AVClosure _) => "AVClosure"
      | Error msg => "Error(" ^ msg ^ ")"

  (* Run a single test, printing PASS or FAIL. *)
  fun run_test ((name, exp, expVal) : string * expressions.exp * expected) =
    let
      val actual = safeExec exp
      val ok = matches (actual, expVal)
    in
      if ok then
        print ("PASS: " ^ name ^ "\n")
      else
        print ("FAIL: " ^ name ^ "\n  expected: " ^ expectedToString expVal ^ "\n  got: " ^ resultToString actual ^ "\n")
    end

  (* Construct a few annotated values for convenience in expected results. *)
  val avInt1 : EA.ann_value = EA.AVInt 1
  val avInt2 : EA.ann_value = EA.AVInt 2
  val avInt3 : EA.ann_value = EA.AVInt 3
  val avInt4 : EA.ann_value = EA.AVInt 4
  val avBoolTrue : EA.ann_value = EA.AVBool true
  val avBoolFalse : EA.ann_value = EA.AVBool false
  val avPairIntBool : EA.ann_value = EA.AVPair (EA.AVInt 1, EA.AVBool true)
  val avNestedPair : EA.ann_value = EA.AVPair (EA.AVInt 1, EA.AVPair (EA.AVInt 2, EA.AVBool false))

  (* Define the list of test cases.  Each tuple contains a human
     readable name, the source expression to evaluate, and the
     expected outcome. *)
  val testcases : (string * expressions.exp * expected) list =
    [
      ("Integer literal", EInt 3, ExpVal avInt3),
      ("Boolean literal", EBool true, ExpVal avBoolTrue),
      ("Unary +1 on int", EPlus1 (EInt 1), ExpVal avInt2),
      ("Unary +1 on bool (error)", EPlus1 (EBool true), ExpError),
      ("Negation on bool", ENeg (EBool true), ExpVal avBoolFalse),
      ("Negation on int (error)", ENeg (EInt 0), ExpError),
      ("Pair of int and bool", EPair (EInt 1, EBool true), ExpVal avPairIntBool),
      ("Nested pair", EPair (EInt 1, EPair (EInt 2, EBool false)), ExpVal avNestedPair),
      ("Let binding integer", ELet ("x", EInt 5, EPlus1 (EVar "x")), ExpVal (EA.AVInt 6)),
      ("Let binding with type error", ELet ("x", EBool true, EPlus1 (EVar "x")), ExpError),
      ("If true branch", EIf (EBool true, EInt 1, EInt 2), ExpVal avInt1),
      ("If false branch", EIf (EBool false, EInt 1, EInt 2), ExpVal avInt2),
      ("If with non‑bool condition", EIf (EInt 0, EInt 1, EInt 2), ExpError),
      ("Identity function", ELam ("x", EVar "x"), ExpClosure),
      ("Apply identity to int", EApp (ELam ("x", EVar "x"), EInt 3), ExpVal avInt3),
      ("Apply identity to bool", EApp (ELam ("x", EVar "x"), EBool false), ExpVal avBoolFalse),
      ("Plus1 function applied to int", EApp (ELam ("x", EPlus1 (EVar "x")), EInt 3), ExpVal avInt4),
      ("Plus1 function applied to bool (error)", EApp (ELam ("x", EPlus1 (EVar "x")), EBool true), ExpError),
      ("Negation function applied to bool", EApp (ELam ("x", ENeg (EVar "x")), EBool false), ExpVal avBoolTrue),
      ("Negation function applied to int (error)", EApp (ELam ("x", ENeg (EVar "x")), EInt 3), ExpError),
      ("Apply non‑function (error)", EApp (EInt 3, EInt 4), ExpError),
      ("Dynamic nested through identity", EApp (ELam ("x", EVar "x"), EApp (ELam ("y", EPlus1 (EVar "y")), EBool true)), ExpError),
      ("Both input and output mismatch", EApp (ELam ("x", EPlus1 (EVar "x")), EBool false), ExpError),
      ("Opposite mismatch (negation with int)", EApp (ELam ("x", ENeg (EVar "x")), EInt 5), ExpError)
    ]

  (* Execute all tests. *)
  val _ = List.app run_test testcases

end
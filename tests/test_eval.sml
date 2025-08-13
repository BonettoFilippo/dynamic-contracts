open eval
open expressions

(* represent evaluation result as either Ok with a value or Error when an exception occurs *)
datatype test_result = Ok of value | Error

(* Safely evaluate an expression with an environment, catching runtime exceptions *)
fun safeEval (env : env) (exp : exp) : test_result =
    (Ok (eval env exp))
    handle UnboundVariable _ => Error
         | DynamicTypeError _ => Error

(* Equality check between two values; ignores closure comparison *)
fun equalValue (VInt x) (VInt y) = x = y
  | equalValue (VBool x) (VBool y) = x = y
  | equalValue (VDynamic v1) (VDynamic v2) = equalValue v1 v2
  | equalValue (VPair (x1, x2)) (VPair (y1, y2)) = equalValue x1 y1 andalso equalValue x2 y2
  | equalValue VNull VNull = true
  | equalValue _ _ = false

(* Equality check between two test results *)
fun equalResult (Ok v1) (Ok v2) = equalValue v1 v2
  | equalResult Error Error = true
  | equalResult _ _ = false

(* Run a single test case, printing PASS or FAIL with description *)
fun run_test (desc, env, expr, expected) =
    let
        val actual = safeEval env expr
    in
        if equalResult actual expected then
            print ("PASS: " ^ desc ^ "\n")
        else
            print ("FAIL: " ^ desc ^ "\n")
    end

(* Construct lists of test cases for various language features *)
val int_literals = List.map (fn i => ("int literal " ^ Int.toString i, ([] : env), EInt i, Ok (VInt i))) [0, ~5, 7]

val bool_literals = List.map (fn b => ("bool literal " ^ Bool.toString b, ([] : env), EBool b, Ok (VBool b))) [true, false]

val plus1_success = List.map (fn i => ("plus1 on " ^ Int.toString i, ([] : env), EPlus1 (EInt i), Ok (VInt (i+1)))) [0, 1, 2, 5]

val plus1_error = List.map (fn b => ("plus1 on bool " ^ Bool.toString b, ([] : env), EPlus1 (EBool b), Error)) [true, false]

val neg_success = List.map (fn b => ("negation of " ^ Bool.toString b, ([] : env), ENeg (EBool b), Ok (VBool (not b)))) [true, false]

val neg_error = List.map (fn i => ("negation of int " ^ Int.toString i, ([] : env), ENeg (EInt i), Error)) [0, 3]

val pair_tests = List.map (fn (i,b) => ("pair (" ^ Int.toString i ^ ", " ^ Bool.toString b ^ ")", ([] : env), EPair (EInt i, EBool b), Ok (VPair (VInt i, VBool b)))) [(1,true),(0,false)]

val nested_pair_tests =
    [("nested pair", ([] : env), EPair (EInt 3, EPair (EBool false, EInt 7)),
      Ok (VPair (VInt 3, VPair (VBool false, VInt 7))))]

val let_tests =
    [
        ("let binding int", ([] : env), ELet ("x", EInt 4, EVar "x"), Ok (VInt 4)),
        ("let plus1", ([] : env), ELet ("x", EInt 5, EPlus1 (EVar "x")), Ok (VInt 6)),
        ("let neg bool", ([] : env), ELet ("x", EBool true, ENeg (EVar "x")), Ok (VBool false)),
        ("let neg int error", ([] : env), ELet ("x", EInt 3, ENeg (EVar "x")), Error)
    ]

val if_tests =
    [
        ("if true branch", ([] : env), EIf (EBool true, EInt 1, EInt 0), Ok (VInt 1)),
        ("if false branch", ([] : env), EIf (EBool false, EInt 1, EInt 0), Ok (VInt 0)),
        ("if non-bool cond", ([] : env), EIf (EInt 1, EInt 1, EInt 0), Error)
    ]

val var_tests =
    [
        ("var lookup success", [("x", VInt 10)], EVar "x", Ok (VInt 10)),
        ("var lookup error", ([] : env), EVar "y", Error)
    ]

val app_tests =
    [
        ("apply identity", ([] : env), EApp (ELam ("x", EVar "x"), EInt 5), Ok (VInt 5)),
        ("apply bool identity", ([] : env), EApp (ELam ("x", EVar "x"), EBool false), Ok (VBool false)),
        ("apply plus1 function", ([] : env), EApp (ELam ("x", EPlus1 (EVar "x")), EInt 2), Ok (VInt 3)),
        ("apply non-function", ([] : env), EApp (EInt 3, EInt 4), Error)
    ]

(* Combine all test cases into one list *)
val testcases = int_literals @ bool_literals @ plus1_success @ plus1_error @ neg_success @ neg_error @ pair_tests @ nested_pair_tests @ let_tests @ if_tests @ var_tests @ app_tests

(* Execute all tests *)
val _ = List.app run_test testcases

open eval
open expressions
open types
(* A simple function to convert values to strings for testing *)
fun string_of_value (VInt n) = Int.toString n
  | string_of_value (VBool b) = Bool.toString b
  | string_of_value (VClosure (_, _, _, _)) = "<closure>"
  | string_of_value (VDynamic v) = "Dynamic(" ^ string_of_value v ^ ")"
  | string_of_value (VError msg) = "Error(" ^ msg ^ ")"
  | string_of_value (VCouple (v1, v2)) = "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"

(* Test helper that prints a description and then the evaluated value *)
fun test (desc: string) (e: exp) =
	let
		val v = run e
	in
		print (desc ^ ": " ^ string_of_value v ^ "\n")
	end

(* Now we define some test expressions *)
fun test_eval () =
    let
		(* Test integer literal *)
		val _ = test "Test int literal" (EInt 42)
		
		(* Test boolean literal *)
		val _ = test "Test boolean literal" (EBool true)
		
		(* Test variable lookup: we'll use an environment in the evaluator, but run uses [].
				So, to test Var we can extend the environment manually. *)
		val _ =
			let val v = eval.eval [("x", VInt 10)] (EVar "x") handle
				UnboundVariable x => VError ("Test variable lookup: unbound variable " ^ x ^ "\n")
       		in
				case v of
					VError msg => print msg
				  | _ => print ("Test variable lookup: " ^ string_of_value v ^ "\n")
			end
      
		(* Test lambda (closure) creation *)
		val _ = test "Test lambda (closure)" (ELam ("x", TInt, EVar "x"))
		
		(* Test function application:
				We'll create a lambda that adds 1 to its argument and apply it to an integer *)
		val addOne = ELam ("x", TInt, (* here we assume a primitive for addition; if not, we can simulate it by returning a constant *) 
                              EInt  (1))  (* For simplicity, we return 1 as a stub; you can adjust this later *)
      	val _ = test "Test application (stub addOne)" (EApp (addOne, EInt 100))
      
		(* Test if expression: if true then 1 else 0 *)
		val _ = test "Test if (true)" (EIf (EBool true, EInt 1, EInt 0))
		val _ = test "Test if (false)" (EIf (EBool false, EInt 1, EInt 0))
		
		(* Test error: applying a non-function *)
		val _ =
			let val v = (run (EApp (EInt 3, EInt 4))) handle
				DynamicTypeError x => VError ("Test error (apply non-function) caught: " ^ x ^ "\n")
       		in
				case v of
					VError msg => print msg
				  | _ => print ("Test error (apply non-function) returned: " ^ string_of_value v ^ "\n")
			end

		(* Test error: unbound variable *)
		val _ =
			let val v = run (EVar "z") handle
				UnboundVariable x => VError ("Test error (unbound variable) caught: " ^ x ^ "\n")
       		in
				case v of
					VError msg => print msg
				  | _ => print ("Test error (unbound variable) returned: " ^ string_of_value v ^ "\n")
			end

      	(* Test cast (stub implementation) *)
      	val _ = test "Test cast (stub)" (ECast (EInt 7, TInt))
    
		(* Test couple literal:
       For example, a couple containing 3 and false *)
    val _ = test "Test couple literal" (ECouple (EInt 3, EBool false))
    
    (* Test nested couple:
       For instance, representing a triple as (3, (false, 7)) *)
    val _ = test "Test nested couple" (ECouple (EInt 3, ECouple (EBool false, EInt 7)))
	in
  		()
    end

val _ = test_eval ()

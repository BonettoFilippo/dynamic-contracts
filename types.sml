(* types signature *)

structure types : TYPES = 
struct

	(* a type is either an int, a bool, a dynamic type, a Null, a function type, or a pair type *)
    datatype typ = 
		TInt
	  | TBool
	  | TDyn
	  | TNull
	  | TFun of typ * typ
	  | TPair of typ * typ

	(* a function that converts types into human readable strings *)
    fun typ_to_string TInt = "int"
      | typ_to_string TBool = "bool"
      | typ_to_string TDyn = "dyn"
      | typ_to_string TNull = "null"
      | typ_to_string (TFun (t1, t2)) = "(" ^ typ_to_string t1 ^ " -> " ^ typ_to_string t2 ^ ")"
	  | typ_to_string (TPair (t1, t2)) = "(" ^ typ_to_string t1 ^ ", " ^ typ_to_string t2 ^ ")"
end;
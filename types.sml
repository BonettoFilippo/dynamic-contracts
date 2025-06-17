(* types signature *)

structure types : TYPES = 
struct

	(* a type is either an int, a bool, a dynamic type, a function type, or a pair type *)
    datatype typ = 
		TInt
	  | TBool
	  | TDyn
	  | TFun of typ * typ
	  | TCouple of typ * typ

	(* a function that converts types into human readable strings *)
    fun string_of_typ TInt = "int"
      | string_of_typ TBool = "bool"
      | string_of_typ TDyn = "dyn"
      | string_of_typ (TFun (t1, t2)) = "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
	  | string_of_typ (TCouple (t1, t2)) = "(" ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ ")"
end;
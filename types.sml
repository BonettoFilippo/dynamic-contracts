(* types signature *)

structure types : TYPES = 
struct

    datatype typ = 
          TInt
        | TBool
        | TDyn
        | TFun of typ * typ

    fun string_of_typ TInt = "int"
      | string_of_typ TBool = "bool"
      | string_of_typ TDyn = "dyn"
      | string_of_typ (TFun (t1, t2)) = "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
    
end;
signature TYPES = 
sig
    (* a type is either an int, a bool, a dynamic type, a function type, or a pair type *)
    datatype typ = 
        TInt
      | TBool
      | TDyn
      | TFun of typ * typ
      | TCouple of typ * typ

    (* a function that converts type into human readable strings *)
    val string_of_typ : typ -> string
    
end;
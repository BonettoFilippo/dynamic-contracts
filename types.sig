signature TYPES = 
sig
    (* a type is either an int, a bool, a dynamic type, a function type, or a pair type *)
    datatype typ = 
        TInt
      | TBool
      | TDyn
      | TNull
      | TFun of typ * typ
      | TPair of typ * typ

    (* a function that converts type into human readable strings *)
    val typ_to_string : typ -> string
    
end;
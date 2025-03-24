signature TYPES = 
sig
    datatype typ = 
        TInt
      | TBool
      | TDyn
      | TFun of typ * typ
      | TCouple of typ * typ

    (* a function that converts type into human readable strings *)
    val string_of_typ : typ -> string
    
end;
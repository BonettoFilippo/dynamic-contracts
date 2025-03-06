(* the abstract syntax of the source langauge *)
(* contains definitions for types and expressions *)

open types

structure abstract_syntax = struct 
    (*  
    datatype typ = 
          TInt
        | TBool
        | TDyn
        | TFun of typ * typ (* input and output type *)
    *)

    (* the end keyword is important to allow recursive data types which will be important with refinement types *)
    datatype exp = 
          EInt of int
        | EBool of bool
        | EVar of string
        | ELam of string * typ * exp
        | EApp of exp * exp
        | EIf of exp * exp * exp
        | ECoerce of exp * typ
end;
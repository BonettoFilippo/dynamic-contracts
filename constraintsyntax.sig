signature CONSTRAINTSYNTAX =
sig
  structure U : UREF

  (* Type of type‐variable references *)
  type tvar = types.typ option U.uref
  (* Type environment mapping identifiers to type‐variable refs *)
  type tenv = (string * tvar * tvar * expressions.exp) list

  (* Constraints collected during inference *)
  datatype constraint = Coerce of tvar * tvar
  val worklist : constraint list ref

  (* Create a fresh type‐variable *)
  val fresh_tvar : unit -> tvar
  (* Record a coercion constraint *)
  val add_coerce  : tvar * tvar -> unit
  (* Read off the current type in a tvar, defaulting to TDyn *)
  val getTyp      : tvar -> types.typ
  (* Unify two tvars, returning the resulting type if any *)
  val unifyEq     : tvar * tvar -> unit

  (* Annotated expression with inner/outer type‐var refs *)
  datatype ann_exp =
      AInt    of int                 * tvar * tvar
    | ABool   of bool                * tvar * tvar
    | AVar    of string              * tvar * tvar
    | ALam    of string * types.typ  * ann_exp * tvar * tvar
    | AApp    of ann_exp * ann_exp   * tvar * tvar
    | ALet    of string * ann_exp * ann_exp * tvar * tvar
    | AIf     of ann_exp * ann_exp * ann_exp * tvar * tvar
    | ACast   of ann_exp * types.typ * tvar * tvar
    | ACouple of ann_exp * ann_exp   * tvar * tvar

  (* Infer annotated AST, returning it plus its inner/outer tvars *)
  val infer    : expressions.exp * tenv -> ann_exp * tvar * tvar
  (* Run inference on an expression, returning the annotated tree
     and the list of accumulated constraints *)
  val generate : expressions.exp -> ann_exp * constraint list

  (* a function to convert a tvar to a string representation *)
  val string_of_tvar : tvar -> string 
  (* pretty preting function for annotations *)
  val prettyp : ann_exp -> string 
  (* pretty preting function for the worklist *)
  val prettyp_worklist : unit -> string
end

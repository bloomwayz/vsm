open Syntax

exception Unification_error_with_loc of string * Location.t
exception Unbound_variable of string * Location.t

type ty
type ty_scheme

type subs = ty -> ty

val string_of_ty : ty -> string
val m_ty_of_ty : ty -> typ
val check_top : expr -> ty
val check_sub : expr -> expr -> ty
val string_of_typ : typ -> string
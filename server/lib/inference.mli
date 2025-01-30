open Syntax

exception Unification_error_with_loc of string * Location.t
exception Unbound_variable of string * Location.t

type ty
type ty_scheme

module Var = String

module Ty_env : sig
  include Map.S with type key = Var.t
end

type tyenv = ty_scheme Ty_env.t
type subs = ty -> ty

val m_ty_of_ty : ty -> typ
val check_top : expr -> typ
val check_sub : expr -> expr -> typ
val get_env_subs : expr -> tyenv * subs
val string_of_ty : typ -> string
val find_var : id -> tyenv -> ty option

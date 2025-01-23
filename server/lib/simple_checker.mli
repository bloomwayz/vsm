(*
 * SNU 4190.310 Programming Languages
 * Type Checker Interface File
 *)

open Syntax

exception Unification_error_with_loc of string * Location.t
exception Unbound_variable of string * Location.t

val check_top : expr -> typ
val check_sub : expr -> expr -> typ
val string_of_ty : typ -> string

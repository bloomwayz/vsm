(*
 * SNU 4190.310 Programming Languages
 * Type Checker Interface File
 *)

open Syntax

val check_top : expr -> typ
val check_sub : expr -> expr -> typ
val string_of_ty : typ -> string

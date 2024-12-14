(*
 * SNU 4190.310 Programming Languages
 * Type Checker Interface File
 *)

open Syntax

val check : expr -> typ
val string_of_ty : typ -> string

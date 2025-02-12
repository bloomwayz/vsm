(*
 * M Language Server
 * Type Inferer
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Str
open Language.Poly_checker

let check_top = check_top

let check_sub = check_sub

let string_of_ty = string_of_ty

let undisclose s =
  let count = ref 0 in
  let dict = [ "'a"; "'b"; "'c"; "'d"; "'e"; "'f"; "'g" ] in
  let r = Str.regexp {|'a[0-9]+|} in
  let rec collect s i =
    match Str.search_forward r s i with
    | i ->
        let sub = Str.matched_string s in
        let subr = Str.regexp sub in
        let s = Str.global_replace subr (List.nth dict !count) s in
        let _ = count := 1 in
        collect s (i + 1)
    | exception Not_found -> s
  in
  collect s 0
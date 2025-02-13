(*
 * M Language Server
 * Type Inferer
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Str
open Range
open Document
open Language.Syntax
open Language.Poly_checker

exception Unbound_variable = Unbound_variable
exception Unification_error_with_loc = Unification_error_with_loc

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

let rec traverse_ast (exp : expr) (acc : expr list) =
  match exp.desc with
  | Const _ | Var _ | Read -> (exp :: acc)
  | Fn (_, e) | Write e | Fst e | Snd e | Malloc e | Deref e ->
      traverse_ast e (exp :: acc)
  | App (e1, e2)
  | Bop (_, e1, e2)
  | Assign (e1, e2)
  | Seq (e1, e2)
  | Pair (e1, e2)
  | Let (Val (_, e1), e2)
  | Let (Rec (_, _, e1), e2) ->
      let acc' = traverse_ast e1 (exp :: acc) in
      traverse_ast e2 acc'
  | If (e1, e2, e3) ->
      let acc' = traverse_ast e1 (exp :: acc) in
      let acc'' = traverse_ast e2 acc' in
      traverse_ast e3 acc''

let in_range (pos : Position.t) (exp : expr) =
  let ln = pos.ln in
  let col = pos.col in

  let range = Range.from_location (exp.loc) in
  let start, end_ = range.start, range.end_ in
  let sln, scl = start.ln, start.col in
  let eln, ecl = end_.ln, end_.col in

  (sln < ln && ln < eln) ||
  (sln = ln && ln < eln && scl <= col) ||
  (sln < ln && ln = eln && col <= ecl) ||
  (sln = ln && ln = eln && scl <= col && col <= ecl)

let subexp_at_pos (ast : expr) (pos : Position.t) =
  let subexps = traverse_ast ast [] in
  let subexps' = List.filter (in_range pos) subexps in
  List.nth_opt subexps' 0

let slice txt lnum cnum =
  let r = Str.regexp "\n" in
  let txtlen = String.length txt in

  let rec compute s i ln =
    match Str.search_forward r s i with
    | i -> if ln = lnum then i else compute s i (ln + 1)
    | exception Not_found -> failwith "Not_found"
  in

  let start = compute txt 0 0 in

  String.sub txt start (txtlen - start)

let in_letval_range (pgmtxt : string) (exp : expr) (pos : Position.t) =
  let ln, col = pos.ln, pos.col in

  let range = Range.from_location exp.loc in
  let sln, scl = range.start.ln, range.start.col in

  let s = slice pgmtxt sln scl in
  let r = Str.regexp {|let val |} in

  let start =
    match Str.search_forward r s 0 with
    | x -> scl + String.length (Str.matched_string s)
    | exception Not_found -> scl + 7
  in

  let id =
    match exp.desc with
    | Let (Val (x, _), _) -> x
    | _ -> failwith "no way!" in

  let end_ = start + String.length id in

  if start <= col && col <= end_ then Some (ln, start, end_) else None

let in_letrec_range (pgmtxt : string) (exp : expr) (lnum : int) (cnum : int) =
  let loc = exp.loc in
  let _, sln, scol = Location.get_pos_info loc.loc_start in

  let s = slice pgmtxt sln scol in
  let r = Str.regexp {|let rec |} in
  let r' = Str.regexp {| = |} in

  let f_start =
    match Str.search_forward r s 0 with
    | x -> scol + String.length (Str.matched_string s)
    | exception Not_found -> scol + 7
  in

  let f, x =
    match exp.desc with
    | Let (Rec (f, x, _), _) -> f, x
    | _ -> failwith "no way!" in

  let f_end = f_start + String.length f in

  let x_start =
    match Str.search_forward r' s f_end with
    | x -> scol + String.length (Str.matched_string s)
    | exception Not_found -> scol + 10
  in
  
  let x_end = x_start + String.length x in

  if f_start <= cnum && cnum <= f_end
    then Some (lnum, f_start, f_end)
  else if x_start <= cnum && cnum <= x_end
    then Some (lnum, x_start, x_end)
  else None
  
let in_fn_range (pgmtxt : string) (exp : expr) (lnum : int)
    (cnum : int) =
  let loc = exp.loc in
  let _, sln, scol = Location.get_pos_info loc.loc_start in

  let s = slice pgmtxt sln scol in
  let r = Str.regexp {|fn |} in

  let start =
    match Str.search_forward r s 0 with
    | x -> scol + String.length (Str.matched_string s)
    | exception Not_found -> scol + 3
  in

  let id = match exp.desc with Fn (x, _) -> x | _ -> failwith "no way!" in
  let end_ = start + String.length id in

  if start <= cnum && cnum <= end_ then Some (lnum, start, end_) else None

let infer_sub (st : States.state) (exp : expr) (curr_pos : Position.t) =
  let pgmtxt = st.rawState in
  let ast = match st.parsedState with
    | Ast x -> x
    | Fail _ -> failwith "meaningless branch; refactor needed"
  in
  let range = Range.from_location exp.loc in
  let lnum, cnum = curr_pos.ln, curr_pos.col in

  match exp.desc with
  | Const (String _) -> Some ("string", range)
  | Const (Int _) -> Some ("int", range)
  | Const (Bool _) -> Some ("bool", range)
  | Let (Val (id, e1), e2) -> (
      let range_opt = in_letval_range pgmtxt exp curr_pos in
      match range_opt with
      | Some (lnum, start, end_) ->
          let range = Range.from_tuples (lnum, start) (lnum, end_) in
          let fty_str =
            try
              let fty = check_sub ast e1 in
              string_of_ty fty
            with Unification_error_with_loc (msg, _) -> msg
          in
          Some (fty_str, range)
      | None -> (
          match check_sub ast exp with
          | x -> Some (string_of_ty x, range)
          | exception _ -> None) )
  | Let (Rec (f, x, e1), e2) -> 
      None
  | Fn (id, expr) -> (
      let range_opt = in_fn_range pgmtxt exp lnum cnum in
      match range_opt with
      | Some (lnum, start, end_) ->
          let range = Range.from_tuples (lnum, start) (lnum, end_) in
          let fty_str =
            try
              let fty = check_sub ast exp in
              let fty = string_of_ty fty in
              let r = Str.regexp {| -> |} in
              let i = Str.search_forward r fty 0 in
              String.sub fty 0 i
            with Unification_error_with_loc (msg, _) -> msg
          in

          Some (fty_str, range)
      | None -> (
          match check_sub ast exp with
          | x -> Some (string_of_ty x, range)
          | exception _ -> None))
  | _ -> (
      match check_sub ast exp with
      | x -> Some (string_of_ty x, range)
      | exception _ -> None)
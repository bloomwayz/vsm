(*
 * M Language Server
 * CodeLens Provider
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Ppx_yojson_conv_lib.Yojson_conv
open Yojson.Safe.Util
open Protocol
open Document
open Range
open Inference

module CodeLensResult = struct
  type t = codelens list
  [@@deriving yojson]

  and codelens =
    { range : Range.t
    ; command : command }
  [@@deriving yojson]

  and command =
    { title : string
    ; command : string }
  [@@deriving yojson]

  let create ~range ~title : t =
    let command = { title; command = "" } in
    [ { range; command } ]
end

let sprint_top ast =
  match (check_top ast) with
  | ty -> undisclose (string_of_ty ty)
  | exception _ -> ""

let get_title : States.pstate -> string = function
  | Ast ast -> sprint_top ast
  | Fail _ -> ""

let compute params =
  let uri = get_uri params in
  let pstate =
    match findp uri with
    | Some x -> x
    | None -> failwith "Lookup failure"
  in
  let range = Range.from_tuples (0, 0) (0, 0) in
  let title = get_title pstate in
  CodeLensResult.create ~range ~title

let run id params =
  let result_ = compute params in
  let result = CodeLensResult.yojson_of_t result_ in
  send (Resp { id; result })
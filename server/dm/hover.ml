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

module HoverResult = struct
  type t = result option [@@yojson.option]
  and result = { contents : contents; range : Range.t }
  and contents = { kind : string; value : string } [@@deriving yojson]

  let markup value = "```ocaml\n" ^ value ^ "\n```"

  let create ~value ~range : t =
    let kind = "markdown" in
    let value = markup value in
    let contents = { kind; value } in
    Some { contents; range }
end

let compute params =
  let uri = get_uri params in
  let curr_pos = params |> member "position" |> Position.t_of_yojson in
  let st =
    match finds uri with Some st -> st | None -> failwith "Lookup failure"
  in
  match st.parsedState with
  | Ast ast -> (
      match infer_sub st ast curr_pos with
      | Some (value, range) -> HoverResult.create ~value ~range
      | None -> None)
  | Fail _ -> None

let run id params =
  let result_ = compute params in
  let result = HoverResult.yojson_of_t result_ in
  send (Resp { id; result })

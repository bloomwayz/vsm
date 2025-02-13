(*
 * M Language Server
 * Semantic Tokens Provider
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Ppx_yojson_conv_lib.Yojson_conv
open Yojson.Safe.Util
open Protocol
open Document
open Range
open Language

module SemanticTokens = struct
  type t = result option
  [@@yojson.option]

  and result = { data : data }

  and data = int list
  [@@deriving yojson]

  let create ~data : t = Some { data }
end

module TokenType = struct
  type t =
    | Enum
    | Parameter
    | Variable
    | Function
    | Keyword
    | Comment
    | String_
    | Number
    | Operator

  let to_int = function
    | Enum -> 0
    | Parameter -> 1
    | Variable -> 2
    | Function -> 3
    | Keyword -> 4
    | Comment -> 5
    | String_ -> 6
    | Number -> 7
    | Operator -> 8
end

module Token = struct
  type t =
    { ln : int
    ; col : int
    ; len : int
    ; type_ : int
    ; modif : int }

  let compare x y =
    if x.ln != y.ln then y.ln - x.ln
    else y.col - x.col

  let sort = List.sort compare

  let make ~ln ~col ~len ~type_ : t =
    let type_ = TokenType.to_int type_ in
    let modif = 0 in
    { ln; col; len; type_; modif }

  let from_range (r : Range.t) =
    let ln = r.start.ln in
    let col = r.start.col in
    let len = r.end_.col - r.start.col in
    make ~ln ~col ~len

  let to_data (tokens : t list) : SemanticTokens.data =
    let rec deltaize acc lst =
      match lst with
      | [] -> acc
      | [ x ] ->
          let acc' = [ x.ln; x.col; x.len; x.type_; 0 ] @ acc in
          deltaize acc' []
      | x :: y :: t ->
          let ln = x.ln - y.ln in
          let col = if ln > 0 then x.col else x.col - y.col in
          let len, type_ = x.len, x.type_ in
          let acc' = [ ln; col; len; type_; 0 ] @ acc in
          deltaize acc' (y :: t)
    in
    tokens |> sort |> (deltaize [])
end

let rec get_token_type : Parser.token -> TokenType.t option = function
  | TRUE | FALSE -> Some Enum
  | INT _ -> Some Number
  | ID _ -> Some Variable
  | STRING _ -> Some String_
  | VAL | FN | REC | LET | IN | END
  | IF | THEN | ELSE -> Some Keyword
  | READ | WRITE
  | AND | OR | PLUS | MINUS
  | COLEQ | MALLOC | BANG -> Some Function
  | EQ | RARROW -> Some Operator
  | COMMENT _ -> Some Comment
  | _ -> None

let highlight lexbuf =
  let acc = ref [] in

  let rec inner () =
    match Lexer.read lexbuf with
    | EOF -> ()
    | token ->
        let _, sln, scl = Location.get_pos_info lexbuf.lex_start_p in
        let _, eln, ecl = Location.get_pos_info lexbuf.lex_curr_p in
        let range = Range.from_tuples (sln - 1, scl) (eln - 1, ecl) in
        begin match (get_token_type token) with
        | Some type_ -> acc := !acc @ [ Token.from_range range ~type_ ]
        | None -> ()
        end;
        inner ()
  in

  inner (); !acc

let lexbuf_to_data lexbuf =
  lexbuf |> highlight |> Token.to_data

let compute params =
  let uri = get_uri params in
  let raw =
    match findr uri with
    | Some x -> x
    | None -> failwith "Lookup failure"
  in
  match Lexing.from_string raw with
  | lexbuf -> SemanticTokens.create ~data:(lexbuf_to_data lexbuf)
  | exception _ -> None

let run id params =
  let result_ = compute params in
  let result = SemanticTokens.yojson_of_t result_ in
  send (Resp { id; result })
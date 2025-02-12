(*
 * M Language Server
 * Position & Range
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Yojson.Safe.Util

module Position = struct
  type t =
    { ln : int
    ; col : int }

  let create ~ln ~col : t =
    { ln; col }

  let t_of_yojson yojson : t =
    let ln = yojson |> member "line" |> to_int in
    let col = yojson |> member "character" |> to_int in
    create ~ln ~col

  let yojson_of_t t =
    `Assoc
    [ ( "line", `Int t.ln )
    ; ( "character", `Int t.col ) ]
end

module Range = struct
  type t =
    { start : Position.t
    ; end_ : Position.t }
  
  let create ~start ~end_ : t =
    { start; end_ }

  let t_of_yojson yojson : t =
    let start = yojson |> member "start" |> Position.t_of_yojson in
    let end_ = yojson |> member "end" |> Position.t_of_yojson in
    create ~start ~end_

  let yojson_of_t t =
    `Assoc
    [ ( "start", t.start |> Position.yojson_of_t )
    ; ( "end", t.end_ |> Position.yojson_of_t ) ]

  let from_tuples stup etup = create
    ~start:(Position.create ~ln:(fst stup) ~col:(snd stup))
    ~end_:(Position.create ~ln:(fst etup) ~col:(snd etup))
end
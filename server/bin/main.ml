open Yojson.Safe
open Language_server

type jsonlog =
  | Req of Yojson.Safe.t
  | Rspn of Yojson.Safe.t
  | Noti of Yojson.Safe.t

let first = ref true
let states : (string, string) Hashtbl.t = Hashtbl.create 39
let log_list = ref []

let string_of_jsonlog jlog =
  match jlog with
  | Req j -> "[Request]\n" ^ pretty_to_string j
  | Rspn j -> "[Response]\n" ^ pretty_to_string j
  | Noti j -> "[Notification]\n" ^ pretty_to_string j

let output_log newlog =
  let strlog = string_of_jsonlog newlog in
  log_list := !log_list @ [ strlog ];
  let oc = open_out "/Users/young/Desktop/vsm/language_server/log.txt" in
  let rec output_log_inner lst =
    match lst with
    | [] -> ()
    | hd :: tl ->
        Printf.fprintf oc "%s\n\n" hd;
        output_log_inner tl
  in
  output_log_inner !log_list;
  close_out oc

let output_json obj =
  let msg = Yojson.Safe.pretty_to_string ~std:true obj in
  let size = String.length msg in
  Printf.printf "Content-Length: %d\r\n\r\n%s" size msg

let on_initialize id =
  let capabilities =
    `Assoc
      [
        ( "textDocumentSync",
          `Assoc [ ("openClose", `Bool true); ("change", `Int 1) ] );
        ("hoverProvider", `Bool true);
        ("codeLensProvider", `Assoc [ ("resolveProvider", `Bool true) ]);
      ]
  in
  let response =
    `Assoc
      [ ("id", `Int id); ("result", `Assoc [ ("capabilities", capabilities) ]) ]
  in
  output_json response;
  output_log (Rspn response)

let get_uri obj =
  obj |> Util.member "textDocument" |> Util.member "uri" |> Util.to_string

let on_did_open params =
  let path = get_uri params in
  let st =
    params |> Util.member "textDocument" |> Util.member "text" |> Util.to_string
  in
  Hashtbl.add states path st

let on_did_change params =
  let path = get_uri params in
  let changes = params |> Util.member "contentChanges" |> Util.to_list in
  match changes with
  | `Assoc [ ("text", `String st) ] :: _ -> Hashtbl.replace states path st
  | _ -> ()

let on_did_close params =
  let path = get_uri params in
  Hashtbl.remove states path

let get_state path =
  match Hashtbl.find_opt states path with
  | Some state -> state
  | None -> failwith "Lookup Failure..."

let word_at_position line cnum =
  let back_reg = Str.regexp {|[^a-zA-Z0-9'"][a-zA-Z0-9'"]|} in
  let start_idx =
    match Str.search_backward back_reg line cnum with
    | result -> result + 1
    | exception Not_found -> 0
  in

  let forward_reg = Str.regexp {|[a-zA-Z0-9'"][^a-zA-Z0-9'"]|} in
  let end_idx =
    match Str.search_forward forward_reg line cnum with
    | result -> result + 1
    | exception Not_found -> String.length line
  in

  String.sub line start_idx (end_idx - start_idx)

(* TODO Need to be sophisticated! There are so many corner cases... *)
let get_token state lnum cnum =
  let lines = String.split_on_char '\n' state in
  let line = List.nth lines lnum in
  let comment_reg = Str.regexp {|^\s*(\*\|\*)\s*$|} in

  if Str.string_match comment_reg line 0 then ""
  else
    let char_at_cnum = line.[cnum] in
    match char_at_cnum with
    | ' ' | ',' | '.' | ';' -> ""
    | '+' | '-' | '!' -> Printf.sprintf "%c" char_at_cnum
    | '=' ->
        if line.[cnum - 1] = ':' then ":="
        else if line.[cnum + 1] = '>' then "=>"
        else "="
    | ':' -> ":="
    | '>' -> "=>"
    | _ -> word_at_position line cnum

let on_hover id params =
  let path = get_uri params in
  let st = get_state path in

  let lnum =
    params |> Util.member "position" |> Util.member "line" |> Util.to_int
  in
  let cnum =
    params |> Util.member "position" |> Util.member "character" |> Util.to_int
  in
  let token = get_token st lnum cnum in

  let response =
    `Assoc
      [ ("id", `Int id); ("result", `Assoc [ ("contents", `String token) ]) ]
  in

  output_json response;
  output_log (Rspn response)

let publish_diag path lnum cnum =
  (* let st = get_state path in let token = get_token st lnum cnum in *)
  let lnum = 9 in
  let cnum = 4 in

  let range =
    `Assoc
      [
        ( "start",
          `Assoc [ ("line", `Int (lnum - 1)); ("character", `Int (cnum - 1)) ]
        );
        ( "end",
          `Assoc [ ("line", `Int (lnum - 1)); ("character", `Int (cnum - 1)) ]
        );
      ]
  in

  let diagnostic =
    `Assoc
      [
        ("range", range);
        ("severity", `Int 1);
        ("message", `String "Parsing Error");
      ]
  in

  let params =
    `Assoc [ ("uri", `String path); ("diagnostics", `List [ diagnostic ]) ]
  in

  let notification =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("method", `String "textDocument/publishDiagnostics");
        ("params", params);
      ]
  in

  output_json notification;
  output_log (Noti notification)

let on_code_lens id params =
  let path = get_uri params in
  let path_len = String.length path in
  let filename = String.sub path 8 (path_len - 8) in
  let st = get_state path in

  try
    let prog = M.get_program_from_string filename st in
    let info =
      match Simple_checker.check prog with
      | infered -> Simple_checker.string_of_ty infered
      | exception _ -> "type check failure"
    in

    let range =
      `Assoc
        [
          ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
          ("end", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
        ]
    in
    let command = `Assoc [ ("title", `String info) ] in
    let result = `Assoc [ ("range", range); ("command", command) ] in

    let response = `Assoc [ ("id", `Int id); ("result", `List [ result ]) ] in

    output_json response;
    output_log (Rspn response)
  with M.SyntaxError (lnum, cnum) -> publish_diag path (lnum - 1) cnum

let read_header () =
  let header = input_line stdin in
  let header_len = String.length header in
  if header = "\r" then failwith "wtf???"
  else if header_len >= 16 && String.sub header 0 16 = "Content-Length: " then
    let temp = String.trim (String.sub header 16 (header_len - 16)) in
    Some (int_of_string temp)
  else failwith "retry..."

let read_content clen =
  let _ = input_line stdin in
  really_input_string stdin clen

let parse_content content =
  let request = from_string content in
  let ido = request |> Util.member "id" |> Util.to_int_option in
  let method_name = request |> Util.member "method" |> Util.to_string in
  let params = request |> Util.member "params" in
  if ido = None then output_log (Noti request) else output_log (Req request);
  (ido, method_name, params)

let rec loop () =
  match read_header () with
  | Some content_len ->
      let content = read_content content_len in
      let ido, method_name, params = parse_content content in
      let id = Option.value ido ~default:(-1) in
      (match method_name with
      | "initialize" ->
          on_initialize id;
          first := false
      | "textDocument/didOpen" -> on_did_open params
      | "textDocument/didChange" -> on_did_change params
      | "textDocument/didClose" -> on_did_close params
      | "textDocument/hover" -> on_hover id params
      | "textDocument/codeLens" -> on_code_lens id params
      | "codeLens/resolve" -> failwith "who r u?"
      | "shutdown" -> failwith "wtf!!!"
      | _ -> ());
      flush_all ();
      loop ()
  | _ -> failwith "wtf?"

let () = loop ()

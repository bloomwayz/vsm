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
  | `Assoc [ ("text", `String st) ] :: _ ->
    Hashtbl.replace states path st
  | _ -> ()

let on_did_close params =
  let path = get_uri params in
  Hashtbl.remove states path

(* let get_token uri lnum cnum = let uri_len = String.length uri in let fpath =
   String.sub uri 7 uri_len in let fin = open_in fpath in for i = 1 to lnum do
   let _ = input_line fin done in let line = input_line fin in line *)

let on_hover id params =
  let path = get_uri params in
  let path_len = String.length path in
  let filename = String.sub path 8 (path_len - 8) in
  let sto = Hashtbl.find_opt states path in
  let st =
    match sto with
    | Some st' -> st'
    | None -> failwith "Lookup Failure..."
  in

  (*
  let lnum =
    params |> Util.member "position" |> Util.member "line" |> Util.to_int
  in
  let cnum =
    params |> Util.member "position" |> Util.member "character" |> Util.to_int
  in
  let token = get_token uri lnum cnum in
  *)

  let pgm = M.get_program_from_string filename st in
  let info =
    match Simple_checker.check pgm with
    | infered -> Simple_checker.string_of_ty infered
    | exception _ -> "type check failure"
  in

  let response =
    `Assoc
      [ ("id", `Int id); ("result", `Assoc [ ("contents", `String info) ]) ]
  in

  output_json response;
  output_log (Rspn response)

(* TODO What should be the command of CodeLens? *)
let on_code_lens id params =
  (* let path = get_uri params in *)
  let response =
    `Assoc
      [
        ("id", `Int id);
        ( "result",
          `Assoc
            [
              ( "range",
                `Assoc
                  [
                    ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
                    ("end", `Assoc [ ("line", `Int 0); ("character", `Int 6) ]);
                  ]
              );
              ( "command", `Assoc [ ("title", `String "typ"); ("command", `String "") ] )
            ]
        );
      ]
  in
  
  output_json response;
  output_log (Rspn response)


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
      | "shutdown" -> failwith "wtf!!!"
      | _ -> ());
      flush_all ();
      loop ()
  | _ -> failwith "wtf?"

let () = loop ()

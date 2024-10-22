open Yojson.Safe

type jsonlog =
  | Req of Yojson.Safe.t
  | Rspn of Yojson.Safe.t
  | Noti of Yojson.Safe.t

let first = ref true
let log_list = ref []

let string_of_jsonlog jlog =
  match jlog with
  | Req j -> "[Request]\n" ^ pretty_to_string j
  | Rspn j -> "[Response]\n" ^ pretty_to_string j
  | Noti j -> "[Notification]\n" ^ pretty_to_string j

let output_log newlog =
  let strlog = string_of_jsonlog newlog in
  log_list := !log_list @ [strlog];
  let oc = open_out "/Users/young/Desktop/vsm/language_server/log.txt" in
  let rec output_log_inner lst =
    match lst with
    | [] -> ()
    | hd::tl -> Printf.fprintf oc "%s\n\n" hd; output_log_inner tl
  in
  output_log_inner !log_list; close_out oc

let output_json obj =
  let msg  = Yojson.Safe.pretty_to_string ~std:true obj in
  let size = String.length msg in
  Printf.printf "Content-Length: %d\r\n\r\n%s" size msg

let on_initialize id =
  let capabilities = `Assoc [
    "hoverProvider", `Bool true
  ] in
  let response = `Assoc [
    "id", `Int id;
    "result", `Assoc [
      "capabilities", capabilities
    ]
  ] in
  output_json response;
  output_log (Rspn response)

(* let get_token uri lnum cnum =
  let uri_len = String.length uri in
  let fpath = String.sub uri 7 uri_len in
  let fin = open_in fpath in
  for i = 1 to lnum do
    let _ = input_line fin
  done in
  let line = input_line fin in
  line *)
  
let on_hover id params = 
  (* let uri = params |> Util.member "textDocument" |> Util.member "uri" |> Util.to_string in
  let lnum = params |> Util.member "position" |> Util.member "line" in
  let cnum = params |> Util.member "position" |> Util.member "character" in
  let token = get_token uri lnum cnum in *)
  let response = `Assoc [
    "id", `Int id;
    "result", `Assoc [
      "contents", `String "참 잘했어요!"
    ]
  ] in
  output_json response;
  output_log (Rspn response)

let read_header () =
  let header = input_line stdin in
  let header_len = String.length header in
  if (header = "\r") then failwith("wtf???")
  else if header_len >= 16 && String.sub header 0 16 = "Content-Length: " then (
    let temp = String.trim (String.sub header 16 (header_len - 16)) in
    Some (int_of_string temp)
  ) else
    failwith ("retry...")

let read_content clen =
  let _ = input_line stdin in
  really_input_string stdin clen

let id_of_jsont obj =
  match obj with
  | `Null -> -1
  | obj -> obj |> Util.to_int

let parse_content content =
  let request = from_string content in
  let pre_id = request |> Util.member "id" in
  let id = id_of_jsont pre_id in
  let method_name = request |> Util.member "method" |> Util.to_string in
  let params = request |> Util.member "params" in
  (if id = -1 then output_log (Noti request) else output_log (Req request));
  (id, method_name, params)

let rec loop () =
  match read_header () with
  | Some content_len ->
      let content = read_content content_len in
      let id, method_name, params = parse_content content in
      (match method_name with
      | "initialize" -> on_initialize id; first := false
      | "textDocument/hover" -> on_hover id params
      | _ -> ());
      flush_all (); loop ()
  | _ -> failwith ("wtf?")

let () = loop ()
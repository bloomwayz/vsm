open Yojson.Safe

exception Exit

let first = ref true

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
  output_json response

let on_hover id = 
  let response = `Assoc [
    "id", `Int id;
    "result", `Assoc [
      "contents", `String "Igeo Mannya?"
    ]
  ] in
  output_json response

let rec read_header () =
  let header = input_line stdin in
  let header_len = String.length header in
  if (header = "\r") then failwith("wtf???")
  else if header_len >= 16 && String.sub header 0 16 = "Content-Length: " then (
    let temp = String.trim (String.sub header 16 (header_len - 16)) in
    Some (int_of_string temp)
  ) else
    read_header ()

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
  (id, method_name, params)

let rec loop () =
  try
    match read_header () with
    | Some content_len ->
        let content = read_content content_len in
        let id, method_name, params = parse_content content in
        (match method_name with
        | "initialize" ->
            on_initialize id; first := false; flush_all (); loop ()
        | "initialized" -> flush_all (); loop ()
        | "textDocument/hover" -> on_hover id
        | _ -> failwith ("Unimplemented: " ^ method_name))
    | _ -> failwith ("wtf?")
  with End_of_file ->
    raise Exit

let () = loop ()
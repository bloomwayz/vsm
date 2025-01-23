open Yojson.Safe
open Server

type jsonlog =
  | Req of Yojson.Safe.t
  | Rspn of Yojson.Safe.t
  | Noti of Yojson.Safe.t

type state = Ast of Syntax.expr | Fail of string * int * int

let first = ref true
let states : (string, state) Hashtbl.t = Hashtbl.create 39
let log_list = ref []
let current_id = ref 0

let string_of_jsonlog jlog =
  match jlog with
  | Req j -> "[Request]\n" ^ pretty_to_string j
  | Rspn j -> "[Response]\n" ^ pretty_to_string j
  | Noti j -> "[Notification]\n" ^ pretty_to_string j

let output_log newlog =
  let strlog = string_of_jsonlog newlog in
  log_list := !log_list @ [ strlog ];
  let oc = open_out "/Users/young/Desktop/vsm/server/log.txt" in
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
  let stoken_list =
    `List
      [
        `String "namespace";
        `String "variable";
        `String "keyword";
        `String "string";
        `String "number";
        `String "operator";
      ]
  in
  let modifier_list = `List [] in

  let capabilities =
    `Assoc
      [
        ( "textDocumentSync",
          `Assoc [ ("openClose", `Bool true); ("change", `Int 1) ] );
        ( "semanticTokensProvider",
          `Assoc
            [
              ( "legend",
                `Assoc
                  [
                    ("tokenTypes", stoken_list);
                    ("tokenModifiers", modifier_list);
                  ] );
              ("full", `Bool true);
            ] );
        ("documentHighlightProvider", `Bool true);
        ("hoverProvider", `Bool true);
        ("codeLensProvider", `Assoc [ ("resolveProvider", `Bool true) ]);
        ( "diagnosticProvider",
          `Assoc
            [
              ("interFileDependencies", `Bool false);
              ("workspaceDiagnostics", `Bool true);
            ] );
        ("signatureHelpProvider", `Bool true)
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

let refresh_diagnostic () =
  let _ = incr current_id in
  let id = !current_id in
  let request =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int id);
        ("method", `String "workspace/diagnostic/refresh");
      ]
  in

  output_json request;
  output_log (Req request)

let on_did_open params =
  let path = get_uri params in
  let path_len = String.length path in
  let filename = String.sub path 8 (path_len - 8) in
  let text =
    params |> Util.member "textDocument" |> Util.member "text" |> Util.to_string
  in

  let st =
    try Ast (M.get_program_from_string filename text)
    with M.SyntaxError (msg, lnum, cnum) -> Fail (msg, lnum - 1, cnum)
  in

  Hashtbl.add states path st

let on_did_change params =
  let path = get_uri params in
  let path_len = String.length path in
  let filename = String.sub path 8 (path_len - 8) in
  let changes = params |> Util.member "contentChanges" |> Util.to_list in

  let text =
    match changes with
    | `Assoc [ ("text", `String change) ] :: _ -> change
    | _ -> ""
  in

  let st =
    try Ast (M.get_program_from_string filename text)
    with M.SyntaxError (msg, lnum, cnum) -> Fail (msg, lnum - 1, cnum)
  in

  Hashtbl.replace states path st;
  refresh_diagnostic ()

let on_did_close params =
  let path = get_uri params in
  Hashtbl.remove states path

let get_state path =
  match Hashtbl.find_opt states path with
  | Some state -> state
  | None -> failwith "Lookup Failure..."

let in_range (lnum : int) (cnum : int) (exp : Syntax.expr) =
  let lnum = lnum + 1 in
  let cnum = cnum + 1 in

  let loc = exp.loc in
  let _, sline, schar = Location.get_pos_info loc.loc_start in
  let _, eline, echar = Location.get_pos_info loc.loc_end in

  if sline <= lnum && lnum <= eline && schar <= cnum && cnum <= echar then true
  else false

let gen_range (loc : Location.t) =
  let _, l1, c1 = Location.get_pos_info loc.loc_start in
  let _, l2, c2 = Location.get_pos_info loc.loc_end in

  let l1, l2 = (l1 - 1, l2 - 1) in

  let start_pos = `Assoc [ ("line", `Int l1); ("character", `Int c1) ] in
  let end_pos = `Assoc [ ("line", `Int l2); ("character", `Int c2) ] in

  `Assoc [ ("start", start_pos); ("end", end_pos) ]

let rec subexp_at_pos exp lnum cnum =
  let subexps = ref [] in

  let rec traverse_ast (exp : Syntax.expr) =
    match exp.desc with
    | Const _ | Var _ | Read -> subexps := exp :: !subexps
    | Fn (_, e) | Write e | Fst e | Snd e | Malloc e | Deref e ->
        subexps := exp :: !subexps;
        traverse_ast e
    | App (e1, e2)
    | Bop (_, e1, e2)
    | Assign (e1, e2)
    | Seq (e1, e2)
    | Pair (e1, e2)
    | Let (Val (_, e1), e2)
    | Let (Rec (_, _, e1), e2) ->
        subexps := exp :: !subexps;
        traverse_ast e1;
        traverse_ast e2
    | If (e1, e2, e3) ->
        subexps := exp :: !subexps;
        traverse_ast e1;
        traverse_ast e2;
        traverse_ast e3
  in

  let _ = traverse_ast exp in
  let result = List.filter (in_range lnum cnum) !subexps in

  match result with [] -> None | x :: xs -> Some x

let on_highlight id params =
  let range =
    `Assoc
      [
        ("start", `Assoc [ ("line", `Int 2); ("character", `Int 1) ]);
        ("end", `Assoc [ ("line", `Int 2); ("character", `Int 10) ]);
      ]
  in
  let result = `Assoc [ ("range", range); ("kind", `Int 1) ] in

  let response = `Assoc [ ("id", `Int id); ("result", `List [ result ]) ] in

  output_json response;
  output_log (Rspn response)

let on_hover id params =
  let path = get_uri params in
  let st = get_state path in

  let lnum =
    params |> Util.member "position" |> Util.member "line" |> Util.to_int
  in
  let cnum =
    params |> Util.member "position" |> Util.member "character" |> Util.to_int
  in

  match st with
  | Ast ast -> (
      match subexp_at_pos ast lnum cnum with
      | Some texp ->
          let value =
            match st with
            | Ast ast -> (
                match Simple_checker.check_sub ast texp with
                | infered ->
                    let tstr = Simple_checker.string_of_ty infered in
                    `String ("```python\n" ^ tstr ^ "\n```")
                | exception _ -> `Null)
            | Fail _ -> `Null
          in

          let content =
            `Assoc [ ("kind", `String "markdown"); ("value", value) ]
          in

          let range = gen_range texp.loc in
          let response =
            `Assoc
              [
                ("id", `Int id);
                ("result", `Assoc [ ("contents", content); ("range", range) ]);
              ]
          in

          output_json response;
          output_log (Rspn response)
      | None ->
          let response = `Assoc [ ("id", `Int id); ("result", `Null) ] in
          output_json response;
          output_log (Rspn response))
  | _ ->
      let response = `Assoc [ ("id", `Int id); ("result", `Null) ] in
      output_json response;
      output_log (Rspn response)

let on_code_lens id params =
  let path = get_uri params in
  (* let path_len = String.length path in *)
  (* let filename = String.sub path 8 (path_len - 8) in *)
  let st = get_state path in

  let info =
    match st with
    | Ast ast -> (
        match Simple_checker.check_top ast with
        | infered -> Simple_checker.string_of_ty infered
        | exception _ -> "")
    | Fail _ -> ""
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

let push_diagnostic id params =
  let path = get_uri params in
  (* let path_len = String.length path in *)
  (* let filename = String.sub path 8 (path_len - 8) in *)
  let st = get_state path in

  match st with
  | Ast ast ->
      (* let result =
        match Simple_checker.check_top ast with
        | exception Simple_checker.Unification_error msg ->  *)
      let response = `Assoc [ ("id", `Int id); ("result", `Null) ] in
      output_json response;
      output_log (Rspn response)
  | Fail (msg, lnum, cnum) ->
      let range =
        `Assoc
          [
            ("start", `Assoc [ ("line", `Int lnum); ("character", `Int 0) ]);
            ("end", `Assoc [ ("line", `Int lnum); ("character", `Int 100) ]);
          ]
      in
      let item =
        `Assoc
          [ ("range", range); ("severity", `Int 1); ("message", `String msg) ]
      in
      let result =
        `Assoc [ ("kind", `String "full"); ("items", `List [ item ]) ]
      in
      let response = `Assoc [ ("id", `Int id); ("result", result) ] in
      output_json response;
      output_log (Rspn response)

  (* [
        `String "namespace";
        `String "variable";
        `String "keyword";
        `String "string";
        `String "number";
        `String "operator";
      ] *)
let produce_stokens (exp : Syntax.expr) =
  let stokens = ref [] in

  let append_token (loc : Location.t) (tokenlen : int) (tokentype : int)  =
    let _, sline, schar = Location.get_pos_info loc.loc_start in
    let _, eline, echar = Location.get_pos_info loc.loc_end in
    let tokenlen = echar - schar in
    let encoded = [ sline - 1; schar; tokenlen; tokentype; 0; ] in
    stokens := !stokens @ encoded
  in

  let rec find_tokens (exp : Syntax.expr) =
    match exp.desc with
    | Const (String s) ->
      let len = String.length s in
      append_token exp.loc len 3
    | Const (Int n) ->
      let str_of_int = Printf.sprintf "%d" n in
      let len = String.length str_of_int in
      append_token exp.loc len 4
    | Const (Bool true) -> append_token exp.loc 4 2
    | Const (Bool false) -> append_token exp.loc 5 2
    | Var x ->
      let len = String.length x in
      append_token exp.loc len 1
    | Fn (x, e) ->
      (* let len = String.length x in *)
      append_token exp.loc 2 2;
      find_tokens e
    | App (e1, e2) ->
      find_tokens e1; find_tokens e2
    | If (e1, e2, e3) ->
      append_token exp.loc 2 2;
      find_tokens e1;
      find_tokens e2;
      find_tokens e3
    | Bop (op, e1, e2) ->
      find_tokens e1;
      find_tokens e2
    | Read -> append_token exp.loc 4 2
    | Write e ->
      append_token exp.loc 4 2;
      find_tokens e
    | Pair (e1, e2) ->
      find_tokens e1;
      find_tokens e2
    | Fst e -> find_tokens e
    | Snd e -> find_tokens e
    | Seq (e1, e2) ->
      find_tokens e1;
      find_tokens e2
    | Let (Val (x, e1), e2) ->
      (* let len = String.length x in *)
      append_token exp.loc 7 2;
      find_tokens e1;
      find_tokens e2
    | Let (Rec (f, x, e1), e2) ->
      append_token exp.loc 7 2;
      find_tokens e1;
      find_tokens e2
    | Malloc e ->
      append_token exp.loc 6 2;
      find_tokens e
    | Assign (e1, e2) ->
      find_tokens e1;
      find_tokens e2
    | Deref e ->
      append_token exp.loc 1 5;
      find_tokens e
  in

  find_tokens exp;
  !stokens

let on_tokens id params =
  let path = get_uri params in
  let st = get_state path in

  match st with
  | Ast ast ->
    (* let stokens = produce_stokens ast in *)
    let stokens = [3; 5; 3; 0; 3; 0; 5; 4; 1; 0; 3; 2; 7; 2; 0] in
    let data = List.map (fun x -> `Int x) stokens in
    let response =
      `Assoc
        [ ("id", `Int id); ("result", `Assoc [ ("data", `List data) ]) ]
    in
    output_json response;
    output_log (Rspn response)
  | Fail _ ->
    let response =
      `Assoc
        [ ("id", `Int id); ("result", `Null) ]
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

let handle_req id method_name params =
  match method_name with
  | "initialize" ->
      on_initialize id;
      first := false
  | "textDocument/documentHighlight" -> ()
  | "textDocument/hover" -> on_hover id params
  | "textDocument/codeLens" -> on_code_lens id params
  | "textDocument/diagnostic" -> push_diagnostic id params
  | "textDocument/semanticTokens/full" -> on_tokens id params
  | "codeLens/resolve" -> failwith "who r u?"
  | "shutdown" -> failwith "wtf!!!"
  | _ -> ()

let handle_noti method_name params =
  match method_name with
  | "textDocument/didOpen" -> on_did_open params
  | "textDocument/didChange" -> on_did_change params
  | "textDocument/didClose" -> on_did_close params
  | _ -> ()

let handle_rspn response =
  (* let result = response |> Util.member "id" in let error = response |>
     Util.member "error" in *)
  ()

let parse_content content =
  let content = from_string content in
  let id_opt = content |> Util.member "id" |> Util.to_int_option in
  let method_opt = content |> Util.member "method" |> Util.to_string_option in

  match method_opt with
  | Some method_name -> (
      let params = content |> Util.member "params" in
      match id_opt with
      | Some id ->
          current_id := id;
          output_log (Req content);
          handle_req id method_name params
      | None ->
          output_log (Noti content);
          handle_noti method_name params)
  | None ->
      output_log (Rspn content);
      handle_rspn content

let rec loop () =
  match read_header () with
  | Some content_len ->
      let content = read_content content_len in
      parse_content content;
      flush_all ();
      loop ()
  | _ -> failwith "wtf?"

let () = loop ()

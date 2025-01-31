open Yojson.Safe
open Server
open Str

type jsonlog =
  | Req of Yojson.Safe.t
  | Rspn of Yojson.Safe.t
  | Noti of Yojson.Safe.t

type state = Ast of Syntax.expr | Fail of string * int * int

let first = ref true
let pgmtxt : (string, string) Hashtbl.t = Hashtbl.create 39
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
        (* 0 *)
        `String "parameter";
        (* 1 *)
        `String "variable";
        (* 2 *)
        `String "function";
        (* 3 *)
        `String "keyword";
        (* 4 *)
        `String "comment";
        (* 5 *)
        `String "string";
        (* 6 *)
        `String "number";
        (* 7 *)
        `String "operator";
        (* 8 *)
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
        ("documentHighlightProvider", `Bool false);
        ("hoverProvider", `Bool true);
        ("codeLensProvider", `Assoc [ ("resolveProvider", `Bool true) ]);
        ( "diagnosticProvider",
          `Assoc
            [
              ("interFileDependencies", `Bool false);
              ("workspaceDiagnostics", `Bool true);
            ] );
        ("signatureHelpProvider", `Bool true);
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

  Hashtbl.add pgmtxt path text;
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

  Hashtbl.replace pgmtxt path text;
  Hashtbl.replace states path st;
  refresh_diagnostic ()

let on_did_close params =
  let path = get_uri params in
  Hashtbl.remove pgmtxt path;
  Hashtbl.remove states path

let get_pgmtxt path =
  match Hashtbl.find_opt pgmtxt path with
  | Some txt -> txt
  | None -> failwith "Lookup Failure..."

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

let rec infer_fn (param : string) (exp : Syntax.expr) =
  match exp.desc with
  | Const (String _) -> "string"
  | Const (Int _) -> "int"
  | Const (Bool _) -> "bool"
  | Var x -> "'a"
  | Fn (x, e) -> "COMING SOON"
  | App (e, _) -> "COMING SOON"
  | If (e1, e2, e3) -> "COMING SOON"
  | _ -> "a"

let rec infer_sub_ (top : Syntax.expr) (sub : Syntax.expr) =
  match sub.desc with
  | Const (String _) -> "string"
  | Const (Int _) -> "int"
  | Const (Bool _) -> "bool"
  | Var x -> "'a"
  | App (_, _) -> (
      match Inference.check_sub top sub with
      | infered -> Inference.string_of_ty infered
      | exception _ -> "'a")
  | Fn (x, e) -> (
      try infer_fn x e
      with _ -> "'a")
  | If (e1, e2, e3) -> (
      match Inference.check_sub top sub with
      | infered -> Inference.string_of_ty infered
      | exception _ -> "")
  | Bop (op, e1, e2) -> (
      match op with
      | Add | Sub -> "int"
      | And | Or | Eq -> "bool")
  | Read -> "int"
  | Write e -> "unit"
  | Pair (e1, e2) -> "'a * 'a"
  | Fst e -> "'a"
  | Snd e -> "'a"
  | Seq (e1, e2) ->
      infer_sub_ top e2
  | Let (Val (x, e1), e2) -> "let"
  | Let (Rec (f, x, e1), e2) -> "letrec"
  | Malloc e ->
      let t =
        match Inference.check_sub top e with
        | infered -> Inference.string_of_ty infered
        | exception _ -> "'a"
      in
      Printf.sprintf "%s loc" t
  | Assign (e1, e2) -> "unit"
  | Deref e -> (
      match Inference.check_sub top sub with
      | infered -> Inference.string_of_ty infered
      | exception _ -> "'a")

let undisclose s =
  let count = ref 0 in
  let dict = [ "'a"; "'b"; "'c"; "'d"; "'e"; "'f"; "'g"; ] in
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

let slice (txt : string) (loc : Location.t) =
  let r = Str.regexp "\n" in
  let _, sln, scol = Location.get_pos_info loc.loc_start in
  let txtlen = String.length txt in
  
  let rec compute s i ln =
    match Str.search_forward r s i with
    | i ->
        if ln = sln then i
        else compute s i (ln + 1)
    | exception Not_found -> failwith "Not_found"
  in

  let start_ = compute txt 0 0 in
  let gaplen = String.length (Str.matched_string txt) in
  let start = start_ + gaplen in

  String.sub txt start (txtlen - start - gaplen)

let in_param_range (pgmtxt : string) (exp : Syntax.expr) (lnum : int) (cnum : int) =
  let loc = exp.loc in
  let _, _, scol = Location.get_pos_info loc.loc_start in

  let s = slice pgmtxt loc in
  let r = Str.regexp {|fn |} in
  let _ = Str.search_forward r s 0 in
  let start = scol + String.length (Str.matched_string s) in

  let id =
      match exp.desc with
      | Fn (x, _) -> x
      | _ -> failwith "no way!"
  in

  let end_ = start + String.length id in

  if start <= cnum && cnum <= end_ then Some (lnum, start, end_)
  else None

let infer_sub (pgmtxt : string) (ast : Syntax.expr) (exp : Syntax.expr) (lnum : int) (cnum : int) =
  let range = gen_range exp.loc in
    match exp.desc with
    | Const (String _) -> Some ("string", range)
    | Const (Int _) -> Some ("int", range)
    | Const (Bool _) -> Some ("bool", range)
    | Fn (id, expr) ->
      let range_opt = in_param_range pgmtxt exp lnum cnum in (
        match range_opt with
        | Some (lnum, start, end_) ->
          let fty = Inference.check_sub ast exp in
          let fty = Inference.string_of_ty fty in
          let r = Str.regexp {| -> |} in
          let i = Str.search_forward r fty 0 in

          let start_pos = `Assoc [ ("line", `Int lnum); ("character", `Int start) ] in
          let end_pos = `Assoc [ ("line", `Int lnum); ("character", `Int end_) ] in
          let range = `Assoc [ ("start", start_pos); ("end", end_pos) ] in

          Some (String.sub fty 0 i, range)
      | None -> (
        match Inference.check_sub ast exp with
        | x -> Some (Inference.string_of_ty x, range)
        | exception _ -> None ) )
    | _ -> (
        match Inference.check_sub ast exp with
        | x -> Some (Inference.string_of_ty x, range)
        | exception _ -> None )

let on_hover id params =
  let path = get_uri params in
  let st = get_state path in
  let pgmtxt = get_pgmtxt path in

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
          let value, range =
            match (infer_sub pgmtxt ast texp lnum cnum) with
            | Some (ty, range) -> `String ("```ocaml\n" ^ (undisclose ty) ^ "\n```"), range
            | None -> `Null, `Null
          in

          let content =
            `Assoc [ ("kind", `String "markdown"); ("value", value) ]
          in

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
  let st = get_state path in

  let info =
    match st with
    | Ast ast -> (
        match Inference.check_top ast with
        | infered -> undisclose (Inference.string_of_ty infered)
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
      let result =
        match Inference.check_top ast with
        | _ -> `Null
        | (exception Inference.Unification_error_with_loc (msg, loc))
        | (exception Inference.Unbound_variable (msg, loc)) ->
            let msg = undisclose msg in
            let _, sline, schar = Location.get_pos_info loc.loc_start in
            let _, eline, echar = Location.get_pos_info loc.loc_end in
            let sline, eline = (sline - 1, eline - 1) in
            let range =
              `Assoc
                [
                  ( "start",
                    `Assoc [ ("line", `Int sline); ("character", `Int schar) ]
                  );
                  ( "end",
                    `Assoc [ ("line", `Int eline); ("character", `Int echar) ]
                  );
                ]
            in
            let item =
              `Assoc
                [
                  ("range", range);
                  ("severity", `Int 1);
                  ("message", `String msg);
                ]
            in
            `Assoc [ ("kind", `String "full"); ("items", `List [ item ]) ]
      in
      let response = `Assoc [ ("id", `Int id); ("result", result) ] in
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

let rec match_all r s i =
  match Str.search_forward r s i with
  | i ->
      let sub = Str.matched_string s in
      (i, sub) :: match_all r s (i + 1)
  | exception Not_found -> []

let get_ln_col s i =
  let pattern = Str.regexp "\n" in
  let nline_lst = match_all pattern s 0 in
  let rec inner lst ln col prev =
    match lst with
    | [] -> (ln, col)
    | (x, _) :: t ->
        if x > col then (ln, col - prev) else inner t (ln + 1) col x
  in
  inner nline_lst 1 i 0

let produce_stokens (exp : Syntax.expr) (txt : string) =
  let stokens = ref [] in

  let append_token sln scol tlen ttyp =
    let encoded = [ sln - 1; scol; tlen; ttyp; 0 ] in
    stokens := encoded :: !stokens
  in

  let rec find_tokens (exp : Syntax.expr) =
    let loc = exp.loc in
    let _, sln, scol = Location.get_pos_info loc.loc_start in
    let _, eln, ecol = Location.get_pos_info loc.loc_end in

    match exp.desc with
    | Const (String s) ->
        let len = String.length s in
        append_token sln scol len 6
    | Const (Int n) ->
        let str_of_int = Printf.sprintf "%d" n in
        let len = String.length str_of_int in
        append_token sln scol len 7
    | Const (Bool true) -> append_token sln scol 4 0
    | Const (Bool false) -> append_token sln scol 5 0
    | Var x ->
        let len = String.length x in
        append_token sln scol len 2
    | Fn (x, e) ->
        let len = String.length x in
        append_token sln scol 2 4;
        append_token sln (scol + 3) len 2;
        append_token sln (scol + len + 4) 2 8;
        find_tokens e
    | App (e1, e2) ->
        find_tokens e1;
        find_tokens e2
    | If (e1, e2, e3) ->
        let _, e1_eln, e1_ecol = Location.get_pos_info e1.loc.loc_end in
        let _, e2_eln, e2_ecol = Location.get_pos_info e2.loc.loc_end in
        append_token sln scol 2 4;
        append_token e1_eln (e1_ecol + 1) 4 4;
        append_token e2_eln (e2_ecol + 1) 4 4;
        find_tokens e1;
        find_tokens e2;
        find_tokens e3
    | Bop (op, e1, e2) ->
        let _, e1_eln, e1_ecol = Location.get_pos_info e1.loc.loc_end in
        append_token e1_eln (e1_ecol + 1) 2 8;
        find_tokens e1;
        find_tokens e2
    | Read -> append_token sln scol 4 3
    | Write e ->
        append_token sln scol 4 3;
        find_tokens e
    | Pair (e1, e2) ->
        find_tokens e1;
        find_tokens e2
    | Fst e -> find_tokens e
    | Snd e -> find_tokens e
    | Seq (e1, e2) ->
        let _, e1_eln, e1_ecol = Location.get_pos_info e1.loc.loc_end in
        append_token e1_eln (e1_ecol + 1) 1 4;
        find_tokens e1;
        find_tokens e2
    | Let (Val (x, e1), e2) ->
        let len = String.length x in
        let _, e1_eln, e1_ecol = Location.get_pos_info e1.loc.loc_end in
        append_token sln scol 7 4;
        append_token sln (scol + 8) len 2;
        append_token e1_eln (e1_ecol + 1) 2 4;
        append_token eln (ecol - 3) 3 4;
        find_tokens e1;
        find_tokens e2
    | Let (Rec (f, x, e1), e2) ->
        append_token sln scol 7 4;
        find_tokens e1;
        find_tokens e2
    | Malloc e ->
        append_token sln scol 6 3;
        find_tokens e
    | Assign (e1, e2) ->
        let _, e1_eln, e1_ecol = Location.get_pos_info e1.loc.loc_end in
        append_token e1_eln (e1_ecol + 2) 2 8;
        find_tokens e1;
        find_tokens e2
    | Deref e ->
        append_token sln scol 1 8;
        find_tokens e
  in

  let find_comment txt =
    let pattern = Str.regexp {|(\*.*\*)|} in
    let cmt_list = match_all pattern txt 0 in
    let rec inner lst =
      match lst with
      | [] -> ()
      | (i, s) :: t ->
          let sln, scol = get_ln_col txt i in
          let len = String.length s in
          append_token sln scol len 5;
          inner t
    in
    inner cmt_list
  in

  let compare_tokens x y =
    if List.nth x 0 != List.nth y 0 then List.nth y 0 - List.nth x 0
    else List.nth y 1 - List.nth x 1
  in

  find_comment txt;
  find_tokens exp;
  List.sort compare_tokens !stokens

let rec delta_of_stokens stokens acc =
  match stokens with
  | [] -> acc
  | [ x ] -> delta_of_stokens [] (x @ acc)
  | x :: y :: t ->
      let rln = List.nth x 0 - List.nth y 0 in
      let rcol =
        if rln > 0 then List.nth x 1 else List.nth x 1 - List.nth y 1
      in
      let tlen, ttype = (List.nth x 2, List.nth x 3) in
      let acc' = [ rln; rcol; tlen; ttype; 0 ] @ acc in
      delta_of_stokens (y :: t) acc'

let on_tokens id params =
  let path = get_uri params in
  let txt = get_pgmtxt path in
  let st = get_state path in

  match st with
  | Ast ast ->
      let stokens = produce_stokens ast txt in
      let data = delta_of_stokens stokens [] in
      let data' = List.map (fun x -> `Int x) data in
      let response =
        `Assoc [ ("id", `Int id); ("result", `Assoc [ ("data", `List data') ]) ]
      in
      output_json response;
      output_log (Rspn response)
  | Fail _ ->
      let response = `Assoc [ ("id", `Int id); ("result", `Null) ] in
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

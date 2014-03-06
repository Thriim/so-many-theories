

open Ast
open Parser
open Lexing
open Lexer
open Sat
open Smt


open Format

let spec = []
let usage = "prog.byte <file>.cnfuf"

module SimpleSat = Sat.Make(Sat.Boolean)

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".cnfuf" || Filename.check_suffix s ".cnf") then
      raise (Arg.Bad "no .cnfuf or .cnf extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let _ =
  let d = open_in file in
  let lb = Lexing.from_channel d in
  try
    let system = if Filename.check_suffix file "cnfuf" then
        let ast = Parser.file Lexer.token lb in
        Sat.translate ast
      else
        let ast = Parser.sat Lexer.token lb in
        dummy_map ast, ast
    in
    printf "%s@." (Ast.string_of_system system);
    try let m = SimpleSat.solver system in
      printf "Sat, with the model: %s@." @@ string_of_model m
    with Unsat ->  printf "Unsat@."

  with
  | Lexical_error s ->
    report_loc (lexeme_start_p lb, lexeme_end_p lb);
    eprintf "Lexical error: %s\n@." s;
    exit 1
  | Parsing.Parse_error ->
    report_loc (lexeme_start_p lb, lexeme_end_p lb);
    eprintf "%s@." lb.lex_buffer;
    eprintf "Syntax error\n@.";
    exit 1

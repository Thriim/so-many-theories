


open Ast
open Parser
open Lexing
open Lexer


open Format

let file = "tests/dummy_test.cnfuf"

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let _ =

  let d = open_in file in
  let lb = Lexing.from_channel d in
  try 
    ignore(Parser.file Lexer.token lb)
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

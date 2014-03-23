

open Ast
open Parser
open Lexing
open Lexer
open Sat
open Equality

open Format

let algorithm = ref CDCL

let spec = ["-dpll", Arg.Unit (fun () -> algorithm := DPLL),
            "The solver uses the dpll algorithm instead of cdcl";

            "-period", Arg.Set_int Sat.vsids_period,
            "Sets the period of of vsids division";

            "-div", Arg.Set_int Sat.vsids_divider,
            "Sets the value of the vsids divider";

            "-no-vsids", Arg.Unit (fun () -> Sat.use_vsids := false),
            "Deactivates the vsids algorithm for CDCL"
]


let usage = "prog.byte <file>.cnfuf"

module SimpleSat = Sat.Make(Sat.Boolean)
module EqualitySat = Sat.Make(Equality.Solver)

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

(** Solves a boolean CNF *)
let solve_boolean lb =
  let ast = Parser.sat Lexer.token lb in
  let system = Boolean.translate ast in
  try
    let time = Sys.time () in
    let m = SimpleSat.solver !algorithm system in
    let took = Sys.time () -. time in
    Format.printf "SAT\n  with the model: %s@." @@ string_of_clause
      @@ model_to_clause m;
    printf "took: %f@." took
  with Unsat -> printf "UNSAT@."

(** Solves an equality cnf *)
let solve_equality lb =
  let ast = Parser.file Lexer.token lb in
  let system = Equality_ast.translate ast in
  printf "%s@." (Equality_ast.string_of_op_system system);
  try
    let time = Sys.time () in
    let m = EqualitySat.solver !algorithm system in
    let took = Sys.time () -. time in
    printf "SAT \n  with the model: %s@." @@ string_of_clause
    @@ model_to_clause m;
    printf "took: %f@." took
  with Unsat ->  printf "UNSAT@."

let _ =
  let d = open_in file in
  let lb = Lexing.from_channel d in
  try
    if Filename.check_suffix file "cnfuf" then solve_equality lb
    else solve_boolean lb
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

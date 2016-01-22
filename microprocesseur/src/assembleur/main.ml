open Ast;;
open Format;;
open Lexer;;
open Lexing;;

let localisation pos input_file =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" input_file l (c-1) c
;;

let main () =
  Printf.printf "Début de Compilation";
  print_endline "";
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  
  let f = open_in input_file in
  let buf = Lexing.from_channel f in
  
  try
    let p = Parser.file Lexer.next_tokens buf in
  
    Compile.assemble_file p output_file
  
  with
    |Lexical_error(str) ->
      (localisation (Lexing.lexeme_start_p buf) input_file;
      eprintf "Erreur Lexical@.";
      Printf.printf "%s\n" str;
      exit 1)
    |Parser.Error ->
      (localisation (Lexing.lexeme_start_p buf) input_file;
      eprintf "Erreur syntaxique@.";
      exit 1)
;;

main ();
























































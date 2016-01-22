{
  open Ast
  open Parser
  open Lexing
  
  exception Lexical_error of string;;
  
  let instr = Hashtbl.create 97
  let () = List.iter (fun (x, y) -> Hashtbl.add instr x y)
             ["MOVE", LMOVE ; "MOVEi", LMOVEi ; "ADD", LADD ; "ADDi", LADDi ;
              "SUB", LSUB ; "SUBi", LSUBi ; "AND", LAND ; "NOT", LNOT ; "LSE", LLSE ; 
              "RSE", LRSE ; "EQZ", LEQZ ; "EQZi", LEQZi ; "MTZ", LMTZ ; "MTZi", LMTZi]
  
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum}
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

let integer = ('0' | ['1'-'9'] digit*)
let ident = letter (letter | digit | '_')*

rule next_tokens = parse
  |"\n" {newline lexbuf; next_tokens lexbuf}
  |[' ' '\t']+ {next_tokens lexbuf} (* Gestion des blancs *)
  |"#MACRO" {DEBMACRO}
  |"#ENDMACRO" {FINMACRO}
  |integer as str_n
    {try
       let nb = int_of_string str_n in
       if nb < (1 lsl 31) then
         INTEGER(nb)
       else
         raise (Lexical_error "Constante entière trop grande !")
     with
       |Failure s -> raise (Lexical_error "Constante entière trop grande !")}
  |ident as str_id {try Hashtbl.find instr str_id with Not_found -> IDENT(str_id)}
  |"," {COMMA}
  |";" {SEMI_COLON}
  |"(" {OPEN_BRACKET}
  |")" {CLOSE_BRACKET}
  |"{" {OPEN_BRACE}
  |"}" {CLOSE_BRACE}
  |"*" {STAR}
  |"-" {MOINS}
  |"//" {comment_line lexbuf}
  |eof {EOF}
  |_ {raise (Lexical_error "")}

and comment_line = parse
  |"\n" {next_tokens lexbuf}
  |_ {comment_line lexbuf}
  |eof {EOF}

%{
  open Ast
  
  exception Mon_Erreur;;
  
  let kw = Hashtbl.create 97
  let () = List.iter (fun (x, y) -> Hashtbl.add kw x y)
             ["R1", R1 ; "R2", R2 ; "R3", R3 ;
              "R4", R4 ; "R5", R5 ; "R6", R6 ;
              "R7", R7 ; "R8", R8 ; "R9", R9 ;
              "R10", R10 ; "R11", R11 ; "R12", R12 ;
              "R13", R13 ; "R14", R14 ; "R15", R15 ;
              "R16", R16 ; "R17", R17 ; "R18", R18 ;
              "R19", R19 ; "R20", R20 ; "R21", R21 ;
              "R22", R22 ; "R23", R23 ; "R24", R24 ;
              "R25", R25 ; "R26", R26 ; "R27", R27 ;
              "R28", R28 ; "R29", R29 ; "R30", R30 ;
              "R31", R31 ; "R32", R32]
  
%}

%token DEBMACRO FINMACRO
%token LMOVE LMOVEi LADD LADDi LSUB LSUBi LAND LOR LXOR LNOT LLSE LRSE LEQZ LEQZi LLTZ LLTZi LMTZ LMTZi
%token <int> INTEGER
%token <string> IDENT
%token COMMA SEMI_COLON OPEN_BRACKET CLOSE_BRACKET OPEN_BRACE CLOSE_BRACE
%token EOF

%start file

%type <Ast.sc_file> file

%%

file:
  |DEBMACRO; l_m = macro*; FINMACRO; mon_main = instructrion*; EOF
    {{l_macro = l_m;
      main = mon_main;
    }}
;

macro:
  |id = IDENT; OPEN_BRACKET; l_arg = separated_list(COMMA, IDENT); CLOSE_BRACKET;
   OPEN_BRACE; l_body = separated_list(SEMI_COLON, instruction); CLOSE_BRACE;
    {{nom = id;
      arg = l_arg;
      body = l_body;
    }}
;

instructrion:
  |LMOVE; a1 = IDENT; COMMA; a2 = IDENT;
    {let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     MOVE(reg1, reg2)}
  |LMOVEi; a1 = INTEGER; COMMA; a2 = IDENT;
    {let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     MOVEi(a1, reg2)}
  |LADD; a1 = IDENT; COMMA; a2 = IDENT;
    {let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     ADD(reg1, reg2)}
  |LADDi; a1 = INTEGER; COMMA; a2 = IDENT;
    {let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     ADDi(a1, reg2)}
  |LSUB; a1 = IDENT; COMMA; a2 = IDENT;
    {let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     SUB(reg1, reg2)}
  |LSUBi; a1 = INTEGER; COMMA; a2 = IDENT;
    {let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     SUBi(a1, reg2)}
  |LAND; a1 = IDENT; COMMA; a2 = IDENT;
    {let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     AND(reg1, reg2)}
  |LOR; a1 = IDENT; COMMA; a2 = IDENT;
    {let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     OR(reg1, reg2)}
  |LXOR; a1 = IDENT; COMMA; a2 = IDENT;
    {let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     XOR(reg1, reg2)}
  |LNOT; a = IDENT;
    {let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     NOT(reg)}
  |LLSE; a = IDENT;
    {let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     LSE(reg)}
  |LRSE; a = IDENT;
    {let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     RSE(reg)}
  |LEQZ; a = IDENT;
    {let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     EQZ(reg)}
  |LEQZi; a = INTEGER;
    {EQZi(a)}
  |LLTZ; a = IDENT;
    {let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     LTZ(reg)}
  |LLTZi; a = INTEGER;
    {LTZi(a)}
  |LMTZ; a = IDENT;
    {let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     MTZ(reg)}
  |LMTZi; a = INTEGER;
    {MTZi(a)}
;































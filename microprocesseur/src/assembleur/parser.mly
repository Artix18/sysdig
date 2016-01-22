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
%token LMOVE LMOVEi LADD LADDi LSUB LSUBi LAND LNOT LLSE LRSE LEQZ LEQZi LMTZ LMTZi
%token <int> INTEGER
%token <string> IDENT
%token COMMA SEMI_COLON OPEN_BRACKET CLOSE_BRACKET OPEN_BRACE CLOSE_BRACE STAR MOINS
%token EOF

%start file

%type <Ast.file> file

%%

file:
  |DEBMACRO; l_m = macro*; FINMACRO; mon_main = instruction*; EOF
    {{l_macro = l_m;
      main = mon_main;
    }}
;

macro:
  |id = IDENT; OPEN_BRACKET; l_arg = separated_list(COMMA, IDENT); CLOSE_BRACKET;
   OPEN_BRACE; l_body = instruction*; CLOSE_BRACE;
    {{nom = id;
      arg = l_arg;
      body = l_body;
    }}
;

arg:
  |a = IDENT;
    {false, a}
  |STAR; a = IDENT;
    {true, a}
;

nombre:
  |MOINS; x = INTEGER;
    {x * -1}
  |x = INTEGER;
    {x}

commande:
  |LMOVE; x1 = arg; COMMA; x2 = arg;
    {let (t1, a1) = x1 in
     let (t2, a2) = x2 in
     let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     MOVE((t1, reg1), (t2, reg2))}
  |LMOVEi; x1 = nombre; COMMA; x2 = arg;
    {let (t2, a2) = x2 in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     MOVEi(x1, (t2, reg2))}
  |LADD; x1 = arg; COMMA; x2 = arg;
    {let (t1, a1) = x1 in
     let (t2, a2) = x2 in
     let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     ADD((t1, reg1), (t2, reg2))}
  |LADDi; x1 = nombre; COMMA; x2 = arg;
    {let (t2, a2) = x2 in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     ADDi(x1, (t2, reg2))}
  |LSUB; x1 = arg; COMMA; x2 = arg;
    {let (t1, a1) = x1 in
     let (t2, a2) = x2 in
     let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     SUB((t1, reg1), (t2, reg2))}
  |LSUBi; x1 = nombre; COMMA; x2 = arg;
    {let (t2, a2) = x2 in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     SUBi(x1, (t2, reg2))}
  |LAND; x1 = arg; COMMA; x2 = arg;
    {let (t1, a1) = x1 in
     let (t2, a2) = x2 in
     let reg1 =
       try
       Hashtbl.find kw a1
       with Not_found -> Var(a1)
     in
     let reg2 =
       try
       Hashtbl.find kw a2
       with Not_found -> Var(a2)
     in
     AND((t1, reg1), (t2, reg2))}
  |LNOT; x = arg;
    {let (t, a) = x in
     let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     NOT((t, reg))}
  |LLSE; x = arg;
    {let (t, a) = x in
     let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     LSE((t, reg))}
  |LRSE; x = arg;
    {let (t, a) = x in
     let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     RSE((t, reg))}
  |LEQZ; x = arg;
    {let (t, a) = x in
     let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     EQZ((t, reg))}
  |LEQZi; a = nombre;
    {EQZi(a)}
  |LMTZ; x = arg;
    {let (t, a) = x in
     let reg =
       try
       Hashtbl.find kw a
       with Not_found -> Var(a)
     in
     MTZ((t, reg))}
  |LMTZi; a = nombre;
    {MTZi(a)}
  |id = IDENT; a = separated_list(COMMA, arg);
    {MACRO(id, List.map (fun x -> (fst x, try Hashtbl.find kw (snd x) with Not_found -> Var(snd x))) a)}
;

instruction:
  |c = commande; SEMI_COLON;
    {c}











































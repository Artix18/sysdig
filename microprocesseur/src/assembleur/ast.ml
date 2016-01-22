type reg =
  |R1 |R2 |R3 |R4
  |R5 |R6 |R7 |R8
  |R9 |R10 |R11 |R12
  |R13 |R14 |R15 |R16
  |R17 |R18 |R19 |R20
  |R21 |R22 |R23 |R24
  |R25 |R26 |R27 |R28
  |R29 |R30 |R31 |R32
  |Var of string

and arg = bool * reg (* false pour direct, true pour indirect *)

and instruction =
  |MOVE of arg * arg
  |MOVEi of int * arg
  |ADD of arg * arg
  |ADDi of int * arg
  |SUB of arg * arg
  |SUBi of int * arg
  |AND of arg * arg
  |NOT of arg
  |LSE of arg
  |RSE of arg
  |EQZ of arg
  |EQZi of int
  |MTZ of arg
  |MTZi of int
  |MACRO of string * arg list

and macro = {
  nom : string;
  arg : string list;
  body : instruction list;
}

and file = {
  l_macro : macro list;
  main : instruction list;
}

(*        Inst    immediate   reg1(6bits)/nb(16bits)  reg2(6bits)
ADD(i) :  0000    0 (1) 
SOUS(i) : 0001
AND :     0100
LSE :     0010     0           000001 (*R1*)          000001 (*R1*) 
EQZ :     0110
MOVE(i) : 1000
NOT :     1100
RSE :     1010
MTZ :     1110
*)













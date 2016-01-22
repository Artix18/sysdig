open Ast;;

exception ERREUR of string;;

let buf = Buffer.create 17;;
module Smap = Map.Make(String);;

let rec int_to_bin nb pos =
  if nb < 0 then
    int_to_bin ((int_of_float (2.0**(float_of_int pos))) + nb) pos
  else
    if pos = 0 then
      ""
    else
      if nb mod 2 = 0 then
        "0"^(int_to_bin (nb/2) (pos-1))
      else
        "1"^(int_to_bin (nb/2) (pos-1))
;;

let code_reg reg =
  let code_reg =
    match snd reg with
    |R1 -> int_to_bin 0 5
    |R2 -> int_to_bin 1 5
    |R3 -> int_to_bin 2 5
    |R4 -> int_to_bin 3 5
    |R5 -> int_to_bin 4 5
    |R6 -> int_to_bin 5 5
    |R7 -> int_to_bin 6 5
    |R8 -> int_to_bin 7 5
    |R9 -> int_to_bin 8 5
    |R10 -> int_to_bin 9 5
    |R11 -> int_to_bin 10 5
    |R12 -> int_to_bin 11 5
    |R13 -> int_to_bin 12 5
    |R14 -> int_to_bin 13 5
    |R15 -> int_to_bin 14 5
    |R16 -> int_to_bin 15 5
    |R17 -> int_to_bin 16 5
    |R18 -> int_to_bin 17 5
    |R19 -> int_to_bin 18 5
    |R20 -> int_to_bin 19 5
    |R21 -> int_to_bin 20 5
    |R22 -> int_to_bin 21 5
    |R23 -> int_to_bin 22 5
    |R24 -> int_to_bin 23 5
    |R25 -> int_to_bin 24 5
    |R26 -> int_to_bin 25 5
    |R27 -> int_to_bin 26 5
    |R28 -> int_to_bin 27 5
    |R29 -> int_to_bin 28 5
    |R30 -> int_to_bin 29 5
    |R31 -> int_to_bin 30 5
    |R32 -> int_to_bin 31 5
    |Var(str) -> raise (ERREUR("Presence d'une variable dans l'assemblage !"))
  in
  let code_direct = if fst reg then "1" else "0" in
  code_reg ^ code_direct
;;

let assemble_inst inst =
  Buffer.add_string buf (
		match inst with
		|ADD(a1, a2) -> "00000"^(code_reg a1)^(code_reg a2)^"0000000000"
		|ADDi(n1, a2) -> "00001"^(int_to_bin n1 16)^(code_reg a2)
		|SUB(a1, a2) -> "00010"^(code_reg a1)^(code_reg a2)^"0000000000"
		|SUBi(n1, a2) -> "00011"^(int_to_bin n1 16)^(code_reg a2)
		|AND(a1, a2) -> "01000"^(code_reg a1)^(code_reg a2)^"0000000000"
		|LSE(a) -> "00100"^(code_reg a)^(code_reg a)^"0000000000"
		|EQZ(a) -> "01100"^(code_reg a)^(code_reg a)^"0000000000"
		|EQZi(n) -> "01101"^(int_to_bin n 16)^(code_reg (false, R32))
		|MOVE(a1, a2) -> "10000"^(code_reg a1)^(code_reg a2)^"0000000000"
		|MOVEi(n1, a2) -> "10001"^(int_to_bin n1 16)^(code_reg a2)
		|NOT(a) -> "11000"^(code_reg a)^(code_reg a)^"0000000000"
		|RSE(a) -> "10100"^(code_reg a)^(code_reg a)^"0000000000"
		|MTZ(a) -> "11100"^(code_reg a)^(code_reg a)^"0000000000"
		|MTZi(n) -> "11101"^(int_to_bin n 16)^(code_reg (false, R32))
		|_ -> raise (ERREUR("Presence d'une macro dans l'assemblage !"))
	)
(*	Buffer.add_string buf "\n" *)
;;

let substitute a v r =
  if snd a = Var(v) then
    (fst a, snd r)
  else
    a
;;

let substitute_inst inst v r =
  match inst with
  |MOVE(a1, a2) -> MOVE(substitute a1 v r, substitute a2 v r)
  |ADD(a1, a2) -> ADD(substitute a1 v r, substitute a2 v r)
  |SUB(a1, a2) -> SUB(substitute a1 v r, substitute a2 v r)
  |AND(a1, a2) -> AND(substitute a1 v r, substitute a2 v r)
  |MOVEi(n1, a2) -> MOVEi(n1, substitute a2 v r)
  |ADDi(n1, a2) -> ADDi(n1, substitute a2 v r)
  |SUBi(n1, a2) -> SUBi(n1, substitute a2 v r)
  |NOT(a) -> NOT(substitute a v r)
  |LSE(a) -> LSE(substitute a v r)
  |RSE(a) -> RSE(substitute a v r)
  |EQZ(a) -> EQZ(substitute a v r)
  |EQZi(n) -> EQZi(n)
  |MTZ(a) -> MTZ(substitute a v r)
  |MTZi(n) -> MTZi(n)
  |MACRO(id, l_a) -> MACRO(id, List.map (fun x -> substitute x v r) l_a)
;;

let rec preprocessor f l_instr =
  match l_instr with
  |[] -> []
  |head::tail -> begin
    match head with
    |MACRO(id, l_a) ->
      Printf.printf "ESSAIE find %s\n" id;
      let macro_def = List.find (fun x -> x.nom = id) f.l_macro in
      Printf.printf "ESSAIE réussi !!\n";
      let macro_body = List.map (fun x -> List.fold_left2 (fun inst v r -> substitute_inst inst v r) x macro_def.arg l_a) macro_def.body in
      preprocessor f (List.append macro_body tail)
    |_ -> head::(preprocessor f tail)
  end
;;

let print_on_file file =
  let f = open_out file in
  Buffer.output_buffer f buf
;;

let assemble_file f output =
  let prog = preprocessor f f.main in
  Printf.printf "Préprocesseur fait %d" (List.length prog);
  print_endline "";
  List.iter assemble_inst prog;
  print_on_file output
;;

(*
let rec assemble instr_l context = match instr_l with
    | [] -> ()
    | p::q ->(
    (
        match p with
          |MOVE(arg1, arg2) -> Buffer.add_string buf ("1000"^"0"^(reg_to_bin (snd arg1) context)^(bool_to_bin (fst arg1))^(reg_to_bin (snd arg2) context)^(bool_to_bin (fst arg2))^"0000000000")
          |MOVEi(nb,arg)    -> Buffer.add_string buf ("1000"^"1"^(int_to_bin nb 16)^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg)))
          |ADD(arg1,arg2)   -> Buffer.add_string buf ("0000"^"0"^(reg_to_bin (snd arg1) context)^(bool_to_bin (fst arg1))^(reg_to_bin (snd arg2) context)^(bool_to_bin (fst arg2))^"0000000000")
          |ADDi(nb,arg)     -> Buffer.add_string buf ("0000"^"1"^(int_to_bin nb 16)^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg)))
          |SUB(arg1, arg2)  -> Buffer.add_string buf ("0000"^"0"^(reg_to_bin (snd arg1) context)^(bool_to_bin (fst arg1))^(reg_to_bin (snd arg2) context)^(bool_to_bin (fst arg2))^"0000000000")
          |SUBi(nb, arg)    -> Buffer.add_string buf ("0001"^"1"^(int_to_bin nb 16)^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg)))
          |AND(arg1, arg2)  -> Buffer.add_string buf ("0100"^"0"^(reg_to_bin (snd arg1) context)^(bool_to_bin (fst arg1))^(reg_to_bin (snd arg2) context)^(bool_to_bin (fst arg2))^"0000000000")
          |NOT(arg)         -> Buffer.add_string buf ("1100"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^"0000000000")
          |LSE(arg)         -> Buffer.add_string buf ("0010"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^"0000000000")
          |RSE(arg)         -> Buffer.add_string buf ("1010"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^"0000000000")
          |EQZ(arg)         -> Buffer.add_string buf ("0110"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (R32) context)^"0"^"0000000000")
          |EQZi(nb)         -> Buffer.add_string buf ("0110"^"1"^(int_to_bin nb 16)^(reg_to_bin (R32) context)^"0")
          |MTZ(arg)         -> Buffer.add_string buf ("1110"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (R32) context)^"0"^"0000000000")
          |MTZi(nb)         -> Buffer.add_string buf ("1110"^"1"^(int_to_bin nb 16)^(reg_to_bin (R32) context)^"0")
          |MACRO(name, args_l) ->
            let mac = Smap.find name context in
            let context2 = List.fold_left2 (fun ctxt2 nom_arg arg -> Smap.add (nom_arg) (reg_to_bin (snd arg) context) ctxt2) (Smap.empty) mac.arg args_l in
            assemble mac.body context2
    ); assemble q context )

let assemble_file file =
    let env = List.fold_left (fun mmap mac -> Smap.add (mac.nom) mac mmap) (Smap.empty) file.l_macro in
    assemble file.main env
*)




















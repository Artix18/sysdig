open Ast;;

let buf = Buffer.create 17
module Smap = Map.Make (string)

let bool_to_bin b = if b then "1" else "0"

let rec int_to_bin nb (?pos=16) = 
    if pos = 0 then ""
    else(
    if nb mod 2 = 0 then
        "0"^(int_to_bin (nb/2) (pos+1))
    else
        "1"^(int_to_bin (nb/2) (pos+1))
    )

let reg_to_bin reg context =
    match reg with
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
    |Var(str) -> Smap.find str context (*5 bits*)

let rec assemble instr_l context = match instr_l with
    | [] -> ()
    | p::q ->(
    (
        match p with
          |MOVE(arg1, arg2) -> Buffer.add_string buf ("1000"^"0"^(reg_to_bin (snd arg1) context)^(bool_to_bin (fst arg1))^(reg_to_bin (snd arg2) context)^(bool_to_bin (fst arg2))^"0000000000")
          |MOVEi(nb,arg)    -> Buffer.add_string buf ("1000"^"1"^(int_to_bin nb)^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg)))
          |ADD(arg1,arg2)   -> Buffer.add_string buf ("0000"^"0"^(reg_to_bin (snd arg1) context)^(bool_to_bin (fst arg1))^(reg_to_bin (snd arg2) context)^(bool_to_bin (fst arg2))^"0000000000")
          |ADDi(nb,arg)     -> Buffer.add_string buf ("0000"^"1"^(int_to_bin nb)^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg)))
          |SUB(arg1, arg2)  -> Buffer.add_string buf ("0000"^"0"^(reg_to_bin (snd arg1) context)^(bool_to_bin (fst arg1))^(reg_to_bin (snd arg2) context)^(bool_to_bin (fst arg2))^"0000000000")
          |SUBi(nb, arg)    -> Buffer.add_string buf ("0001"^"1"^(int_to_bin nb)^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg)))
          |AND(arg1, arg2)  -> Buffer.add_string buf ("0100"^"0"^(reg_to_bin (snd arg1) context)^(bool_to_bin (fst arg1))^(reg_to_bin (snd arg2) context)^(bool_to_bin (fst arg2))^"0000000000")
          |NOT(arg)         -> Buffer.add_string buf ("1100"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^"0000000000")
          |LSE(arg)         -> Buffer.add_string buf ("0010"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^"0000000000")
          |RSE(arg)         -> Buffer.add_string buf ("1010"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^"0000000000")
          |EQZ(arg)         -> Buffer.add_string buf ("0110"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (R32) context)^"0"^"0000000000")
          |EQZi(nb)         -> Buffer.add_string buf ("0110"^"1"^(int_to_bin nb)^(reg_to_bin (R32) context)^"0")
          |MTZ(arg)         -> Buffer.add_string buf ("1110"^"0"^(reg_to_bin (snd arg) context)^(bool_to_bin (fst arg))^(reg_to_bin (R32) context)^"0"^"0000000000")
          |MTZi(nb)         -> Buffer.add_string buf ("1110"^"1"^(int_to_bin nb)^(reg_to_bin (R32) context)^"0")
          |MACRO(name, args_l) ->
            let mac = Smap.find name macros in
            let context2 = List.fold_left2 (fun ctxt2 nom_arg arg -> Smap.add (nom_arg) (reg_to_bin (snd arg) context)) (Smap.empty) mac.arg args_l in
            assemble mac.body context2
    ); assemble q context )

let assemble_file file =
    let env = List.fold_left (fun mmap mac -> Smap.add (mac.nom) mac mmap) (Smap.empty) file.l_macro in
    assemble file.main env


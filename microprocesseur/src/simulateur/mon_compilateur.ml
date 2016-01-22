open Netlist_ast;;

exception Non_conforme of string;;

let buf = Buffer.create 17;;

let string_of_bit b =
  if b then
    "1"
  else
    "0"
;;

let is_reg l_reg id =
  List.mem id l_reg
;;

let list_reg l_eq =
  let rec loop l_eq out =
    match l_eq with
    |[] -> out
    |h::t -> begin
      match snd h with
      |Ereg(id) -> loop t (id::out)
      |_ -> loop t out
    end
  in
  loop l_eq []
;;

let list_reg_eq l_reg l_eq =
  List.map (fun reg -> List.find (fun x -> reg = fst x) l_eq) l_reg
;;

let write_include () =
  Buffer.add_string buf "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <stdint.h>\n#include <time.h>\n\n"
;;

"void display(char** RAM, int i, int len) {\nint j, nb = 0;\nfor(j = len - 1 ; j >= 0 ; j--)\nnb = nb * 2 + RAM[i][j];\nprintf(\"%d\n\", nb);\n}\n\n"


let write_functions () =
  (* Fonction file_len *)
  Buffer.add_string buf "int file_len(FILE* f) {\nint ret;\nfor(ret = 0 ; fgetc(f) != EOF ; ret++);\nrewind(f);\nreturn ret;\n}\n\n";
  (* Fonction inputs *)
  Buffer.add_string buf "void inputs(char* out, char* INPUT, int* ind) {\n*out = INPUT[*ind];\n(*ind)++;\n}\n\n";
  (* Fonction inputs_tab *)
  Buffer.add_string buf "void inputs_tab(char* out, int len, char* INPUT, int* ind) {\nint i;\nfor(i = 0 ; i < len ; i++) {\nout[i] = INPUT[*ind];\n(*ind)++;\n}\n}\n\n";
  (* Fonction outputs_tab*)
  Buffer.add_string buf "void outputs_tab(char* out, int len) {\nint i;\nfor(i = 0 ; i < len ; i++)\nprintf(\"%d\", (int) out[i]);\nprintf(\"\\n\");}\n\n";
  (* Fonction display*)
  Buffer.add_string buf "void display(char** RAM, int i, int len) {\nint j;\nuint64_t nb = 0;\nfor(j = len - 1 ; j >= 0 ; j--)\nnb = nb * 2 + RAM[i][j];\nprintf(\"%lld\\n\", (long long int) nb);\n}\n\n";
  (* Fonction concat *)
  Buffer.add_string buf "void concat(char* dest, char* a1, char* a2, int len1, int len2) {\nint i;\nfor(i = 0 ; i < len1 ; i++)\ndest[i] = a1[i];\nfor(i = 0 ; i < len2 ; i++)\ndest[i + len1] = a2[i];\n}\n\n";
  (* Fonction concat_bit_bit *)
  Buffer.add_string buf "void concat_bit_bit(char* dest, char b1, char b2) {\ndest[0] = b1;\ndest[1] = b2;\n}\n\n";
  (* Fonction concat_bit_tab *)
  Buffer.add_string buf "void concat_bit_tab(char* dest, char b1, char* a2, int len2) {\nint i;\ndest[0] = b1;\nfor(i = 0 ; i < len2 ; i++)\ndest[i+1] = a2[i];\n}\n\n";
  (* Fonction concat_tab_bit *)
  Buffer.add_string buf "void concat_tab_bit(char* dest, char* a1, char b2, int len1) {\nint i;\ndest[len1] = b2;\nfor(i = 0 ; i < len1 ; i++)\ndest[i] = a1[i];\n}\n\n";
  (* Fonction slice *)
  Buffer.add_string buf "void slice(char* dest, int deb, int fin, char* src) {\nint i;\nfor(i = 0 ; i <= fin - deb ; i++)\ndest[i] = src[i + deb];\n}\n\n";
  (* Fonction update_time *)
  Buffer.add_string buf "void update_time(char** RAM, int word_size) {\nint i;\nuint64_t tmp = clock() / CLOCKS_PER_SEC;\nfor(i = 0 ; i < word_size ; i++) {\n*((*RAM) + i) = tmp % 2;\ntmp /= 2;\n}\n}\n"
;;

let write_main_head () =
  Buffer.add_string buf "int main(int argc, char *argv[]) {\nuint64_t __mon_ind2__;\nint __mon_i1__, __mon_i2__, __mon_j1__, __mon_j2__, __TAILLE_RAM__, __TAILLE_ROM__, ind = 0;\nFILE* f;\nint n = atoi(argv[1]);\n"
;;

let write_open_f_input () =
  Buffer.add_string buf "f = fopen(argv[2], \"r\");\n";
;;

let write_read_file () =
  Buffer.add_string buf "INPUT = malloc(file_len(f) * sizeof(char));\n";
  Buffer.add_string buf "for(__mon_i1__ = 0 ; (__mon_j1__ = fgetc(f)) != EOF ; __mon_i1__++)\nINPUT[__mon_i1__] = __mon_j1__ - 48;\n"
;;

let write_close_f_input () =
  Buffer.add_string buf "fclose(f);\n"
;;

let write_declarations l_var l_ty =
  let write_var v =
    match Env.find v l_ty with
    |TBit -> Buffer.add_string buf v
    |TBitArray(n) ->
      Buffer.add_string buf v;
      Buffer.add_string buf "[";
      Buffer.add_string buf (string_of_int n);
      Buffer.add_string buf "]"
  in
  let rec write l_var =
    match l_var with
    |[] -> ()
    |[h] ->
      write_var h;
      Buffer.add_string buf ";\n";
    |h::t ->
      write_var h;
      Buffer.add_string buf ", ";
      write t
  in
  Buffer.add_string buf "char ";
  write l_var
;;

let write_memory_declaration l_eq memory f =
  let rec find_size l_eq =
    match l_eq with
    |[] -> ("", 0, 0)
    |h::t ->
      if memory = "ROM" then begin
        match snd h with
        |Erom(addr_size, word_size, _) ->
          (fst h, addr_size, word_size)
        |_ -> find_size t
      end
      else begin
        match snd h with
        |Eram(addr_size, word_size, _, _, _, _) ->
          (fst h, addr_size, word_size)
        |_ -> find_size t
      end
  in
  let rec write_mem size =
    match size with
    |(name, addr_size, word_size) ->
      Buffer.add_string buf (Printf.sprintf "f = fopen(%S, \"r\");\n__TAILLE_%s__ = (file_len(f) + 3) / %d;\nchar** %s = malloc(__TAILLE_%s__ * sizeof(char*));\n" f memory (if word_size = 0 then 1 else word_size) memory memory);
      Buffer.add_string buf
      (Printf.sprintf "for(__mon_i1__ = 0 ; __mon_i1__ < __TAILLE_%s__ ; __mon_i1__++)\n*(%s + __mon_i1__) = malloc(%d * sizeof(char));\nfclose(f);\n" memory memory word_size)
  in
  let size = find_size l_eq in
  write_mem size;
  size
;;

let rec write_init_memory_ROM size f_ROM =
  match size with
  |(name, addr_size, word_size) ->
    Buffer.add_string buf "f = fopen(\"";
    Buffer.add_string buf f_ROM;
    Buffer.add_string buf"\", \"r\");\n";
    Buffer.add_string buf "for(__mon_i1__ = 0 ; __mon_i1__ < __TAILLE_ROM__";
    Buffer.add_string buf " ; __mon_i1__++)\nfor(__mon_j1__ = 0 ; __mon_j1__ < ";
    Buffer.add_string buf (string_of_int word_size);
    Buffer.add_string buf " ; __mon_j1__++)\n*(*(ROM + __mon_i1__) + __mon_j1__) = ((char) fgetc(f)) - 48;\n";
    Buffer.add_string buf "fclose(f);\n"
;;

let rec write_init_memory_RAM size f_RAM =
  match size with
  |(name, addr_size, word_size) ->
    Buffer.add_string buf "f = fopen(\"";
    Buffer.add_string buf f_RAM;
    Buffer.add_string buf"\", \"r\");\n";
    Buffer.add_string buf "for(__mon_i1__ = 0 ; __mon_i1__ < __TAILLE_RAM__";
    Buffer.add_string buf " ; __mon_i1__++)\nfor(__mon_j1__ = 0 ; __mon_j1__ < ";
    Buffer.add_string buf (string_of_int word_size);
    Buffer.add_string buf " ; __mon_j1__++)\n*(*(RAM + __mon_i1__) + __mon_j1__) = ((char) fgetc(f)) - 48;\n";
    Buffer.add_string buf "fclose(f);\n"
;;

let rec write_update_RAM size l_eq =
  List.iter (fun x ->
    match size with
    |(name, addr_size, word_size) -> begin
      match snd x with
      |Eram(_, _, _, we, wa, data) -> begin
        match we, wa, data with
        |Avar(id1), Avar(id2), Avar(id3) ->
          Buffer.add_string buf (Printf.sprintf "__mon_ind2__ = 0;\nif(%s) {\nfor(__mon_i2__ = %d - 1 ; __mon_i2__ >= 0 ; __mon_i2__--)\n__mon_ind2__ = __mon_ind2__ * 2 + " id1 addr_size);
          Buffer.add_string buf id2;
          Buffer.add_string buf "[__mon_i2__];\nmemcpy(*(RAM + __mon_ind2__), ";
          Buffer.add_string buf id3;
          Buffer.add_string buf ", ";
          Buffer.add_string buf (string_of_int word_size);
          Buffer.add_string buf " * sizeof (char));\n}";
        |Aconst(VBit(b1)), Avar(id2), Avar(id3) ->
          Buffer.add_string buf (Printf.sprintf "__mon_ind2__ = 0;\nif(%s) {\nfor(__mon_i2__ = %d - 1 ; __mon_i2__ >= 0 ; __mon_i2__--)\n__mon_ind2__ = __mon_ind2__ * 2 + " (string_of_bit b1) addr_size);
          Buffer.add_string buf id2;
          Buffer.add_string buf "[__mon_i2__];\nmemcpy(*(RAM + __mon_ind2__), ";
          Buffer.add_string buf id3;
          Buffer.add_string buf ", ";
          Buffer.add_string buf (string_of_int word_size);
          Buffer.add_string buf " * sizeof (char));\n}";
        |_ -> raise (Non_conforme "Arguments constants d'une RAM non compris dans l'implémentation !")
      end
      |_ -> ()
    end) l_eq
;;

let rec write_init_reg l_reg =
  match l_reg with
  |[] -> ()
  |h::t ->
    Buffer.add_string buf h;
    Buffer.add_string buf " = 0;\n";
    write_init_reg t
;;

let write_loop_head size_RAM =
  let (name, addr_size, word_size) = size_RAM in
  Buffer.add_string buf "for(__mon_i1__ = 0 ; __mon_i1__ != n ; __mon_i1__++) {\n";
(*  Buffer.add_string buf "printf(\"Cycle Numero : %d\\n\", __mon_i1__);fflush(stdout);\n";*)
  Buffer.add_string buf (Printf.sprintf "update_time(RAM, %d);\n" word_size);
;;

let write_loop_foot () =
  Buffer.add_string buf "}\n"
;;

let write_program_foot () =
  Buffer.add_string buf "return 0;\n}\n"
;;

let rec write_inputs l_in l_ty =
  match l_in with
  |[] -> ()
  |h::t ->
    begin
      match Env.find h l_ty with
      |TBit ->
        Buffer.add_string buf "inputs(&";
        Buffer.add_string buf h;
      |TBitArray(n) ->
        Buffer.add_string buf "inputs_tab(";
        Buffer.add_string buf h;
        Buffer.add_string buf ", ";
        Buffer.add_string buf (string_of_int n);
    end;
    Buffer.add_string buf ", INPUT, &ind);\n";
    write_inputs t l_ty
;;

let rec write_display size =
  match size with
  |(name, addr_size, word_size) ->
  (*  Buffer.add_string buf "printf(\"PC_new :\\n\");fflush(stdout);outputs_tab(PC_new, 32);\n";
  Buffer.add_string buf "printf(\"FLAG_new :\\n\");fflush(stdout);outputs_tab(FLAG_new, 32);\n";
  Buffer.add_string buf "printf(\"OVERFLOW_new :\\n\");fflush(stdout);outputs_tab(OVERFLOW_new, 32);\n";
  Buffer.add_string buf "printf(\"R29_new :\\n\");fflush(stdout);outputs_tab(R29_new, 32);\n";
  Buffer.add_string buf "printf(\"R28_new :\\n\");fflush(stdout);outputs_tab(R28_new, 32);\n";
  Buffer.add_string buf "printf(\"R27_new :\\n\");fflush(stdout);outputs_tab(R27_new, 32);\n";
  Buffer.add_string buf "printf(\"R26_new :\\n\");fflush(stdout);outputs_tab(R26_new, 32);\n";
  Buffer.add_string buf "printf(\"R25_new :\\n\");fflush(stdout);outputs_tab(R25_new, 32);\n";
  Buffer.add_string buf "printf(\"R24_new :\\n\");fflush(stdout);outputs_tab(R24_new, 32);\n";
  Buffer.add_string buf "printf(\"R23_new :\\n\");fflush(stdout);outputs_tab(R23_new, 32);\n";
  Buffer.add_string buf "printf(\"R22_new :\\n\");fflush(stdout);outputs_tab(R22_new, 32);\n";
  Buffer.add_string buf "printf(\"R21_new :\\n\");fflush(stdout);outputs_tab(R21_new, 32);\n";
  Buffer.add_string buf "printf(\"R20_new :\\n\");fflush(stdout);outputs_tab(R20_new, 32);\n";
  Buffer.add_string buf "printf(\"R19_new :\\n\");fflush(stdout);outputs_tab(R19_new, 32);\n";
  Buffer.add_string buf "printf(\"R18_new :\\n\");fflush(stdout);outputs_tab(R18_new, 32);\n";
  Buffer.add_string buf "printf(\"R17_new :\\n\");fflush(stdout);outputs_tab(R17_new, 32);\n";
  Buffer.add_string buf "printf(\"R16_new :\\n\");fflush(stdout);outputs_tab(R16_new, 32);\n";
  Buffer.add_string buf "printf(\"R15_new :\\n\");fflush(stdout);outputs_tab(R15_new, 32);\n";
  Buffer.add_string buf "printf(\"R14_new :\\n\");fflush(stdout);outputs_tab(R14_new, 32);\n";
  Buffer.add_string buf "printf(\"R13_new :\\n\");fflush(stdout);outputs_tab(R13_new, 32);\n";
  Buffer.add_string buf "printf(\"R12_new :\\n\");fflush(stdout);outputs_tab(R12_new, 32);\n";
  Buffer.add_string buf "printf(\"R11_new :\\n\");fflush(stdout);outputs_tab(R11_new, 32);\n";
  Buffer.add_string buf "printf(\"R10_new :\\n\");fflush(stdout);outputs_tab(R10_new, 32);\n";
  Buffer.add_string buf "printf(\"R9_new :\\n\");fflush(stdout);outputs_tab(R9_new, 32);\n";
  Buffer.add_string buf "printf(\"R8_new :\\n\");fflush(stdout);outputs_tab(R8_new, 32);\n";
  Buffer.add_string buf "printf(\"R7_new :\\n\");fflush(stdout);outputs_tab(R7_new, 32);\n";
  Buffer.add_string buf "printf(\"R6_new :\\n\");fflush(stdout);outputs_tab(R6_new, 32);\n";
  Buffer.add_string buf "printf(\"R5_new :\\n\");fflush(stdout);outputs_tab(R5_new, 32);\n";
  Buffer.add_string buf "printf(\"R4_new :\\n\");fflush(stdout);outputs_tab(R4_new, 32);\n";
  Buffer.add_string buf "printf(\"R3_new :\\n\");fflush(stdout);outputs_tab(R3_new, 32);\n";
  Buffer.add_string buf "printf(\"R2_new :\\n\");fflush(stdout);outputs_tab(R2_new, 32);\n";
  Buffer.add_string buf "printf(\"R1_new :\\n\");fflush(stdout);outputs_tab(R1_new, 32);\n";
  Buffer.add_string buf "printf(\"res :\\n\");fflush(stdout);outputs_tab(res, 32);\n";*)
    Buffer.add_string buf  "\nif(__mon_i1__ % 1000 == 0) {\nsystem(\"clear\");\n";
    Buffer.add_string buf (Printf.sprintf "display(RAM, 1, %d);\n" word_size);
    Buffer.add_string buf (Printf.sprintf "display(RAM, 2, %d);\n" word_size);
    Buffer.add_string buf (Printf.sprintf "display(RAM, 3, %d);\n" word_size);
    Buffer.add_string buf (Printf.sprintf "display(RAM, 4, %d);\n" word_size);
    Buffer.add_string buf (Printf.sprintf "display(RAM, 5, %d);}\n" word_size);
;;

let rec write_outputs l_out l_reg l_ty =
  match l_out with
  |[] -> ()
  |h::t ->
  begin
    match Env.find h l_ty with
    |TBit ->
      Buffer.add_string buf "printf(\"";
      Buffer.add_string buf h;
      Buffer.add_string buf " = %d\\n\", (int) ";
      Buffer.add_string buf h;
      Buffer.add_string buf ");\n"
    |TBitArray(n) ->
      Buffer.add_string buf "outputs_tab(";
      Buffer.add_string buf h;
      Buffer.add_string buf ", ";
      Buffer.add_string buf (string_of_int n);
      Buffer.add_string buf ");\n"
  end;
    write_outputs t l_reg l_ty
;;

let write_val v =
  match v with
  |VBit(b) ->
    if b then
      Buffer.add_string buf "1"
    else
      Buffer.add_string buf "0"
  |VBitArray(b_array) -> raise (Non_conforme "Nappe de fils constante non comprise dans l'implémentation !")
;;

let write_arg a =
  match a with
  |Avar(id) -> Buffer.add_string buf id;
  |Aconst(v) -> write_val v
;;


let write_eq eq l_reg l_ty =
  let left_v = fst eq in
  if List.mem left_v l_reg then
    ()
  else (
  (* Ecriture de la partie gauche de l'équation *)
  (match snd eq with
  |Ereg(_) |Enot(_) |Ebinop(_, _, _) |Emux(_, _, _) |Eselect(_, _) ->
    Buffer.add_string buf left_v;
    begin
      match Env.find left_v l_ty with
      |TBitArray(_) -> Buffer.add_string buf "[0]"
      |_ -> ()
    end;
    Buffer.add_string buf " = "
  |_ -> ());
  (* Ecriture de la partie droite de l'équation (le calcul)*)
  (match snd eq with
  |Earg(a) -> begin
    match a with
    |Aconst(v) ->
      Buffer.add_string buf left_v;
      Buffer.add_string buf " = ";
      write_val v
    |Avar(id) -> begin
      match Env.find left_v l_ty with
      |TBit ->
        Buffer.add_string buf left_v;
        Buffer.add_string buf " = ";
        Buffer.add_string buf id
      |TBitArray(len) ->
        Buffer.add_string buf "memcpy(";
        Buffer.add_string buf left_v;
        Buffer.add_string buf ", ";
        (match Env.find id l_ty with
         |TBitArray(_) -> Buffer.add_string buf id;
         |TBit -> Buffer.add_string buf "&"; Buffer.add_string buf id);
        Buffer.add_string buf ", ";
        Buffer.add_string buf (string_of_int len);
        Buffer.add_string buf " * sizeof(char))"
    end
  end
  |Ereg(id) ->
    Buffer.add_string buf id
  |Enot(a) ->
    Buffer.add_string buf "!";
    write_arg a
  |Ebinop(op, a1, a2) -> begin
    match op with
    |Or ->
      write_arg a1;
      Buffer.add_string buf " || ";
      write_arg a2
    |Xor ->
      write_arg a1;
      Buffer.add_string buf " ^ ";
      write_arg a2
    |And ->
      write_arg a1;
      Buffer.add_string buf " && ";
      write_arg a2
    |Nand ->
      Buffer.add_string buf "!(";
      write_arg a1;
      Buffer.add_string buf " && ";
      write_arg a2;
      Buffer.add_string buf ")"
  end
  |Emux(a1, a2, a3) -> (* a1 vaut 0 alors a2 sinon a3 Il faut inverser !!!!!!!!! *)
    Buffer.add_string buf "(";
    write_arg a1;
    Buffer.add_string buf ")?";
    write_arg a3;
    Buffer.add_string buf ":";
    write_arg a2
  |Erom(addr_size, word_size, a) -> begin
    match a with
    |Avar(id) ->
      Buffer.add_string buf (Printf.sprintf "__mon_ind2__ = 0;\nfor(__mon_i2__ = %d - 1 ; __mon_i2__ >= 0 ; __mon_i2__--)\n__mon_ind2__ = __mon_ind2__ * 2 + " addr_size);
      Buffer.add_string buf id;
      Buffer.add_string buf "[__mon_i2__];\nif(__mon_ind2__ < __TAILLE_ROM__)\nmemcpy(";
      Buffer.add_string buf left_v;
      Buffer.add_string buf ", *(ROM + __mon_ind2__), ";
      Buffer.add_string buf (string_of_int word_size);
      Buffer.add_string buf " * sizeof (char));\n"
    |Aconst(v) -> raise (Non_conforme "Adresse constante d'une ROM non comprise dans l'implémentation !")
  end
  |Eram(addr_size, word_size, a_read_addr, a_write_enable, a_write_addr, a_data) -> begin
    match a_read_addr, a_write_enable, a_write_addr, a_data with
    |Avar(id1), _, _, _ ->
      Buffer.add_string buf (Printf.sprintf "__mon_ind2__ = 0;\nfor(__mon_i2__ = %d - 1 ; __mon_i2__ >= 0 ; __mon_i2__--)\n__mon_ind2__ = __mon_ind2__ * 2 + " addr_size);
      Buffer.add_string buf id1;
      Buffer.add_string buf "[__mon_i2__];\nif(__mon_ind2__ < __TAILLE_RAM__)\nmemcpy(";
      Buffer.add_string buf left_v;
      Buffer.add_string buf ", *(RAM + __mon_ind2__), ";
      Buffer.add_string buf (string_of_int word_size);
      Buffer.add_string buf " * sizeof (char));\n";
    |_ -> raise (Non_conforme "Arguments constants d'une RAM non compris dans l'implémentation !")
  end
  |Econcat(a1, a2) -> begin
    match a1, a2 with
    |Avar(id1), Avar(id2) -> begin
      Buffer.add_string buf "concat(";
      Buffer.add_string buf left_v;
      Buffer.add_string buf ", ";
      match Env.find id1 l_ty, Env.find id2 l_ty with
      |TBitArray(len1), TBitArray(len2) ->
        Buffer.add_string buf id1;
        Buffer.add_string buf ", ";
        Buffer.add_string buf id2;
        Buffer.add_string buf ", ";
        Buffer.add_string buf (string_of_int len1);
        Buffer.add_string buf ", ";
        Buffer.add_string buf (string_of_int len2);
        Buffer.add_string buf ")"
      |TBitArray(len1), TBit ->
        Buffer.add_string buf id1;
        Buffer.add_string buf ", &";
        Buffer.add_string buf id2;
        Buffer.add_string buf ", ";
        Buffer.add_string buf (string_of_int len1);
        Buffer.add_string buf ", 1)"
      |TBit, TBitArray(len2) ->
        Buffer.add_string buf "&";
        Buffer.add_string buf id1;
        Buffer.add_string buf ", ";
        Buffer.add_string buf id2;
        Buffer.add_string buf ", 1, ";
        Buffer.add_string buf (string_of_int len2);
        Buffer.add_string buf ")"
		  |TBit, TBit ->
		    Buffer.add_string buf "&";
        Buffer.add_string buf id1;
        Buffer.add_string buf ", &";
        Buffer.add_string buf id2;
        Buffer.add_string buf ", 1, 1)";
    end
    |Aconst(VBit(b1)), Aconst(VBit(b2)) -> begin
      Buffer.add_string buf "concat_bit_bit(";
      Buffer.add_string buf left_v;
      Buffer.add_string buf ", ";
      Buffer.add_string buf (string_of_bit b1);
      Buffer.add_string buf ", ";
      Buffer.add_string buf (string_of_bit b2);
      Buffer.add_string buf ")";
    end
    |Avar(id1), Aconst(VBit(b2)) -> begin
      Buffer.add_string buf "concat_tab_bit(";
      Buffer.add_string buf left_v;
      Buffer.add_string buf ", ";
      match Env.find id1 l_ty with
      |TBitArray(len1) ->
        Buffer.add_string buf id1;
        Buffer.add_string buf ", ";
        Buffer.add_string buf (string_of_bit b2);
        Buffer.add_string buf ", ";
        Buffer.add_string buf (string_of_int len1);
        Buffer.add_string buf ")"
      |TBit ->
        Buffer.add_string buf "&";
        Buffer.add_string buf id1;
        Buffer.add_string buf ", ";
        Buffer.add_string buf (string_of_bit b2);
        Buffer.add_string buf ", 1)"
    end
    |Aconst(VBit(b1)), Avar(id2) -> begin
      Buffer.add_string buf "concat_bit_tab(";
      Buffer.add_string buf left_v;
      Buffer.add_string buf ", ";
      match Env.find id2 l_ty with
      |TBitArray(len2) ->
        Buffer.add_string buf (string_of_bit b1);
        Buffer.add_string buf ", ";
        Buffer.add_string buf id2;
        Buffer.add_string buf ", ";
        Buffer.add_string buf (string_of_int len2);
        Buffer.add_string buf ")"
      |TBit ->
        Buffer.add_string buf (string_of_bit b1);
        Buffer.add_string buf ", &";
        Buffer.add_string buf id2;
        Buffer.add_string buf ", 1)";
    end
    |_ -> raise (Non_conforme "Arguments constants de concat non compris dans l'implémentation !")
  end
  |Eslice(i, j, a) -> begin
    match a with
    |Avar(id) ->
      Buffer.add_string buf "slice(";
      Buffer.add_string buf left_v;
      Buffer.add_string buf ", ";
      Buffer.add_string buf (string_of_int i);
      Buffer.add_string buf ", ";
      Buffer.add_string buf (string_of_int j);
      Buffer.add_string buf ", ";
      Buffer.add_string buf id;
      Buffer.add_string buf ")"
    |_ -> raise (Non_conforme "Arguments constants de slice non compris dans l'implémentation !")
  end
  |Eselect(i, a) -> begin
    match a with
    |Avar(id) -> begin
      match Env.find id l_ty with
      |TBitArray(_) ->
        Buffer.add_string buf id;
        Buffer.add_string buf "[";
        Buffer.add_string buf (string_of_int i);
        Buffer.add_string buf "]"
      |TBit ->
        Buffer.add_string buf id
    end
    |_ -> raise (Non_conforme "Arguments constants de select non compris dans l'implémentation !")
  end);
  Buffer.add_string buf ";\n")
;;

let rec write_leq l_eq l_reg l_ty =
  Printf.printf "write_leq, à traiter ----> %d" (List.length l_eq);
  print_endline "";
  match l_eq with
  |[] -> ()
  |h::t ->
    write_eq h l_reg l_ty;
    write_leq t l_reg l_ty
;;

let print_on_file file =
  let f = open_out file in
  Buffer.output_buffer f buf
;;

let list_var prog =
  let rec loop l_eq out =
    match l_eq with
    |[] -> out
    |h::t -> loop t ((fst h)::out)
  in
  loop prog.p_eqs prog.p_inputs
;;

let read_program file_prog =
  let p = Netlist.read_file file_prog in
  Printf.printf "NETLIST LU";
  print_endline "";
  try
    Scheduler.schedule p
  with
  |Scheduler.Combinational_cycle ->
    Format.eprintf "The netlist has a combinatory cycle.@.";
    exit 2
;;

let compil file file_prog f_ROM f_RAM =
  Printf.printf "DEBUT COMPILATION";
  print_endline "";
  let prog = read_program file_prog in
  Printf.printf "PROGRAMME LU";
  print_endline "";
  let l_in = prog.p_inputs in
  let l_out = prog.p_outputs in
  let l_eq = prog.p_eqs in
  let l_var = list_var prog in
  let l_reg = list_reg l_eq in
  let l_reg_eq = list_reg_eq l_reg l_eq in
  let l_ty = prog.p_vars in
  
  (* Initialisation *)
  write_include ();
  write_functions ();
  write_main_head ();
  let size_ROM = write_memory_declaration l_eq "ROM" f_ROM in
  let size_RAM = write_memory_declaration l_eq "RAM" f_RAM in
  write_declarations l_var l_ty;
  write_init_reg l_reg;
  write_init_memory_ROM size_ROM f_ROM;
  write_init_memory_RAM size_RAM f_RAM;
  
  
  (* Corps de la boucle *)
  write_loop_head size_RAM;
  write_leq l_eq l_reg l_ty;
  write_leq l_reg_eq [] l_ty;
  write_update_RAM size_RAM l_eq;
  write_display size_RAM;
  write_loop_foot ();
  
  (* Fin du programme *)
  write_program_foot ();
  print_on_file file
;;









































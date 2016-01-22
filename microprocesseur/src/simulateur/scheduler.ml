open Netlist_ast
open Graph

exception Combinational_cycle

module Meq = Map.Make(String);;

type map_eq = exp Meq.t;;

let read_exp eq =
  let rec id_list l_a out =
    match l_a with
    |(Avar(id))::t -> id_list t (id::out)
    |_::t -> id_list t out
    |_ -> out
  in
  match snd eq with
  |Earg(a) -> id_list [a] []
  |Ereg id -> []
  |Enot(a) -> id_list [a] []
  |Ebinop(op, a1, a2) -> id_list [a1 ; a2] []
  |Emux(a1, a2, a3) -> id_list [a1 ; a2 ; a3] []
  |Erom(addr_size, word_size, a) -> id_list [a] []
  |Eram(addr_size, word_size, a1, a2, a3, a4) -> id_list [a1 ; a2 ; a3 ; a4] []
  |Econcat(a1, a2) -> id_list [a1 ; a2] []
  |Eslice(_, _, a) -> id_list [a] []
  |Eselect(_, a) -> id_list [a] []
;;

let print_type vars =
  Env.iter
    (fun k -> fun v ->
      match v with
      |TBit -> Printf.printf "%s est un bit\n" k; flush stdout
      |TBitArray(n) -> Printf.printf "%s est une nappe de bits de taille %d\n" k n; flush stdout) 
    vars
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

let schedule p =
  let g = Graph.mk_graph ()
  and l_eq = p.p_eqs
  and l_in = p.p_inputs
  and l_out = p.p_outputs in
  let rec insert_lval_graph lval =
    match lval with
    |h::t -> 
      Graph.add_node g h false;
      insert_lval_graph t
    |_ -> ()
  in
  (* Les Registres et les RAM peuvent faire partie d'une boucle *)
  let rec insert_leq_graph leq l_reg =
    match leq with
    |(id, Eram(addr_size, word_size, a1, a2, a3, a4))::t ->
      Graph.add_node g id true;
      insert_leq_graph t l_reg
    |(id, _)::t ->
      if is_reg l_reg id then
        (Graph.add_node g id true;
        insert_leq_graph t l_reg)
      else
        (Graph.add_node g id false;
        insert_leq_graph t l_reg)
    |_ -> ()
  in
  let rec insert_edge_of_node s lv =
    match lv with
    |h::t ->
      Graph.add_edge g s h;
      insert_edge_of_node s t
    |_ -> ()
  in
  let rec insert_edge_of_graph leq =
    (*Printf.printf "insert_edge_of_graph, à traiter ----> %d" (List.length leq);
    print_endline "";*)
    match leq with
    |h::t ->
      insert_edge_of_node (fst h) (read_exp h);
      insert_edge_of_graph t
    |_-> ()
  in
  let rec delete_inputs lord out =
    match lord with
    |h::t ->
      if List.mem h l_in then
        delete_inputs t out
      else
        delete_inputs t (h::out)
    |[] -> out
  in
  let list_to_map l =
    List.fold_left (fun x y -> Meq.add (fst y) (snd y) x) Meq.empty l
  in
  let rec create_new_leq lord meq new_leq =
    (*Printf.printf "create_new_leq, à traiter ----> %d" (List.length lord);
    print_endline "";*)
    match lord with
    |h::t ->
      let ex = Meq.find h meq in
      create_new_leq t meq ((h, ex)::new_leq)
    |[] -> new_leq
  in
  let l_reg = list_reg l_eq in
  insert_lval_graph l_in;
  insert_leq_graph l_eq l_reg;
  insert_edge_of_graph l_eq;
  try
    let lvar_ord = Graph.topological g in
    print_type p.p_vars;
    {p_eqs = List.rev (create_new_leq (delete_inputs lvar_ord []) (list_to_map l_eq) []);
     p_inputs = l_in;
     p_outputs = l_out;
     p_vars = p.p_vars;}
  with Graph.Cycle -> raise Combinational_cycle
;;



















































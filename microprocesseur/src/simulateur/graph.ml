exception Cycle
type mark = NotVisited | InProgress | Visited

(* Ajout du parametre is_cycle dans la structure node
   Est placé à vrai si l'élément peut faire partie d'une boucle
   Dans ce cas là, on ne le liera pas à d'autres éléments
*)

module MStr = Map.Make(String);;

type 'a graph = {mutable g_node : 'a node MStr.t}
and 'a node = {
  n_label : 'a;
  is_cycle : bool;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = {g_node = MStr.empty}

let add_node g x c=
  let n = {n_label = x;
           is_cycle = c;
           n_mark = NotVisited;
           n_link_to = [];
           n_linked_by = []} in
  g.g_node <- MStr.add n.n_label n g.g_node
;;

let node_for_label g x =
  MStr.find x g.g_node

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  if n1.is_cycle = false then
  begin
    n1.n_link_to <- n2::n1.n_link_to;
    n2.n_linked_by <- n1::n2.n_linked_by
  end

let clear_marks g =
  MStr.iter (fun k n -> n.n_mark <- NotVisited) g.g_node

let list_of_map m =
  List.rev_map (fun (k, x) -> x) (MStr.bindings m)
;;

let find_roots g =
  let l_node = list_of_map g.g_node in
  List.filter (fun n -> n.n_linked_by = []) l_node

let has_cycle g =
  let rec find_cycle_from_node n =
    if n.n_mark = InProgress then
      true
    else if n.n_link_to = [] then
      false
    else if n.n_mark = Visited then
      false
    else begin
      n.n_mark <- InProgress;
      let lout = List.rev_map find_cycle_from_node n.n_link_to in
      n.n_mark <- Visited;
      List.fold_left (||) false lout
    end
  in
  let out = List.fold_left (||) false (List.rev_map find_cycle_from_node (list_of_map g.g_node)) in
  clear_marks g;
  out
;;

let topological g =
  let out = ref [] in
  let is_unvisited n =
    n.n_mark = NotVisited
  in
  let rec profond x =
    let rec appel_prof l_n =
      match l_n with
      |[] -> ()
      |h::t ->
      begin
        if is_unvisited h then
          profond h;
        appel_prof t
      end
    in
    x.n_mark <- Visited;
    appel_prof x.n_link_to;
    out := x.n_label::(!out)
  in
  if has_cycle g then
    raise Cycle
  else begin
    List.iter profond (find_roots g);
    clear_marks g;
    !out
  end
;;
      
      
      




















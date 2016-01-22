let main () =
  Printf.printf "Début de Compilation (peut prendre du temps)";
  print_endline "";
  let netlist = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let f_ROM = Sys.argv.(3) in
  let f_RAM = Sys.argv.(4) in
  Mon_compilateur.compil output_file netlist f_ROM f_RAM
;;

main ();

















































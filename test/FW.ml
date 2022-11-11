open Shortest_path

let classic () =
  let module FW = FW_Classic in
  let sp = FW.initiate 4 in
  let add_neighbors = FW.add_neighbors sp in
  add_neighbors 0 1 (-4.);
  add_neighbors 0 2 3.;
  add_neighbors 1 3 2.;
  add_neighbors 2 0 2.;
  add_neighbors 2 1 4.;
  add_neighbors 3 2 1.;
  FW.print sp;
  print_newline ();
  FW.run
    ~f:(fun e ->
      FW.print e;
      print_newline ())
    sp

let generalized () =
  let module FW = FW_Generalized in
  let sp = FW.initiate 4 in
  let add_neighbors = FW.add_neighbors sp in
  add_neighbors 0 1 (-4.);
  add_neighbors 0 2 3.;
  add_neighbors 1 3 2.;
  add_neighbors 2 0 2.;
  add_neighbors 2 1 4.;
  add_neighbors 3 2 1.;
  FW.print sp;
  print_newline ();
  FW.run ~f:FW.print sp

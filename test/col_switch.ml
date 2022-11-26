let main () =
  let open Switch_graph in
  (* Path taken from report *)
  let col_f =
    let tbl = ColorFunction.init () in
    ColorFunction.add tbl 0 1 (ColorSet.of_list [ 1; 2 ]);
    ColorFunction.add tbl 1 2 (ColorSet.of_list [ 3; 4; 1 ]);
    ColorFunction.add tbl 2 3 (ColorSet.of_list [ 4; 1; 2 ]);
    ColorFunction.add tbl 3 4 (ColorSet.of_list [ 4 ]);
    ColorFunction.add tbl 4 5 (ColorSet.of_list [ 3; 1; 2 ]);
    ColorFunction.add tbl 5 6 (ColorSet.of_list [ 3; 1 ]);
    tbl
  in
  let m = initiate col_f 7 in
  print m;
  run ~f:print m 6

let main' ?(stdout = stdout) () =
  let open Switch_graph in
  let graph = read_json "./test/graph2.json" in
  print ~stdout graph

let main_mdd () =
  let open Switch_graph_mdd in
  (* Path taken from report *)
  let col_f = ColorFunction.init () in
  ColorFunction.add col_f 0 1 (ColorSet.of_list [ 1; 2 ]);
  ColorFunction.add col_f 1 2 (ColorSet.of_list [ 3; 4; 1 ]);
  ColorFunction.add col_f 2 3 (ColorSet.of_list [ 4; 1; 2 ]);
  ColorFunction.add col_f 3 4 (ColorSet.of_list [ 4 ]);
  ColorFunction.add col_f 4 5 (ColorSet.of_list [ 3; 1; 2 ]);
  ColorFunction.add col_f 5 6 (ColorSet.of_list [ 3; 1 ]);
  ColorFunction.add col_f 1 7 (ColorSet.of_list [ 8 ]);
  ColorFunction.add col_f 7 0 (ColorSet.of_list [ 10 ]);
  ColorFunction.add col_f 1 8 (ColorSet.of_list [ 9 ]);
  ColorFunction.add col_f 8 0 (ColorSet.of_list [ 14 ]);
  let graph = initiate col_f 0 in
  print graph;
  Printf.fprintf stdout "------------\n";
  run
    ~f:(fun e ->
      print e;
      Printf.fprintf stdout "------------\n")
    graph 10

let main_mdd' ?(stdout = stdout) () =
  let open Switch_graph_mdd in
  let graph = read_json ~src:1 "./test/graph2.json" in
  print graph;
  let path_length = read_int () in
  let t = Sys.time () in
  Printf.fprintf stdout "------------\n";
  (* run ~f:ignore graph (read_int ()); *)
  run ~f:ignore graph path_length;
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  Printf.fprintf stdout "------------\n";
  print graph

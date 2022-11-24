let main () =
  let open Switch_graph in
  (* Path taken from report *)
  let col_f : color_function =
    let tbl = Hashtbl.create 100 in
    Hashtbl.add tbl (0, 1) (ColorSet.of_list [ 1; 2 ]);
    Hashtbl.add tbl (1, 2) (ColorSet.of_list [ 3; 4; 1 ]);
    Hashtbl.add tbl (2, 3) (ColorSet.of_list [ 4; 1; 2 ]);
    Hashtbl.add tbl (3, 4) (ColorSet.of_list [ 4 ]);
    Hashtbl.add tbl (4, 5) (ColorSet.of_list [ 3; 1; 2 ]);
    Hashtbl.add tbl (5, 6) (ColorSet.of_list [ 3; 1 ]);
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
  let col_f = Color_function.init () in
  Color_function.add col_f 0 1 (ColorSet.of_list [ 1; 2 ]);
  Color_function.add col_f 1 2 (ColorSet.of_list [ 3; 4; 1 ]);
  Color_function.add col_f 2 3 (ColorSet.of_list [ 4; 1; 2 ]);
  Color_function.add col_f 3 4 (ColorSet.of_list [ 4 ]);
  Color_function.add col_f 4 5 (ColorSet.of_list [ 3; 1; 2 ]);
  Color_function.add col_f 5 6 (ColorSet.of_list [ 3; 1 ]);
  let graph = initiate col_f 0 in
  run
    ~f:(fun e ->
      print e;
      Printf.fprintf stdout "------------\n")
    graph 10

let main_mdd' ?(stdout = stdout) () =
  let open Switch_graph_mdd in
  let graph = read_json ~src:2 "./test/graph2.json" in
  print graph;
  Printf.fprintf stdout "------------\n";
  run
    ~f:(fun e ->
      print ~stdout e;
      Printf.fprintf stdout "------------\n")
    graph 100;
  Printf.fprintf stdout "------------\n";
  print graph

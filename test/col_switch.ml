let main () =
  let open Switch_graph in
  (* Path taken from report *)
  let col_f =
    let tbl = ColorFunction.init () in
    ColorFunction.add tbl 0 1 (MySet.ColorSet.of_list [ 1; 2 ]);
    ColorFunction.add tbl 1 2 (MySet.ColorSet.of_list [ 3; 4; 1 ]);
    ColorFunction.add tbl 2 3 (MySet.ColorSet.of_list [ 4; 1; 2 ]);
    ColorFunction.add tbl 3 4 (MySet.ColorSet.of_list [ 4 ]);
    ColorFunction.add tbl 4 5 (MySet.ColorSet.of_list [ 3; 1; 2 ]);
    ColorFunction.add tbl 5 6 (MySet.ColorSet.of_list [ 3; 1 ]);
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
  let open Switch_graph_mdd.Make (StateMddSet) in
  (* Path taken from report *)
  let col_f = ColorFunction.init () in
  ColorFunction.add col_f 0 1 (MySet.ColorSet.of_list [ 1; 2 ]);
  ColorFunction.add col_f 1 2 (MySet.ColorSet.of_list [ 3; 4; 1 ]);
  ColorFunction.add col_f 2 3 (MySet.ColorSet.of_list [ 4; 1; 2 ]);
  ColorFunction.add col_f 3 4 (MySet.ColorSet.of_list [ 4 ]);
  ColorFunction.add col_f 4 5 (MySet.ColorSet.of_list [ 3; 1; 2 ]);
  ColorFunction.add col_f 5 6 (MySet.ColorSet.of_list [ 3; 1 ]);
  ColorFunction.add col_f 1 7 (MySet.ColorSet.of_list [ 8 ]);
  ColorFunction.add col_f 7 0 (MySet.ColorSet.of_list [ 10 ]);
  ColorFunction.add col_f 1 8 (MySet.ColorSet.of_list [ 9 ]);
  ColorFunction.add col_f 8 0 (MySet.ColorSet.of_list [ 14 ]);
  let graph = initiate col_f 0 in
  print graph;
  Printf.fprintf stdout "------------\n";
  run
    ~f:(fun e ->
      print e;
      Printf.fprintf stdout "------------\n")
    graph 10

let main_mdd' ?(stdout = stdout) () =
  let open Switch_graph_mdd.Make (StateMddSet) in
  Printf.printf "Enter depth ";
  let depth = read_int () in
  Printf.printf "Enter src ";
  let src = read_int () in
  Printf.printf "Should print (0 = false, 1 = true) ";
  let f =
    if read_int () = 1 then (fun e ->
      print ~fathers:true e;
      Printf.fprintf stdout "------------\n")
    else ignore
  in

  (* print graph; *)
  (* for l = 0 to path_length do *)
  let l = depth * 1 in
  let graph = read_json ~src "./test/graph2.json" in
  let t = Sys.time () in
  (* Printf.fprintf stdout "------------\n"; *)
  (* run ~f:ignore graph (read_int ()); *)
  run ~f:ignore graph l;
  Printf.printf "%d: %f\n" l (Sys.time () -. t);
  f graph;
  print_int l;
  print_endline "";
  (* done; *)
  Printf.fprintf stdout "------------\n"
(* print graph *)

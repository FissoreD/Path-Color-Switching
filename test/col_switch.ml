open Switch_graph

let main () =
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
  let open MatrixShortestPath in
  let m = initiate col_f 7 in
  print m;
  run ~f:print m 6

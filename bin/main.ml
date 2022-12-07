let () =
  let open Arg in
  let depth = ref 10 in
  let verbose = ref false in
  let print_last = ref false in
  let src = ref 1 in
  let file_path = ref "./test/graph2.json" in
  let all_diff = ref false in
  let print_fathers = ref false in
  let to_count_paths = ref false in
  let speclist =
    align
      [
        ("-depth", Set_int depth, " Set the depth of research.");
        ( "-src",
          Set_int src,
          " Set the source node from which run create the MDD." );
        ( "-v",
          Set verbose,
          " Print each step of the algorithm execution in verbose mode." );
        ("-p", Set print_last, " Print only the last step of the layer.");
        ("-allDiff", Set all_diff, " Set the allDifferent constraint.");
        ("-c", Set to_count_paths, " Count the number of paths from the source");
        ( "-pFath",
          Set print_fathers,
          " Print the fathers list, if in verbose mode." );
        ( "-f",
          Set_string file_path,
          " Set the file path to read the input to parse." );
      ]
  in
  let usage_msg =
    "Here is the list of all the optional commands you are allowed to enter in \
     order to modify the default behaviour of the program"
  in
  Arg.parse speclist print_endline usage_msg;
  let m : (module Switch_graph_mdd.M) =
    if !all_diff then (module StateMddSetAllDiff) else (module StateMddSet)
  in
  let open Switch_graph_mdd.Make ((val m : Switch_graph_mdd.M)) in
  let print_function e =
    print ~fathers:!print_fathers e;
    Printf.fprintf stdout "------------\n"
  in
  let time_start = Sys.time () in
  let graph = read_json ~src:!src !file_path in
  run ~f:(if !verbose then print_function else ignore) graph !depth;
  let time_end = Sys.time () in
  if !print_last && not !verbose then print_function graph;
  Printf.printf "The chosen depth is %d with a running time of %f. " !depth
    (time_end -. time_start);
  if !to_count_paths then
    Printf.printf "Total path found = %d\n" (count_paths graph)
  else Printf.printf "\n"

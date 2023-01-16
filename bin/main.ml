type params = {
  all_diff : bool ref;
  print_fathers : bool ref;
  file_path : string ref;
  path_length : int ref;
  print_last : bool ref;
  verbose : bool ref;
  to_count_paths : bool ref;
  src : int ref;
  loop : bool ref;
}

let run
    {
      all_diff;
      print_fathers;
      file_path;
      path_length;
      to_count_paths;
      src;
      loop;
      print_last;
      verbose;
    } =
  let m : (module Mdd.State) =
    if !all_diff then (module StateMddSetAllDiff) else (module StateMddSet)
  in
  let module M = (val m : Mdd.State) in
  let open Switch_graph_mdd.Make (M) in
  let print_result e =
    print ~fathers:!print_fathers e;
    Printf.fprintf stdout "------------\n"
  in
  let graph = read_json ~src:!src !file_path in
  let time_start = ref (Sys.time ()) in
  for depth = 1 to !path_length do
    print_endline "";
    make_iteration graph;
    let time_end = Sys.time () in
    if false && !path_length = depth then keep_min_in_last_layer graph;
    if (((not !loop) && depth = !path_length) || !loop) && !verbose then
      print_result graph;
    if !print_last && (not !verbose) && !path_length = depth then
      print_result graph;
    if ((not !loop) && depth = !path_length) || !loop then (
      Printf.printf
        "The src is : %d, the path length is %d with a running time of %f ! \n"
        !src depth (time_end -. !time_start);
      if !to_count_paths then
        Printf.printf "Total paths found = %d\n" (count_paths graph));
    time_start := !time_start +. Sys.time () -. time_end
  done

let () =
  let open Arg in
  let path_length = ref 10 in
  let verbose = ref false in
  let print_last = ref false in
  let src = ref 1 in
  let file_path = ref "./test/graph2.json" in
  let all_diff = ref false in
  let print_fathers = ref false in
  let to_count_paths = ref false in
  let loop = ref false in
  let speclist =
    align
      [
        ("-length", Set_int path_length, " Set the length of path (deafult 10).");
        ("-src", Set_int src, " Set the source of the path (default 1).");
        ("-v", Set verbose, " Print each step of the algorithm execution.");
        ( "-p",
          Set print_last,
          " Print the list of node joinable in -length steps." );
        ("-allDiff", Set all_diff, " Set the allDifferent constraint.");
        ("-c", Set to_count_paths, " Count the number of paths from the source");
        ( "-pFath",
          Set print_fathers,
          " Print the fathers list, if in verbose mode." );
        ( "-f",
          Set_string file_path,
          " Set the file path to read the input to parse." );
        ("-loop", Set loop, " The algorithm computes paths from 1 to -length");
      ]
  in
  let usage_msg =
    "Here is the list of all the optional commands you are allowed to enter in \
     order to modify the default behaviour of the program"
  in
  Arg.parse speclist print_endline usage_msg;
  run
    {
      all_diff;
      file_path;
      path_length;
      print_fathers;
      verbose;
      to_count_paths;
      src;
      print_last;
      loop;
    }

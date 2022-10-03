(**
  w, h
  c_00 c_10 ... c_w0
  c_01 c_11 ... c_w1
  ...
  c_0h c_1h ... c_wh
  
  n
  a, b = l
  c, d = l
  ...

  where 
  - [w, h] are the width and the height of the matrix
  - c_00 throught c_wh is a { 0, 1 } matrix where each cell c_ij is one if there is an edge from node i to node j, 0 otherwise.
  - the number of edges of the matrix
  - [a, b = l] represents a production of the color function : the edge with extremities (a: int), (b: int) which is represented with a 1 by the cell c_ab in the matrix is associated to [l] the list of colors of it on the form [0,2,3...]: int list separated with comma
*)
let parse (txt : string) =
  let l = String.split_on_char '\n' txt in
  let w, h =
    match Lib.find_all_ints_in_string (List.hd l) with
    | a :: [ b ] -> (a, b)
    | _ -> invalid_arg "Parser : First line of txt should be a couple of ints"
  in
  let m = Array.make_matrix h w 0 in
  let rec fill_matrix l = function
    | x when x = h -> List.tl l
    | h ->
        Lib.find_all_ints_in_string (List.hd l)
        |> List.iteri (fun w e -> m.(h).(w) <- e);
        fill_matrix (List.tl l) (h + 1)
  in
  let l = fill_matrix (List.tl l) 0 in
  let edge_nb = int_of_string (List.hd l) in
  let colors = Hashtbl.create edge_nb in
  List.iter
    (fun e ->
      match Lib.find_all_ints_in_string e with
      | a :: b :: l -> Hashtbl.add colors (a, b) (Graph.S.of_list l)
      | _ -> invalid_arg "Parser : invalid cost function")
    l;
  ({ m; colors } : Graph.t)

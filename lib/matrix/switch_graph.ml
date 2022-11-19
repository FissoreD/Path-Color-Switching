module ColorSet = MySet.Make (Int)

type color_function = (int * int, ColorSet.t) Hashtbl.t
type cell_cnt = ColorSet.t

type graph = {
  m : cell_cnt Adj_mat.matrix;
  mutable mi : cell_cnt Adj_mat.matrix;
}

let print ?(stdout = stdout) { mi; _ } = Adj_mat.print ~stdout mi
let initiate n = Adj_mat.initiate false n ColorSet.empty

let initiate (col_f : color_function) n =
  let update_cnt { m; mi } (i, j) cols =
    m.(i).(j) <- { d = 0.; info = cols };
    mi.(i).(j) <- { d = 0.; info = cols }
  in
  let matrix = { m = initiate n; mi = initiate n } in
  Hashtbl.iter (update_cnt matrix) col_f;
  matrix

let calc_dist fm i j k =
  let c1, c2 = (fm.mi.(i).(k), fm.m.(k).(j)) in
  if c1.d <> infinity && c2.d <> infinity then
    let cont1, cont2 = (c1.info, c2.info) in
    let inters = ColorSet.inter cont1 cont2 in
    if ColorSet.is_empty inters then (c1.d +. 1., cont2) else (c1.d, inters)
  else (infinity, ColorSet.empty)

let make_iteration fm =
  let n = Adj_mat.size fm.m in
  let m' = Adj_mat.initiate false n ColorSet.empty in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      for k = 0 to n - 1 do
        let cell = m'.(i).(j) in
        let d, col = calc_dist fm i j k in
        if d = cell.d then cell.info <- ColorSet.union col cell.info
        else if d < cell.d then (
          cell.d <- d;
          cell.info <- col)
      done
    done
  done;
  fm.mi <- m'

let run ?(f = ignore) (am : graph) n =
  for _ = 1 to n - 1 do
    make_iteration am;
    f am
  done

let read_json jspath =
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_file jspath in
  let tbl_nodes : (int, ColorSet.t) Hashtbl.t = Hashtbl.create 2048 in
  let col_function : color_function = Hashtbl.create 2048 in

  (* Nodes should start with zero value *)
  let minus_one = ( + ) (-1) in

  (* Parse nodes and to tbl_node (node_id, col) *)
  json |> member "nodes" |> to_list
  |> List.iter (fun e ->
         Hashtbl.add tbl_nodes
           (member "id" e |> to_int |> minus_one)
           (member "scales" e |> to_list |> filter_int |> List.map minus_one
          |> ColorSet.of_list));

  json |> member "links" |> to_list
  |> List.iter (fun e ->
         let s = member "source" e |> to_int |> minus_one in
         let t = member "target" e |> to_int |> minus_one in
         Hashtbl.add col_function (s, t) (Hashtbl.find tbl_nodes s));

  initiate col_function (Hashtbl.length tbl_nodes)

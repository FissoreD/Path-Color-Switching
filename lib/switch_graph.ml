module ColorSet = Set.Make (Int)

type color_function = (int * int, ColorSet.t) Hashtbl.t

module MatrixShortestPath = struct
  include Graph.AdjacencyMatrix

  type cell_cnt = ColorSet.t
  type tfw = { m : cell_cnt matrix; mutable mi : cell_cnt matrix }

  let print { mi; _ } = print mi
  let initiate' n = initiate false n ColorSet.empty

  let initiate (col_f : color_function) n =
    let update_cnt { m; mi } (i, j) cols =
      m.(i).(j) <- { d = 0.; info = cols };
      mi.(i).(j) <- { d = 0.; info = cols }
    in
    let matrix = { m = initiate' n; mi = initiate' n } in
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
    let n = size fm.m in
    let m' = initiate' n in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        for k = 0 to n - 1 do
          let cell = m'.(i).(j) in
          let d, col = calc_dist fm i j k in
          if d = cell.d then cell.info <- ColorSet.inter col cell.info
          else if d < cell.d then (
            cell.d <- d;
            cell.info <- col)
        done
      done
    done;
    fm.mi <- m'

  let run ?(f = ignore) (am : tfw) n =
    for _ = 1 to n - 1 do
      make_iteration am;
      f am
    done
end

module FW_Classic = struct
  include Adj_mat

  type tfw = { m : unit matrix; mutable mi : unit matrix }
  (** m is the adjacency matrix, mi the matrix at time i *)

  let initiate' ?(is_sym = true) n = initiate is_sym n ()
  let initiate n = { m = initiate' n; mi = initiate' n }

  let add_neighbors { mi; m; _ } v1 v2 w =
    add_neighbors m v1 v2 w;
    add_neighbors mi v1 v2 w

  let print { mi; _ } = print mi

  let make_iteration { m; mi; _ } k =
    let n = size m in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        mi.(i).(j).d <- min (mi.(i).(k).d +. mi.(k).(j).d) mi.(i).(j).d
      done
    done

  let run ?(f = ignore) (am : tfw) =
    for i = 0 to size am.m - 1 do
      make_iteration am i;
      f am
    done
end

module FW_Generalized = struct
  include FW_Classic

  let initiate' = initiate' ~is_sym:false

  let make_iteration fm =
    let n = size fm.m in
    let m' = initiate' n in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        for k = 0 to n - 1 do
          m'.(i).(j).d <- min (fm.mi.(i).(k).d +. fm.m.(k).(j).d) m'.(i).(j).d
        done
      done
    done;
    fm.mi <- m'

  let run ?(f = ignore) (am : tfw) =
    for _ = 0 to size am.m - 1 do
      make_iteration am;
      f am
    done
end
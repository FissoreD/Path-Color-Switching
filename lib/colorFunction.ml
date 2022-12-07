module ColorSet = MySet.Make (Int)

type colorFunction = {
  is_sym : bool;
  tbl : (int * int, ColorSet.t) Hashtbl.t;
  get_col : int * int -> ColorSet.t;
}

let get tbl (v1, v2) =
  Option.value ~default:ColorSet.empty (Hashtbl.find_opt tbl (v1, v2))

let init ?(is_sym = false) () =
  let tbl = Hashtbl.create 2048 in
  { is_sym; tbl; get_col = get tbl }

let add { tbl; is_sym; _ } v1 v2 col =
  Hashtbl.replace tbl (v1, v2) col;
  if is_sym then Hashtbl.replace tbl (v2, v1) col

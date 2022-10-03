module S = Set.Make (Int)

type t = { m : int array array; colors : (int * int, S.t) Hashtbl.t }
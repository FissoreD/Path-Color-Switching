module ColorSet = MySet.ColorSet

type t2 = { color : ColorSet.t; w : int }

type t = {
  name : int;
  (* children : (int, t) Hashtbl.t; *)
  mutable father : t list;
  content : t2;
}

let canAdd _ _ = true

let compareForUnion a b : Mdd.action =
  match compare a.content.w b.content.w with
  | -1 -> KEEP_S1
  | 0 -> MERGE
  | _ -> KEEP_S2

let mergeAction { name; father; content = { w; color } } b =
  {
    name;
    father = father @ b.father;
    content = { w; color = ColorSet.union color b.content.color };
  }

let compare _a _b = compare _a.name _b.name

let rec print ?(stdout = stdout) { name; father; _ } =
  Printf.fprintf stdout "%d " name;
  Printf.fprintf stdout "[";
  List.iter (print ~stdout) father;
  Printf.fprintf stdout "]"

let content : t2 = { w = 0; color = ColorSet.Full }
let clean = false
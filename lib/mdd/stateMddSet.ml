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

let mergeAction a b =
  (* let tbl = Hashtbl.copy a.children in *)
  (* let tbl = a.children in *)
  (* Hashtbl.iter (fun k v -> Hashtbl.replace tbl k v) b.children; *)
  {
    name = a.name;
    father = a.father @ b.father;
    (* children = tbl; *)
    content =
      {
        w = a.content.w;
        color = ColorSet.union a.content.color b.content.color;
      };
  }

let compare a b = compare a.name b.name

let rec print ?(stdout = stdout) { name; father; _ } =
  Printf.fprintf stdout "%d " name;
  Printf.fprintf stdout "[";
  List.iter (print ~stdout) father;
  Printf.fprintf stdout "]"

let content : t2 = { w = 0; color = ColorSet.Full }
let cmp = compare

module IntSet = MySet.ColorSet
include StateMddSet

let rec canAdd e { name; father; _ } =
  e <> name && List.for_all (canAdd e) father

let make_father_set a =
  let rec aux a acc =
    match a with [] -> acc | h :: _ -> aux h.father (h.name :: acc)
  in
  IntSet.of_list (aux a [])

let compare a b =
  match cmp a.name b.name with
  | 0 -> IntSet.compare (make_father_set a.father) (make_father_set b.father)
  | n -> n

let clean = not clean
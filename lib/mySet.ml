(** Extends set module by adding the Full set  *)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Make (Ord : OrderedType) = struct
  module Set = Set.Make (Ord)
  include Set

  type x = t
  type t = Full | Set of x

  let inter a b =
    match (a, b) with Full, a | a, Full -> a | Set a, Set b -> Set (inter a b)

  let union a b =
    match (a, b) with
    | Full, _ | _, Full -> Full
    | Set a, Set b -> Set (union a b)

  let disjoint a b =
    match (a, b) with
    | Full, _ | _, Full -> false
    | Set a, Set b -> disjoint a b

  let cardinal = function Full -> -1 | Set a -> cardinal a
  let is_empty = function Full -> false | Set a -> is_empty a
  let is_full = ( = ) Full
  let full = Full
  let mem a = function Full -> true | Set s -> mem a s
  let empty = Set empty
  let add e = function Full -> Full | Set s -> Set (add e s)
  let singleton e = Set (singleton e)
  let remove e = function Full -> Full | Set s -> Set (remove e s)
  let filter p = function Full -> Full | Set s -> Set (filter p s)

  let choose = function
    | Full -> invalid_arg "Can't choose in full set"
    | Set s -> choose s

  let diff a b =
    match (a, b) with
    | Full, Set _ -> Full
    | Set _, Full -> empty
    | Set a, Set b -> Set (diff a b)
    | Full, Full -> invalid_arg "Can't make diff between Full sets"

  let compare a b =
    match (a, b) with
    | Full, Full -> 0
    | Full, _ -> 1
    | _, Full -> -1
    | Set a, Set b -> compare a b

  let equal a b =
    match (a, b) with
    | Full, Full -> true
    | Set a, Set b -> equal a b
    | _ -> false

  let subset a b =
    match (a, b) with
    | Full, _ -> true
    | _, Full -> false
    | Set a, Set b -> subset a b

  let elements = function
    | Full -> invalid_arg "Can't return elements of a full list"
    | Set a -> elements a

  let of_list e = Set (of_list e)
  let print f = function Full -> Printf.printf "full" | Set e -> iter f e

  let iter f = function
    | Full -> invalid_arg "Can't loop over infinite set"
    | Set a -> iter f a

  let exists f = function Full -> true | Set a -> exists f a

  let find_opt elt set =
    match set with Full -> Some elt | Set s -> find_opt elt s

  let fold f s accu = match s with Full -> Full | Set s -> fold f s accu
  let find_first_opt f = function Full -> None | Set s -> find_first_opt f s
end

module ColorSet = Make (Int)

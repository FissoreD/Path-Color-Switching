type action = MERGE | REPLACE | IGNORE

module type OrderedType = sig
  type t

  val merge : t -> t -> action
  val compare : t -> t -> int
end

module Make (Ord : OrderedType) = struct
  module MySet = MySet.Make (Ord)
  include MySet
end
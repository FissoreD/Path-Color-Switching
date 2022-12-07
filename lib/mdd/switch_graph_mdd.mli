module type M = sig
  type t2 = StateMddSet.t2 = { color : Mdd.ColorSet.t; w : int }
  type t = StateMddSet.t = { name : int; mutable father : t list; content : t2 }

  val compareForUnion : t -> t -> Mdd.action
  val mergeAction : t -> t -> t
  val print : ?stdout:out_channel -> t -> unit
  val canAdd : int -> t -> bool
  val compare : t -> t -> int
end

module Make : functor (M : M) -> sig
  type col_f = ColorFunction.colorFunction

  type graph = {
    graph : Adj_list.graph;
    root : Mdd.Make(M).S.t;
    last_layer : Mdd.Make(M).mdd_layers;
    update_function : StateMddSet.t -> int -> StateMddSet.t2;
    get_succ : int -> int list;
    col_f : col_f;
  }

  val content : StateMddSet.t2
  val print : ?stdout:out_channel -> ?fathers:bool -> graph -> unit
  val update_function : col_f -> StateMddSet.t -> int -> StateMddSet.t2
  val initiate : ?is_sym:bool -> col_f -> int -> graph
  val make_iteration : graph -> unit
  val count_paths : graph -> int
  val run : ?f:(graph -> unit) -> graph -> int -> unit
  val read_json : ?src:int -> string -> graph
end

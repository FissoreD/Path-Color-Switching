module ColorSet = MySet.ColorSet

type action =
  | MERGE  (** MERGE : we have to remove t1, and add `mergeAction s1 s2`*)
  | KEEP_S1  (** We have to keep the content of s1*)
  | KEEP_S2  (** We have to keep the content of s2*)

module type State = sig
  type t2 = { color : ColorSet.t; w : int }
  type t = { name : int; mutable father : t list; content : t2 }

  val canAdd : int -> t -> bool
  val compareForUnion : t -> t -> action
  val mergeAction : t -> t -> t
  val compare : t -> t -> int
  val print : ?stdout:out_channel -> t -> unit
  val clean : bool
end

module Make (T : State) = struct
  module Set = MySet.Make (T)

  type mdd_layers = Set.t ref

  let add set s2 =
    (* All the fathers of node, will replace the child old_n with new_n *)
    match Set.find_opt s2 set with
    | None -> Set.add s2 set
    | Some s1 -> (
        match T.compareForUnion s1 s2 with
        | MERGE ->
            let new_node = T.mergeAction s1 s2 in
            (* replaceFather new_node s1;
               replaceFather new_node s2; *)
            Set.add new_node (Set.remove s1 set)
        | KEEP_S1 ->
            (* replaceFather s1 s2; *)
            set
        | KEEP_S2 ->
            (* replaceFather s2 s1; *)
            Set.add s2 (Set.remove s1 set))

  (** 
    Takes a list of labels, an update function and a node.  
    For each label, it adds a transition to a new child whose 
    state is computed through the udpate function
  *)
  let add_succ succ_names give_color_and_cost father =
    List.fold_left
      (fun acc successor ->
        if T.canAdd successor father then
          let node : T.t =
            {
              name = successor;
              father = [ father ];
              content = give_color_and_cost successor;
            }
          in
          add acc node
        else acc)
      Set.empty succ_names

  (** take an mdd as a list of layers, and compute a new layer from the last layer of the mdd *)
  let update_layers get_succ col_f (last_layer : mdd_layers) =
    let update_function (col_f : ColorFunction.colorFunction) (n1 : T.t) n2 :
        T.t2 =
      let new_cols = col_f.get_col (n1.name, n2) in
      let inter = ColorSet.inter new_cols n1.content.color in
      if ColorSet.cardinal inter = 0 then
        { w = n1.content.w + 1; color = new_cols }
      else { w = n1.content.w; color = inter }
    in
    let build_new_layer e =
      let new_layer = ref Set.empty in
      Set.iter
        (fun (e : T.t) ->
          let succ = add_succ (get_succ e.name) (update_function col_f e) e in
          new_layer :=
            Set.fold
              (fun (e : Set.elt) (acc : Set.t) -> add acc e)
              succ !new_layer)
        e;
      !new_layer
    in
    last_layer := build_new_layer !last_layer (*!layers*)
end
(**
  An MDD is a functor parametrized by the State type of the MDD. 
  Each state has 
    - a name : the label of the state in the graph. 
    - a mapping (int, Mdd) representing the children of the node
    - a list of fathers (note the node can be merged)
    - a content, to store informations about the state

  The State type should implement 
    - a compare method -> states must by ordered
    - a merge method -> if the compare method returns that two state s1, s2 are the same,
      we should be able to know if
        - we have to merge the content of them := MERGE
        - we have to keep the content of s1 
        - we have to keep the content of s2
    - a mergeAction -> merging the content of the two states
*)

module ColorSet = MySet.ColorSet

type action =
  | MERGE  (** MERGE : we have to remove t1, and add `mergeAction s1 s2`*)
  | KEEP_S1  (** We have to keep the content of s1*)
  | KEEP_S2  (** We have to keep the content of s2*)

module type State = sig
  type t2 = { color : ColorSet.t; w : int }

  type t = {
    name : int;
    children : (int, t) Hashtbl.t;
    mutable father : t list;
    content : t2;
  }

  val compareForUnion : t -> t -> action
  val mergeAction : t -> t -> t
  val compare : t -> t -> int
  val print : ?stdout:out_channel -> t -> unit
end

module Make (T : State) = struct
  module S = MySet.Make (T)

  type mdd_layers = S.t list ref

  let initate name content : mdd_layers =
    let r : S.t =
      S.singleton { children = Hashtbl.create 2048; father = []; name; content }
    in
    ref [ r ]

  let add set s2 =
    (* All the fathers of node, will replace the child old_n with new_n *)
    let replaceFather (new_n : T.t) (old_n : T.t) =
      List.iter
        (fun (f : T.t) -> Hashtbl.replace f.children old_n.name new_n)
        old_n.father
    in
    match S.find_opt s2 set with
    | None -> S.add s2 set
    | Some s1 -> (
        match T.compareForUnion s1 s2 with
        | MERGE ->
            let new_node = T.mergeAction s1 s2 in
            replaceFather new_node s1;
            replaceFather new_node s2;
            S.add new_node (S.remove s1 set)
        | KEEP_S1 ->
            replaceFather s1 s2;
            set
        | KEEP_S2 ->
            replaceFather s2 s1;
            S.add s2 (S.remove s1 set))

  let union = S.fold (fun (e : S.elt) (acc : S.t) -> add acc e)

  (** 
    Takes a list of labels, an update function and a node.  
    For each label, it adds a transition to a new child whose 
    state is computed through the udpate function
  *)
  let add_succ labels update_function father =
    List.fold_left
      (fun acc label ->
        let node : T.t =
          {
            name = label;
            father = [ father ];
            children = Hashtbl.create 2048;
            content = update_function father label;
          }
        in
        Hashtbl.add father.children label node;
        add acc node)
      S.empty labels

  (** take an mdd as a list of layers, and compute a new layer from the last layer of the mdd *)
  let update_layers get_succ update_function (layers : mdd_layers) =
    let last_layer = List.hd !layers in
    let build_new_layer e =
      let new_layer = ref S.empty in
      S.iter
        (fun (e : T.t) ->
          let succ = add_succ (get_succ e.name) update_function e in
          new_layer := union succ !new_layer)
        e;
      !new_layer
    in
    layers := build_new_layer last_layer :: [] (*!layers*)
end

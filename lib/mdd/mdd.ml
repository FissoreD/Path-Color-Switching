type 'a mdd_tree = {
  node : int list;
  state : 'a;
  mutable children : (int, 'a mdd_tree) Hashtbl.t;
  father : 'a mdd_tree;
}
(**
    'node_type is the type of the content of a node   
    'label is the label of a transition between two nodes
*)

type 'a mdd_layers = 'a mdd_tree list list ref

let initate s r_cnt =
  let rec r =
    { node = [ s ]; children = Hashtbl.create 2048; father = r; state = r_cnt }
  in
  ref [ [ r ] ]

(** 
    Takes a list of labels, an update function and a node.  
    For each label, it adds a transition to a new child whose 
    state is computed through the udpate function
*)
let add_succ labels update father =
  List.map
    (fun label ->
      let node =
        {
          node = label :: father.node;
          father;
          children = Hashtbl.create 2048;
          state = update (List.hd father.node) label father.state;
        }
      in
      Hashtbl.add father.children label node;
      node)
    labels

(** take an mdd as a list of layers, and compute a new layer from the last layer of the mdd *)
let update_layers get_succ update_function (layers : 'a mdd_layers) =
  layers :=
    List.(
      map
        (fun n -> add_succ (get_succ (List.hd n.node)) update_function n)
        (hd !layers)
      |> flatten)
    :: !layers

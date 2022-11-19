type 'a mdd_tree = {
  node : int list;
  state : 'a;
  mutable children : (int, 'a mdd_tree) Hashtbl.t;
  mutable father : 'a mdd_tree list;
}
(**
    'node_type is the type of the content of a node   
    'label is the label of a transition between two nodes
*)

type 'a mdd_layers = 'a mdd_tree list list ref

let initate s r_cnt =
  let r =
    { node = [ s ]; children = Hashtbl.create 2048; father = []; state = r_cnt }
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
          father = [ father ];
          children = Hashtbl.create 2048;
          state = update (List.hd father.node) label father.state;
        }
      in
      Hashtbl.add father.children label node;
      node)
    labels

(** take an mdd as a list of layers, and compute a new layer from the last layer of the mdd *)
let update_layers merge get_succ update_function (layers : 'a mdd_layers) =
  let last_layer = List.hd !layers in
  let rec build_new_layer res = function
    | [] -> res
    | hd :: tl ->
        build_new_layer
          (merge res @@ add_succ (get_succ (List.hd hd.node)) update_function hd)
          tl
  in
  layers := build_new_layer [] last_layer :: !layers

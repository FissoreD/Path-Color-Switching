module ColorSet = MySet.Make (Int)

type state_type = { w : int; mutable cols : ColorSet.t }

type graph = {
  graph : Adj_list.graph;
  mdd_layers : state_type Mdd.mdd_layers;
  update_function : int -> int -> state_type -> state_type;
  get_succ : int -> int list;
  col_f : Color_function.color_function;
}

let root_cnt : state_type = { w = 0; cols = ColorSet.full }

let print ?(stdout = stdout) { mdd_layers; _ } =
  List.iter
    (fun ({ node; state; _ } : state_type Mdd.mdd_tree) ->
      Printf.fprintf stdout "{state = %d ; w = %d ; cols = " (List.hd node)
        state.w;
      ColorSet.print (Printf.fprintf stdout "%d ") state.cols;
      Printf.fprintf stdout " }; \n")
    (List.hd !mdd_layers
    |> List.sort (fun (a : state_type Mdd.mdd_tree) b ->
           compare (List.hd a.node) (List.hd b.node)))

let update_function col_f n1 n2 ({ w; cols } : state_type) : state_type =
  let new_cols = Hashtbl.find col_f (n1, n2) in
  let inter = ColorSet.inter new_cols cols in
  if ColorSet.cardinal inter = 0 then { w = w + 1; cols = new_cols }
  else { w; cols = inter }

let initiate ?(is_sym = false) (col_f : Color_function.color_function) s =
  let graph = Adj_list.initiate is_sym in
  Hashtbl.iter (fun (v1, v2) _ -> Adj_list.add_neighbors graph v1 v2) col_f.tbl;
  {
    graph;
    mdd_layers = Mdd.initate s root_cnt;
    get_succ = Adj_list.get_succ graph;
    update_function = update_function col_f.tbl;
    col_f;
  }

module TreeComp = struct
  type t = state_type Mdd.mdd_tree

  let compare (a : t) (b : t) = compare (List.hd a.node) (List.hd b.node)
end

module SetState = struct
  include MySet.Make (TreeComp)

  let add new_elt = function
    | Full -> Full
    | Set set as s -> (
        match find_opt new_elt set with
        | None -> add new_elt s
        | Some (old : TreeComp.t) ->
            if old.state.w < new_elt.state.w then s
            else if old.state.w > new_elt.state.w then
              add new_elt (remove old s)
            else (
              new_elt.state.cols <-
                ColorSet.union new_elt.state.cols old.state.cols;
              new_elt.father <- new_elt.father @ old.father;
              List.iter
                (fun (e : state_type Mdd.mdd_tree) ->
                  Hashtbl.replace e.children (List.hd new_elt.node) new_elt)
                old.father;
              add new_elt (remove old s)))
end

let make_iteration g =
  Mdd.update_layers
    (fun (a : TreeComp.t list) (b : TreeComp.t list) ->
      List.fold_left (Fun.flip SetState.add) (SetState.of_list b) a
      |> SetState.elements)
    g.get_succ g.update_function g.mdd_layers

let rec run ?(f = ignore) g = function
  | 0 -> ()
  | n ->
      make_iteration g;
      f g;
      run ~f g (n - 1)

let read_json ?(src = 0) jspath =
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_file jspath in
  let tbl_nodes : (int, ColorSet.t) Hashtbl.t = Hashtbl.create 2048 in
  let col_function = Color_function.init () in

  (* Nodes should start with zero value *)
  let minus_one = ( + ) (-1) in

  (* Parse nodes and to tbl_node (node_id, col) *)
  json |> member "nodes" |> to_list
  |> List.iter (fun e ->
         Hashtbl.add tbl_nodes
           (member "id" e |> to_int |> minus_one)
           (member "scales" e |> to_list |> filter_int |> List.map minus_one
          |> ColorSet.of_list));

  json |> member "links" |> to_list
  |> List.iter (fun e ->
         let s = member "source" e |> to_int |> minus_one in
         let t = member "target" e |> to_int |> minus_one in
         Color_function.add col_function s t (Hashtbl.find tbl_nodes s));

  initiate col_function src

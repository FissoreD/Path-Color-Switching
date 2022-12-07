module type M = sig
  type t2 = StateMddSet.t2 = { color : MySet.ColorSet.t; w : int }
  type t = StateMddSet.t = { name : int; mutable father : t list; content : t2 }

  val compareForUnion : t -> t -> Mdd.action
  val mergeAction : t -> t -> t
  val print : ?stdout:out_channel -> t -> unit
  val canAdd : int -> t -> bool
  val compare : t -> t -> int
end

module Make (M : M) = struct
  module ColorSet = MySet.ColorSet
  module IntSet = MySet.ColorSet
  module MddState = Mdd.Make (M)

  type col_f = ColorFunction.colorFunction

  type graph = {
    graph : Adj_list.graph;
    root : MddState.S.t;
    last_layer : MddState.mdd_layers;
    update_function : StateMddSet.t -> int -> StateMddSet.t2;
        (** takes a state of the MDD and transition label and compute the content of the next state *)
    get_succ : int -> int list;
    col_f : col_f;
  }

  let content : StateMddSet.t2 = { w = 0; color = ColorSet.full }

  let print ?(stdout = stdout) ?(fathers = false) { last_layer; _ } =
    MddState.S.iter
      (fun ({ name; content; _ } as n) ->
        Printf.fprintf stdout "{state = %d ; w = %d ; cols = " name content.w;
        ColorSet.print (Printf.fprintf stdout "%d ") content.color;
        Printf.fprintf stdout "; p = ";
        if fathers then StateMddSet.print ~stdout n;
        Printf.fprintf stdout " }; \n")
      !last_layer

  let update_function (col_f : col_f) (n1 : StateMddSet.t) n2 : StateMddSet.t2 =
    let new_cols = col_f.get_col (n1.name, n2) in
    let inter = ColorSet.inter new_cols n1.content.color in
    if ColorSet.cardinal inter = 0 then
      { w = n1.content.w + 1; color = new_cols }
    else { w = n1.content.w; color = inter }

  let initiate ?(is_sym = false) (col_f : col_f) name =
    let graph = Adj_list.initiate is_sym in
    Hashtbl.iter
      (fun (v1, v2) _ -> Adj_list.add_neighbors graph v1 v2)
      col_f.tbl;
    {
      graph;
      root = !(MddState.initate name { w = 0; color = ColorSet.Full });
      last_layer = MddState.initate name { w = 0; color = ColorSet.Full };
      get_succ = Adj_list.get_succ graph;
      update_function = update_function col_f;
      col_f;
    }

  let make_iteration g =
    MddState.update_layers g.get_succ g.update_function g.last_layer

  let count_paths { last_layer; _ } =
    let rec aux res (l : StateMddSet.t) =
      if l.father <> [] then res := !res * List.length l.father;
      List.iter (aux res) l.father
    in
    let res = ref 0 in
    MddState.S.iter
      (fun e ->
        let cnt = ref 1 in
        aux cnt e;
        res := !res + !cnt)
      !last_layer;
    !res

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
    let col_function = ColorFunction.init () in

    (* Nodes should start with zero value *)
    (* let minus_one = ( + ) (-1) in *)

    (* Parse nodes and to tbl_node (node_id, col) *)
    json |> member "nodes" |> to_list
    |> List.iter (fun e ->
           Hashtbl.add tbl_nodes
             (member "id" e |> to_int)
             (member "scales" e |> to_list |> filter_int |> ColorSet.of_list));

    json |> member "links" |> to_list
    |> List.iter (fun e ->
           let s = member "source" e |> to_int in
           let t = member "target" e |> to_int in
           ColorFunction.add col_function s t (Hashtbl.find tbl_nodes s));

    initiate col_function src
end

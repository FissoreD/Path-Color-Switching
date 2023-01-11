module ColorSet = MySet.ColorSet
module IntSet = MySet.ColorSet

module Make (M : Mdd.State) = struct
  module MddState = Mdd.Make (M)

  type graph = {
    graph : Adj_list.graph;
    root : MddState.S.t;
    last_layer : MddState.mdd_layers;
    get_succ : int -> int list;
    col_f : ColorFunction.colorFunction;
  }

  let content : M.t2 = { w = 0; color = ColorSet.full }

  let print ?(stdout = stdout) ?(fathers = false) { last_layer; _ } =
    MddState.S.iter
      (fun ({ name; content; _ } as n) ->
        Printf.fprintf stdout "{state = %d ; w = %d ; cols = " name content.w;
        ColorSet.print (Printf.fprintf stdout "%d ") content.color;
        Printf.fprintf stdout "; p = ";
        if fathers then M.print ~stdout n;
        Printf.fprintf stdout " }; \n")
      !last_layer

  let initiate ?(is_sym = false) (col_f : ColorFunction.colorFunction) name =
    let graph = Adj_list.initiate is_sym in
    Hashtbl.iter
      (fun (v1, v2) _ -> Adj_list.add_neighbors graph v1 v2)
      col_f.tbl;
    {
      graph;
      root = MddState.S.singleton { father = []; name; content };
      last_layer = ref (MddState.S.singleton { father = []; name; content });
      get_succ = Adj_list.get_succ graph;
      col_f;
    }

  let make_iteration g = MddState.update_layers g.get_succ g.col_f g.last_layer

  let count_paths { last_layer; _ } =
    let res = ref 0 in
    let rec aux (l : M.t) =
      if l.father = [] then incr res;
      List.iter aux l.father
    in
    MddState.S.iter aux !last_layer;
    !res

  let clean { last_layer; _ } =
    if M.clean then (
      let mins = Hashtbl.create @@ MddState.S.cardinal !last_layer in
      MddState.S.iter
        (fun e ->
          Hashtbl.replace mins e.name
            (min e.content.w
               (Option.value ~default:max_int @@ Hashtbl.find_opt mins e.name)))
        !last_layer;
      last_layer :=
        MddState.S.filter
          (fun (e : M.t) -> e.content.w <= Hashtbl.find mins e.name)
          !last_layer)

  let rec run ?(f = ignore) g = function
    | 0 -> clean g
    | n ->
        make_iteration g;
        f g;
        run ~f g (n - 1)

  let read_json ?(src = 0) jspath =
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_file jspath in
    let tbl_nodes : (int, ColorSet.t) Hashtbl.t = Hashtbl.create 2048 in
    let col_function = ColorFunction.init () in

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

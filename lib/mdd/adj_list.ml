module MySet = MySet.Make (Int)

type graph = { is_sym : bool; neighs : (int, MySet.t ref) Hashtbl.t }
(** the graph is represented on the form of an adjacency list  *)

let initiate is_sym = { is_sym; neighs = Hashtbl.create 2048 }

let add_neighbors { is_sym; neighs } n1 n2 =
  let add_replace x y =
    let n =
      Option.value ~default:(ref MySet.empty) (Hashtbl.find_opt neighs x)
    in
    n := MySet.add y !n;
    Hashtbl.replace neighs x n
  in
  add_replace n1 n2;
  if is_sym then add_replace n2 n1

let size = Hashtbl.length

let get_succ { neighs; _ } v =
  match Hashtbl.find_opt neighs v with
  | None -> []
  | Some a -> MySet.elements !a

let print ?(stdout = stdout) { neighs; _ } =
  Hashtbl.iter
    (fun key value ->
      Printf.printf "%d : " key;
      MySet.iter (fun x -> Printf.fprintf stdout "%6d " x) !value;
      Printf.fprintf stdout "\n")
    neighs;
  Printf.printf "\n"

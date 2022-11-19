type 'a cell_cnt = { mutable d : float; mutable info : 'a }
type 'a matrix = 'a cell_cnt array array

let initiate is_sym n info : 'a matrix =
  let am =
    Array.init n (fun _ -> Array.init n (fun _ -> { d = infinity; info }))
  in
  if is_sym then
    for i = 0 to n - 1 do
      am.(i).(i).d <- 0.
    done;
  am

let add_neighbors am n1 n2 weight = am.(n1).(n2).d <- weight
let size = Array.length

let print ?(stdout = stdout) (am : 'a matrix) =
  Array.iter
    (fun e ->
      Array.iter (fun x -> Printf.fprintf stdout "%6.2f " x.d) e;
      Printf.fprintf stdout "\n")
    am;
  print_endline ""

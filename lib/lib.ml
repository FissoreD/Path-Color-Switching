let openF y s = "bin/y20" ^ y ^ "/input/day" ^ s ^ ".txt" |> open_in
let closeF = close_in

(** 
  Takes an year, a day and a function to parse each line of the the input file. Returns the List of line parsed
*)
let read_file year day func =
  let inp = openF year day in
  let rec aux () =
    try
      let l = input_line inp in
      func l :: aux ()
    with End_of_file ->
      closeF inp;
      []
  in
  aux ()

exception Invalid_input
exception Switch_not_implemented

module List = struct
  include List

  let rec remove elt : 'a t -> 'a t = function
    | [] -> []
    | hd :: tl when hd = elt -> remove elt tl
    | hd :: tl -> hd :: remove elt tl

  let count elt =
    let rec count acc = function
      | [] -> acc
      | hd :: tl when elt = hd -> count (acc + 1) tl
      | _ :: tl -> count acc tl
    in
    count 0

  let get a b = (Fun.flip nth) a b

  let rec set index elt = function
    | [] -> []
    | _ :: tl when index = 0 -> elt :: tl
    | hd :: tl -> hd :: set (index - 1) elt tl

  let split_on_index i =
    let rec aux acc i = function
      | [] -> (acc, [])
      | hd :: tl when i = 0 -> (List.rev (hd :: acc), tl)
      | hd :: tl -> aux (hd :: acc) (i - 1) tl
    in
    aux [] (i - 1)

  let print f l = iter (fun elt -> f elt |> Printf.printf "%s ") l

  let transpose_matrix mat =
    let height = hd mat |> length in
    let width = length mat in
    init height (fun i -> init width (fun j -> get j mat |> get i))

  let sub start len =
    let rec aux res start len = function
      | _ when len <= 0 -> res |> rev
      | [] -> res
      | _ :: tl when start > 0 -> aux res (start - 1) len tl
      | hd :: tl -> aux (hd :: res) start (len - 1) tl
    in
    aux [] start len

  let rec index_of elt = function
    | [] -> raise Invalid_input
    | hd :: _ when hd = elt -> 0
    | _ :: tl -> 1 + index_of elt tl

  let remove_last l = sub 0 (List.length l - 1) l

  let rec findi pos f = function
    | [] -> raise Not_found
    | hd :: _ when f hd -> pos
    | _ :: tl -> findi pos f tl

  let findi f = findi 0 f
  let last l = get (length l - 1) l

  let append a b =
    let rec aux res = function [] -> res | hd :: tl -> aux (hd :: res) tl in
    aux b (List.rev a)

  let map f l =
    let rec aux acc = function [] -> acc | hd :: tl -> aux (f hd :: acc) tl in
    aux [] (List.rev l)
end

module Array = struct
  include Array

  let findi f arr =
    let res = ref (-1) in
    (try
       for i = 0 to Array.length arr - 1 do
         if f arr.(i) then (
           res := i;
           raise Exit)
       done
     with Exit -> ());
    if !res = -1 then raise Not_found else !res

  let findi_all f arr =
    let res = ref [] in
    (try
       for i = 0 to Array.length arr - 1 do
         if f arr.(i) then (
           res := i :: !res;
           raise Exit)
       done
     with Exit -> ());
    !res

  let to_matrix width arr =
    Array.init
      (Array.length arr / width)
      (fun i -> Array.init width (fun j -> arr.((i * width) + j)))
end

let is_odd n = n land 1 = 1
let is_even n = not (is_odd n)
let halve n = n lsr 1
let double n = n lsl 1

let is_int s =
  try
    int_of_string s |> ignore;
    true
  with _ -> false

let string_2_char_list s =
  let rec aux acc = function
    | -1 -> acc
    | n -> aux (String.get s n :: acc) (n - 1)
  in
  aux [] (String.length s - 1)

let bool_to_int n = if n then 1 else 0
let string_of_char = String.make 1

let char_to_letter_pos ?(first_letter = 'a') c =
  Char.code c - Char.code first_letter

let letter_pos_to_char ?(first_letter = 'a') c =
  Char.chr (c + Char.code first_letter)

let char_list_2_string l =
  let buf = Buffer.create (List.length l) in
  let rec aux = function
    | [] -> ()
    | hd :: tl ->
        Buffer.add_char buf hd;
        aux tl
  in
  aux l;
  Buffer.contents buf

let count_substring str sub =
  let sub_len = String.length sub in
  let len_diff = String.length str - sub_len
  and reg = Re.Str.regexp_string sub in
  let rec aux i n =
    if i > len_diff then n
    else
      try
        let pos = Re.Str.search_forward reg str i in
        aux (pos + sub_len) (succ n)
      with Not_found -> n
  in
  aux 0 0

let in_bouond_inclusive (a, b) elt = a <= elt && elt <= b

let repeat_string n s =
  let rec aux acc = function 0 -> acc | n -> aux (acc ^ s) (n - 1) in
  aux "" n

let string_start_padding str_len pad_char str =
  String.make (max 0 (str_len - String.length str)) pad_char ^ str

let bin_to_int s = "0b" ^ s |> int_of_string

let int_to_bin = function
  | 0 -> "0"
  | n ->
      let rec aux = function
        | 0 -> ""
        | n -> aux (halve n) ^ if is_even n then "0" else "1"
      in
      aux n

let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u

let lcm m n =
  match (m, n) with 0, _ | _, 0 -> 0 | m, n -> abs (m * n) / gcd m n

let print_bool x = Printf.printf "%b" x
let remove_comma = Re.Str.global_replace (Re.Str.regexp ",") ""
let md5_to_hex s = Digest.string s |> Digest.to_hex

let rec is_power_of_two n =
  if n < 1 then invalid_arg "Integer must be positive"
  else if n = 1 then true
  else if is_even n then is_power_of_two @@ halve n
  else false

(** returns p in N such that pow(2, p) <= n and p is max integer satisfying this constraint *)
let two_power_floor =
  let rec aux acc = function
    | x when x < 1 -> invalid_arg "Should be greater than 0"
    | 1 -> acc
    | n -> aux (acc + 1) (halve n)
  in
  aux 0

let timeit f =
  let t = Sys.time () in
  let res = f () in
  print_endline @@ Printf.sprintf "\nTime is : %f\n" (Sys.time () -. t);
  res

let open_submodule f1 f2 = function
  | 1 -> f1
  | 2 -> f2
  | _ -> invalid_arg "3rd param should be 1 or 2"

let print_int i = print_endline @@ Printf.sprintf "%d" i

let rec permutations l =
  let n = List.length l in
  if n = 1 then [ l ]
  else
    let rec sub acc e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then acc @ t else sub (h :: acc) e t
    in
    let rec aux k =
      let e = List.nth l k in
      let subperms = permutations (sub [] e l) in
      let t = List.map (fun a -> e :: a) subperms in
      if k < n - 1 then List.rev_append t (aux (k + 1)) else t
    in
    aux 0

let split_on_space l =
  Re.Str.(split (regexp "[\t\r ]") l) |> List.filter (( <> ) "")

let find_all_ints_in_string l =
  let open Re.Str in
  let len = String.length l in
  let rec find_ints pos acc =
    if pos >= len then []
    else
      try
        let next = search_forward (regexp {|-?[0-9]+|}) l pos in
        let found = matched_group 0 l in
        int_of_string found :: find_ints (next + String.length found) (acc + 1)
      with Not_found -> []
  in
  find_ints 0 0

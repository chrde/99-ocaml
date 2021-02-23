(* 1 *)
let rec last l =
  match l with
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
;;

let%test _ = last [] = None
let%test _ = last [ 1 ] = Some 1
let%test _ = last [ 1; 2; 3; 4 ] = Some 4

(* 2 *)
let rec two_last l =
  match l with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> two_last t
;;

let%test _ = two_last [] = None
let%test _ = two_last [ 1 ] = None
let%test _ = two_last [ 1; 2 ] = Some (1, 2)
let%test _ = two_last [ 1; 2; 3 ] = Some (2, 3)

(* 3 *)
let rec at pos l =
  match l with
  | [] -> None
  | x :: y -> if pos = 1 then Some x else at (pos - 1) y
;;

let%test _ = at 0 [] = None
let%test _ = at 3 [] = None
let%test _ = at 3 [ 1 ] = None
let%test _ = at 3 [ 1; 2; 3 ] = Some 3
let%test _ = at 3 [ 1; 2; 3; 4; 5 ] = Some 3

(* 4 *)
let rec length l =
  match l with
  | [] -> 0
  | [ _ ] -> 1
  | _ :: y -> 1 + length y
;;

let length_tail l =
  let rec length n l =
    match l with
    | [] -> n
    | _ :: t -> length (n + 1) t
  in
  length 0 l
;;

let%test _ = length [] = 0
let%test _ = length [ 1 ] = 1
let%test _ = length [ 1; 2 ] = 2
let%test _ = length_tail [] = 0
let%test _ = length_tail [ 1 ] = 1
let%test _ = length_tail [ 1; 2 ] = 2

(* 5 *)
let rec rev l =
  match l with
  | [] -> []
  | x :: y -> rev y @ [ x ]
;;

let rev_tail l =
  let rec rev acc l =
    match l with
    | [] -> acc
    | x :: y -> rev (x :: acc) y
  in
  rev [] l
;;

let%test _ = rev [] = []
let%test _ = rev [ "1" ] = [ "1" ]
let%test _ = rev [ 1; 2 ] = [ 2; 1 ]
let%test _ = rev [ 1; 2; 3 ] = [ 3; 2; 1 ]
let%test _ = rev_tail [] = []
let%test _ = rev_tail [ "1" ] = [ "1" ]
let%test _ = rev_tail [ 1; 2 ] = [ 2; 1 ]
let%test _ = rev_tail [ 1; 2; 3 ] = [ 3; 2; 1 ]

(* 6 *)
let is_palindrome l =
  let rev_l = List.rev l in
  rev_l = l
;;

let%test _ = is_palindrome [] = true
let%test _ = is_palindrome [ 1 ] = true
let%test _ = is_palindrome [ 1; 2 ] = false
let%test _ = is_palindrome [ 1; 2; 1 ] = true
let%test _ = is_palindrome [ 1; 2; 3; 2; 1 ] = true
let%test _ = is_palindrome [ 1; 2; 3; 3; 2; 1 ] = true

(* 7 *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten l =
  match l with
  | [] -> []
  | One a :: y -> a :: flatten y
  | Many l :: y -> flatten l @ flatten y
;;

let%test _ = flatten [] = []
let%test _ = flatten [ One "a" ] = [ "a" ]
let%test _ = flatten [ Many [ One "a" ] ] = [ "a" ]
let%test _ = flatten [ Many [ One "a"; One "b" ] ] = [ "a"; "b" ]
let%test _ = flatten [ Many [ One "a"; Many [ One "b" ] ] ] = [ "a"; "b" ]

let%test _ =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]
;;

(* 8 *)
let rec compress l =
  match l with
  | x :: (y :: _ as z) -> if x = y then compress z else x :: compress z
  | l -> l
;;

let%test _ = compress [] = []
let%test _ = compress [ 1 ] = [ 1 ]
let%test _ = compress [ 1; 1; 1 ] = [ 1 ]
let%test _ = compress [ 1; 2; 1 ] = [ 1; 2; 1 ]

let%test _ =
  compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;

(* 9 *)
let pack l =
  let rec pack_rec total acc l =
    match l with
    | [] -> []
    | [ x ] -> (x :: acc) :: total
    | x :: (y :: _ as z) ->
      if x = y then pack_rec total (x :: acc) z else pack_rec ((x :: acc) :: total) [] z
  in
  List.rev (pack_rec [] [] l)
;;

let%test _ = pack [] = []
let%test _ = pack [ 1 ] = [ [ 1 ] ]
let%test _ = pack [ 1; 2 ] = [ [ 1 ]; [ 2 ] ]
let%test _ = pack [ 1; 1 ] = [ [ 1; 1 ] ]
let%test _ = pack [ 1; 1; 1 ] = [ [ 1; 1; 1 ] ]

let%test _ =
  pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

(* 10 *)
let encode l = List.map (fun l -> List.length l, List.hd l) (pack l)

let encode1 l =
  let rec encode_rec total count l =
    match l with
    | [] -> []
    | [ x ] -> (count + 1, x) :: total
    | x :: (y :: _ as z) ->
      if x = y
      then encode_rec total (count + 1) z
      else encode_rec ((count + 1, x) :: total) 0 z
  in
  List.rev (encode_rec [] 0 l)
;;

let%test _ = encode [] = []
let%test _ = encode [ 1 ] = [ 1, 1 ]
let%test _ = encode [ 1; 1 ] = [ 2, 1 ]
let%test _ = encode [ 1; 2 ] = [ 1, 1; 1, 2 ]

let%test _ =
  encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
;;

let%test _ = encode [] = encode1 []
let%test _ = encode [ 1 ] = encode1 [ 1 ]
let%test _ = encode [ 1; 1 ] = encode1 [ 1; 1 ]
let%test _ = encode [ 1; 2 ] = encode1 [ 1; 2 ]

let%test _ =
  encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = encode1 [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;

(* 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_m l = List.map (fun (x, y) -> if x = 1 then One y else Many (x, y)) (encode l)

let%test _ = encode_m [] = []
let%test _ = encode_m [ 1 ] = [ One 1 ]
let%test _ = encode_m [ 1; 1 ] = [ Many (2, 1) ]
let%test _ = encode_m [ 1; 2 ] = [ One 1; One 2 ]

let%test _ =
  encode_m [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;

(* 12 *)
let decode l =
  let rec decode_rec acc l =
    match l with
    | [] -> acc
    | [ One x ] -> x :: acc
    | [ Many (x, y) ] ->
      if x > 1
      then decode_rec (y :: acc) [ Many (x - 1, y) ]
      else decode_rec acc [ One y ]
    | x :: y -> decode_rec acc [ x ] @ decode_rec [] y
  in
  decode_rec [] l
;;

let decode_1 l =
  let rec repeat acc count x =
    if count = 0 then acc else repeat (x :: acc) (count - 1) x
  in
  let rec decode_rec acc l =
    match l with
    | [] -> acc
    | One x :: y -> decode_rec (x :: acc) y
    | Many (n, x) :: y -> decode_rec (repeat acc n x) y
  in
  decode_rec [] (List.rev l)
;;

let%test _ = decode [] = []
let%test _ = decode [ One 1 ] = [ 1 ]
let%test _ = decode [ Many (2, 1) ] = [ 1; 1 ]

let%test _ =
  decode [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
  = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;

let%test _ = decode_1 [] = decode []
let%test _ = decode_1 [ One 1 ] = decode [ One 1 ]
let%test _ = decode_1 [ Many (2, 1) ] = decode [ Many (2, 1) ]

let%test _ =
  decode_1
    [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
  = decode
      [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;

(* 13 *)
let encode_direct l =
  let rle count x = if count = 0 then One x else Many (count + 1, x) in
  let rec encode_rec acc count l =
    match l with
    | [] -> acc
    | [ x ] -> rle count x :: acc
    | x :: (y :: _ as z) ->
      if x = y then encode_rec acc (count + 1) z else encode_rec (rle count x :: acc) 0 z
  in
  encode_rec [] 0 (List.rev l)
;;

let%test _ = encode_m [] = encode_direct []
let%test _ = encode_m [ 1 ] = encode_direct [ 1 ]
let%test _ = encode_m [ 1; 1 ] = encode_direct [ 1; 1 ]
let%test _ = encode_m [ 1; 2 ] = encode_direct [ 1; 2 ]

let%test _ =
  encode_m [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = encode_direct [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;

(* 14 *)
let rec duplicate l =
  match l with
  | [] -> []
  | [ x ] -> [ x; x ]
  | x :: y -> x :: x :: duplicate y
;;

let%test _ = duplicate [] = []
let%test _ = duplicate [ 1 ] = [ 1; 1 ]
let%test _ = duplicate [ 1; 2 ] = [ 1; 1; 2; 2 ]

let%test _ =
  duplicate [ "a"; "b"; "c"; "c"; "d" ]
  = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
;;

(* 15 *)
let replicate l count =
  let rec rep acc x count = if count = 0 then acc else rep (x :: acc) x (count - 1) in
  let rec replicate_rec acc l =
    match l with
    | [] -> acc
    | x :: y -> replicate_rec (rep acc x count) y
  in
  replicate_rec [] (List.rev l)
;;

let replicate1 l count =
  let rec rep count acc x = if count = 0 then acc else rep (count - 1) (x :: acc) x in
  List.fold_left (rep count) [] (List.rev l)
;;

let%test _ = replicate1 [] 0 = replicate [] 0
let%test _ = replicate1 [ 1 ] 0 = replicate [ 1 ] 0
let%test _ = replicate1 [ 1; 2 ] 0 = replicate [ 1; 2 ] 0
let%test _ = replicate1 [ 1; 2 ] 1 = replicate [ 1; 2 ] 1
let%test _ = replicate1 [ 1; 2 ] 2 = replicate [ 1; 2 ] 2
let%test _ = replicate1 [ "a"; "b"; "c" ] 3 = replicate [ "a"; "b"; "c" ] 3
let%test _ = replicate [] 0 = []
let%test _ = replicate [ 1 ] 0 = []
let%test _ = replicate [ 1; 2 ] 0 = []
let%test _ = replicate [ 1; 2 ] 1 = [ 1; 2 ]
let%test _ = replicate [ 1; 2 ] 2 = [ 1; 1; 2; 2 ]

let%test _ =
  replicate [ "a"; "b"; "c" ] 3 = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
;;

(* 16 *)
let drop l count =
  let rec drop_rec l c =
    match l, c with
    | [], _ -> []
    | _ :: y, 1 -> drop_rec y count
    | x :: y, n -> x :: drop_rec y (n - 1)
  in
  drop_rec l count
;;

let%test _ = drop [] 0 = []
let%test _ = drop [] 1 = []
let%test _ = drop [ 1 ] 0 = [ 1 ]
let%test _ = drop [ 1 ] 1 = []
let%test _ = drop [ 1; 2 ] 1 = []
let%test _ = drop [ 1; 2 ] 3 = [ 1; 2 ]
let%test _ = drop [ 1; 2; 3; 4; 5; 6 ] 3 = [ 1; 2; 4; 5 ]

let%test _ =
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
;;

(* 17 *)
let split l count =
  let rec split_rec first second count =
    match second, count with
    | [], _ -> List.rev first, second
    | _, 0 -> List.rev first, second
    | x :: y, count -> split_rec (x :: first) y (count - 1)
  in
  split_rec [] l count
;;

let%test _ = split [] 0 = ([], [])
let%test _ = split [] 1 = ([], [])
let%test _ = split [ 1 ] 0 = ([], [ 1 ])
let%test _ = split [ 1; 2 ] 0 = ([], [ 1; 2 ])
let%test _ = split [ 1; 2 ] 1 = ([ 1 ], [ 2 ])
let%test _ = split [ 1 ] 1 = ([ 1 ], [])
let%test _ = split [ 1 ] 2 = ([ 1 ], [])
let%test _ = split [ 1; 2 ] 2 = ([ 1; 2 ], [])
let%test _ = split [ 1; 2; 3 ] 1 = ([ 1 ], [ 2; 3 ])
let%test _ = split [ 1; 2; 3 ] 2 = ([ 1; 2 ], [ 3 ])

let%test _ =
  split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
;;

(* 18 *)
let slice l x y =
  let rec take acc count l =
    match l, count with
    | [], _ -> acc
    | _, 0 -> acc
    | x :: y, count -> take (x :: acc) (count - 1) y
  in
  let rec drop count l =
    match l, count with
    | [], _ -> []
    | x, 0 -> x
    | _ :: y, count -> drop (count - 1) y
  in
  List.rev (take [] (y - x + 1) (drop x l))
;;

let%test _ = slice [] 0 0 = []
let%test _ = slice [ 1 ] 0 0 = [ 1 ]
let%test _ = slice [ 1 ] 0 1 = [ 1 ]
let%test _ = slice [ 1; 2 ] 1 2 = [ 2 ]
let%test _ = slice [ 1; 2; 3 ] 1 3 = [ 2; 3 ]
let%test _ = slice [ 1; 2; 3; 4; 5; 6 ] 2 4 = [ 3; 4; 5 ]

let%test _ =
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
  = [ "c"; "d"; "e"; "f"; "g" ]
;;

(* 19 *)
let rotate l count =
  let count = if count < 0 then List.length l + count else count in
  let rec split_at x acc l =
    match l, x with
    | [], _ -> acc, []
    | l, 0 -> acc, l
    | h :: t, x -> split_at (x - 1) (h :: acc) t
  in
  let left, right = split_at count [] l in
  right @ List.rev left
;;

let%test _ = rotate [] 0 = []
let%test _ = rotate [ 1 ] 0 = [ 1 ]
let%test _ = rotate [ 1; 2 ] 0 = [ 1; 2 ]
let%test _ = rotate [ 1; 2 ] 1 = [ 2; 1 ]
let%test _ = rotate [ 1; 2; 3 ] 1 = [ 2; 3; 1 ]
let%test _ = rotate [ 1; 2; 3 ] 2 = [ 3; 1; 2 ]
let%test _ = rotate [ 1; 2; 3 ] 3 = [ 1; 2; 3 ]

let%test _ =
  rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
  = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
;;

let%test _ =
  rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-2)
  = [ "g"; "h"; "a"; "b"; "c"; "d"; "e"; "f" ]
;;

(* 20 *)
let remove_at x l =
  let rec remove_at_rec acc l x =
    match l, x with
    | [], _ -> List.rev acc
    | _ :: y, 0 -> List.rev acc @ y
    | x :: y, c -> remove_at_rec (x :: acc) y (c - 1)
  in
  remove_at_rec [] l x
;;

let%test _ = remove_at 0 [] = []
let%test _ = remove_at 1 [] = []
let%test _ = remove_at 0 [ 1 ] = []
let%test _ = remove_at 1 [ 1 ] = [ 1 ]
let%test _ = remove_at 1 [ 1; 2 ] = [ 1 ]
let%test _ = remove_at 1 [ 1; 2; 3 ] = [ 1; 3 ]
let%test _ = remove_at 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "c"; "d" ]

(* 21 *)
let rec insert_at item pos l =
  match l, pos with
  | [], _ -> [ item ]
  | x, 0 -> item :: x
  | x :: y, count -> x :: insert_at item (count - 1) y
;;

let%test _ = insert_at "1" 1 [] = [ "1" ]
let%test _ = insert_at "1" 2 [] = [ "1" ]
let%test _ = insert_at "2" 0 [ "1" ] = [ "2"; "1" ]
let%test _ = insert_at "2" 1 [ "1" ] = [ "1"; "2" ]
let%test _ = insert_at "3" 1 [ "1"; "2" ] = [ "1"; "3"; "2" ]
let%test _ = insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "alfa"; "b"; "c"; "d" ]
let%test _ = insert_at "alfa" 3 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "alfa"; "d" ]
let%test _ = insert_at "alfa" 4 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d"; "alfa" ]

(* 22 *)
let rec range x y =
  if x = y then [ x ] else if x < y then x :: range (x + 1) y else x :: range (x - 1) y
;;

let%test _ = range 0 0 = [ 0 ]
let%test _ = range 0 1 = [ 0; 1 ]
let%test _ = range 1 0 = [ 1; 0 ]
let%test _ = range 4 2 = [ 4; 3; 2 ]
let%test _ = range 2 4 = [ 2; 3; 4 ]
let%test _ = range 4 9 = [ 4; 5; 6; 7; 8; 9 ]
let%test _ = range 9 4 = [ 9; 8; 7; 6; 5; 4 ]

(* 23 *)
let rand_select l n =
  (* for test *)
  Random.init 4;
  let rec select acc n l =
    match l, n with
    | [], _ -> raise Not_found
    | x :: y, 0 -> x, acc @ y
    | x :: y, n -> select (x :: acc) (n - 1) y
  in
  let rec rand_select_rec acc n l =
    if n = 0
    then acc
    else (
      let choice, l = select [] (Random.int (List.length l)) l in
      rand_select_rec (choice :: acc) (n - 1) l)
  in
  rand_select_rec [] n l
;;

let%test _ = rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3 = [ "d"; "c"; "g" ]

(* 24 *)
let lotto_select n max = rand_select (range 1 max) n

let%test _ = lotto_select 6 49 = [ 23; 32; 43; 30; 6; 10 ]

(* 25 *)
let permutation l = rand_select l (List.length l)

let%test _ = permutation [ 1; 2; 3; 4 ] = [ 4; 1; 2; 3 ]

(* 26 *)
let rec extract n l =
  if n = 0
  then [ [] ]
  else (
    match l with
    | [] -> []
    | x :: y ->
      let left = List.map (List.cons x) (extract (n - 1) y) in
      let right = extract n y in
      left @ right)
;;

let%test _ = extract 0 [] = [ [] ]
let%test _ = extract 0 [ "a" ] = [ [] ]
let%test _ = extract 1 [] = []
let%test _ = extract 1 [ "a" ] = [ [ "a" ] ]
let%test _ = extract 1 [ "a"; "b"; "c" ] = [ [ "a" ]; [ "b" ]; [ "c" ] ]

let%test _ =
  extract 2 [ "a"; "b"; "c"; "d" ]
  = [ [ "a"; "b" ]; [ "a"; "c" ]; [ "a"; "d" ]; [ "b"; "c" ]; [ "b"; "d" ]; [ "c"; "d" ] ]
;;

(* 27 *)
let group_diff l1 l2 =
  let not_exists l x = Option.is_none (List.find_opt (( = ) x) l) in
  List.filter (fun x -> not_exists l1 x) l2
;;

let%test _ = group_diff [ 1 ] [ 2 ] = [ 2 ]
let%test _ = group_diff [ 1; 2 ] [ 2; 4 ] = [ 4 ]
let%test _ = group_diff [ 1; 2; 3 ] [ 2; 4 ] = [ 4 ]
let%test _ = group_diff [ 1; 2; 3 ] [ 2; 4; 5 ] = [ 4; 5 ]

type 'a partial_solution =
  { groups : 'a list list
  ; remaining_list : 'a list
  ; sizes : int list
  }

let new_partials p c =
  List.map
    (fun (group, remaining_list) ->
      { groups = group :: p.groups; remaining_list; sizes = List.tl p.sizes })
    c
;;

let candidates n remaining_list =
  let add_remaining group = group, group_diff group remaining_list in
  List.map add_remaining (extract n remaining_list)
;;

let rec group_rec p =
  match p.sizes with
  | [] -> [ p ]
  | x :: _ ->
    let c = candidates x p.remaining_list in
    let new_p = new_partials p c in
    List.concat_map group_rec new_p
;;

let group list sizes =
  let p = { groups = []; remaining_list = list; sizes } in
  List.map (fun p -> List.rev p.groups) (group_rec p)
;;

let%test _ =
  group [ "a"; "b"; "c"; "d" ] [ 2; 1 ]
  = [ [ [ "a"; "b" ]; [ "c" ] ]
    ; [ [ "a"; "b" ]; [ "d" ] ]
    ; [ [ "a"; "c" ]; [ "b" ] ]
    ; [ [ "a"; "c" ]; [ "d" ] ]
    ; [ [ "a"; "d" ]; [ "b" ] ]
    ; [ [ "a"; "d" ]; [ "c" ] ]
    ; [ [ "b"; "c" ]; [ "a" ] ]
    ; [ [ "b"; "c" ]; [ "d" ] ]
    ; [ [ "b"; "d" ]; [ "a" ] ]
    ; [ [ "b"; "d" ]; [ "c" ] ]
    ; [ [ "c"; "d" ]; [ "a" ] ]
    ; [ [ "c"; "d" ]; [ "b" ] ]
    ]
;;

(* 28 *)
let length_sort l =
  let length_compare x y = compare (List.length x) (List.length y) in
  List.fast_sort length_compare l
;;

let%test _ =
  length_sort
    [ [ "a"; "b"; "c" ]
    ; [ "d"; "e" ]
    ; [ "f"; "g"; "h" ]
    ; [ "d"; "e" ]
    ; [ "i"; "j"; "k"; "l" ]
    ; [ "m"; "n" ]
    ; [ "o" ]
    ]
  = [ [ "o" ]
    ; [ "d"; "e" ]
    ; [ "d"; "e" ]
    ; [ "m"; "n" ]
    ; [ "a"; "b"; "c" ]
    ; [ "f"; "g"; "h" ]
    ; [ "i"; "j"; "k"; "l" ]
    ]
;;

let frequency_sort l =
  let rec frequency_sort_rec acc cur l =
    match l with
    | [] -> cur :: acc
    | [ x ] -> (x :: cur) :: acc
    | x :: y :: z ->
      if List.length x = List.length y
      then frequency_sort_rec acc (x :: cur) (y :: z)
      else frequency_sort_rec ((x :: cur) :: acc) [] (y :: z)
  in
  length_sort (List.rev l) |> frequency_sort_rec [] [] |> length_sort |> List.concat
;;

let%test _ =
  frequency_sort
    [ [ "a"; "b"; "c" ]
    ; [ "d"; "e" ]
    ; [ "f"; "g"; "h" ]
    ; [ "d"; "e" ]
    ; [ "i"; "j"; "k"; "l" ]
    ; [ "m"; "n" ]
    ; [ "o" ]
    ]
  = [ [ "i"; "j"; "k"; "l" ]
    ; [ "o" ]
    ; [ "a"; "b"; "c" ]
    ; [ "f"; "g"; "h" ]
    ; [ "d"; "e" ]
    ; [ "d"; "e" ]
    ; [ "m"; "n" ]
    ]
;;

;;
frequency_sort [ [ "d"; "e" ]; [ "d"; "e" ]; [ "m"; "n" ] ]

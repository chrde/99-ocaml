type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let example_tree =
  Node
    ( 'a'
    , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
    , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) )
;;

(* 55 *)
let add_trees_with left right acc =
  let add_right_tree acc l =
    List.fold_left (fun a r -> Node ('x', l, r) :: a) acc right
  in
  List.fold_left add_right_tree acc left
;;

let rec cbal_tree n =
  if n = 0
  then [ Empty ]
  else if n mod 2 = 1
  then (
    let t = cbal_tree (n / 2) in
    add_trees_with t t [])
  else (
    let left = cbal_tree ((n / 2) - 1) in
    let right = cbal_tree (n / 2) in
    add_trees_with left right (add_trees_with right left []))
;;

;;
add_trees_with (cbal_tree 2) (cbal_tree 1) []

let%test _ = List.length (cbal_tree 1) = 1
let%test _ = List.length (cbal_tree 40) = 524288

(* 56 *)
let is_symmetric t =
  let rec is_symmetric left right =
    match left, right with
    | Empty, Empty -> true
    | Node (_, l1, r1), Node (_, l2, r2) -> is_symmetric l1 r2 && is_symmetric l2 r1
    | _ -> false
  in
  match t with
  | Empty -> true
  | Node (_, l, r) -> is_symmetric l r
;;

(* 57 *)
let construct n =
  let rec construct tree n =
    match tree with
    | Empty -> Node (n, Empty, Empty)
    | Node (m, l, r) ->
      if m > n
      then Node (m, construct l n, r)
      else if m < n
      then Node (m, l, construct r n)
      else tree
  in
  List.fold_left construct Empty n
;;

let%test _ = is_symmetric (construct [ 5; 3; 18; 1; 4; 12; 21 ]) = true
let%test _ = is_symmetric (construct [ 3; 2; 5; 7; 4 ]) = false

(* 58 *)
let sym_cbal_trees n = List.filter is_symmetric (cbal_tree n)

let%test _ = List.length (sym_cbal_trees 57) = 256

(* 59 *)
let rec hbal_tree n =
  if n = 0
  then [ Empty ]
  else if n = 1
  then [ Node ('x', Empty, Empty) ]
  else (
    let left = hbal_tree (n - 1) in
    let right = hbal_tree (n - 2) in
    add_trees_with left right [] |> add_trees_with left left |> add_trees_with right left)
;;

let%test _ = List.length (hbal_tree 3) = 15

(* 60 *)
let min_nodes h =
  let rec iter left h0 h1 = if left == 0 then h0 else iter (left - 1) h1 (h0 + h1) in
  iter h 0 1
;;

(* 61 *)
let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r
;;

let%test _ = count_leaves Empty = 0
let%test _ = count_leaves example_tree = 3

let leaves t =
  let rec iter t acc =
    match t with
    | Empty -> acc
    | Node (x, Empty, Empty) -> x :: acc
    | Node (_, l, r) -> iter l (iter r acc)
  in
  iter t []
;;

let%test _ = leaves Empty = []
let%test _ = leaves example_tree = [ 'd'; 'e'; 'g' ]

(* 62 *)
let internals t =
  let rec iter t acc =
    match t with
    | Empty -> acc
    | Node (_, Empty, Empty) -> acc
    | Node (x, l, r) -> iter l (x :: iter r acc)
  in
  iter t []
;;

let%test _ = internals Empty = []
let%test _ = internals (Node ('a', Empty, Empty)) = []
let%test _ = internals example_tree = [ 'b'; 'a'; 'c'; 'f' ]

let at_level t lvl =
  let rec iter t level acc =
    match t with
    | Empty -> acc
    | Node (x, l, r) ->
      if level = lvl then x :: acc else iter l (level + 1) (iter r (level + 1) acc)
  in
  iter t 1 []
;;

let%test _ = at_level example_tree 2 = [ 'b'; 'c' ]
let%test _ = at_level example_tree 5 = []

(* 63 *)
let childs idx = 2 * idx, (2 * idx) + 1

let%test _ = childs 1 = (2, 3)
let%test _ = childs 2 = (4, 5)
let%test _ = childs 3 = (6, 7)

let complete_binary_tree l =
  let rec subtree n t =
    let left, right = childs n in
    match List.nth_opt t (n - 1) with
    | None -> Empty
    | Some x -> Node (x, subtree left t, subtree right t)
  in
  subtree 1 l
;;

let%test _ =
  complete_binary_tree [ 1; 2; 3; 4; 5; 6 ]
  = Node
      ( 1
      , Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty))
      , Node (3, Node (6, Empty, Empty), Empty) )
;;

(* 64 *)
let layout_binary_tree1 t =
  let rec iter t x y =
    match t with
    | Empty -> Empty, x
    | Node (v, l, r) ->
      let new_l, l_x = iter l x (y + 1) in
      let new_r, r_x = iter r (l_x + 1) (y + 1) in
      Node ((v, l_x, y), new_l, new_r), r_x
  in
  iter t 1 1
;;

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node
    ( 'n'
    , Node
        ( 'k'
        , Node ('c', leaf 'a', Node ('h', Node ('g', leaf 'e', Empty), Empty))
        , leaf 'm' )
    , Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty) )
;;

let expected =
  Node
    ( ('n', 8, 1)
    , Node
        ( ('k', 6, 2)
        , Node
            ( ('c', 2, 3)
            , Node (('a', 1, 4), Empty, Empty)
            , Node
                ( ('h', 5, 4)
                , Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty)
                , Empty ) )
        , Node (('m', 7, 3), Empty, Empty) )
    , Node
        ( ('u', 12, 2)
        , Node
            ( ('p', 9, 3)
            , Empty
            , Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty) )
        , Empty ) )
;;

let%test _ = layout_binary_tree1 example_layout_tree = (expected, 13)

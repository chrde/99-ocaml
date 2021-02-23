type 'a leftist =
  | Leaf
  | Node of 'a leftist * 'a * 'a leftist * int

let singleton k = Node (Leaf, k, Leaf, 1)

let rank l =
  match l with
  | Leaf -> 0
  | Node (_, _, _, r) -> r
;;

let rec merge t1 t2 =
  match t1, t2 with
  | Leaf, t | t, Leaf -> t
  | Node (l1, k1, r1, _), Node (_, k2, _, _) ->
    if k1 > k2
    then merge t2 t1
    else (
      let merged = merge r1 t2 in
      let rank_left = rank l1
      and rank_right = rank merged in
      if rank_left >= rank_right
      then Node (l1, k1, merged, rank_right + 1)
      else Node (merged, k1, l1, rank_left + 1))
;;

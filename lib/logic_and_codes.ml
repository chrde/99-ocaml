type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

(* 46 & 47 *)
let table2 a b expr =
  let rec eval a val_a b val_b = function
    | Var x -> if x = a then val_a else val_b
    | Not e -> not (eval a val_a b val_b e)
    | And (e1, e2) -> eval a val_a b val_b e1 && eval a val_a b val_b e2
    | Or (e1, e2) -> eval a val_a b val_b e1 || eval a val_a b val_b e2
  in
  List.concat
    (List.map (fun a -> List.map (fun b -> a, b) [ true; false ]) [ true; false ])
  |> List.map (fun (a', b') -> a', b', eval a a' b b' expr)
;;

let%test _ =
  table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
  = [ true, true, true; true, false, true; false, true, false; false, false, false ]
;;

(* 48 *)
let table vars expr =
  let rec eval expr env =
    match expr with
    | Var x -> List.assoc x env
    | Not e -> not (eval e env)
    | And (e1, e2) -> eval e1 env && eval e2 env
    | Or (e1, e2) -> eval e1 env || eval e2 env
  in
  let rec combinations cur acc = function
    | [] -> List.rev (List.rev cur :: acc)
    | x :: y ->
      combinations ((x, true) :: cur) acc y @ combinations ((x, false) :: cur) acc y
  in
  combinations [] [] vars |> List.map (fun env -> env, eval expr env)
;;

let%test _ =
  let a = Var "a"
  and b = Var "b"
  and c = Var "c" in
  table [ "a"; "b"; "c" ] (Or (And (a, Or (b, c)), Or (And (a, b), And (a, c))))
  = [ [ "a", true; "b", true; "c", true ], true
    ; [ "a", true; "b", true; "c", false ], true
    ; [ "a", true; "b", false; "c", true ], true
    ; [ "a", true; "b", false; "c", false ], false
    ; [ "a", false; "b", true; "c", true ], false
    ; [ "a", false; "b", true; "c", false ], false
    ; [ "a", false; "b", false; "c", true ], false
    ; [ "a", false; "b", false; "c", false ], false
    ]
;;

(* 49 *)
let gray m =
  let rec gray_rec acc n =
    if m = n
    then acc
    else (
      let (left, right) =
        List.map ((^) "0") acc , List.map ((^) "1") (List.rev acc)
      in
      gray_rec (left @ right) (n + 1))
  in
  gray_rec [ "0"; "1" ] 1
;;

let%test _ = gray 1 = ["0";"1"]
let%test _ = gray 2 = ["00"; "01"; "11"; "10"]
let%test _ = gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]


(* TODO 50 *)

;;
open Lib

let pprint fmt a = Format.fprintf fmt "%c" a

let pprint_tree x = Binary_trees.show_binary_tree pprint x

let () = 
  let t = Binary_trees.cbal_tree 1 in
  List.map pprint_tree t
  |> String.concat "\n"
  |> print_endline


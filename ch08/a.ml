
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree


let rec add x = function
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (y, left, right) as t ->
      match compare x y with
      | 0 -> t
      | -1 -> Node (y, add x left, right)
      | 1 -> Node (x, left, right)
      | _ -> failwith "can't occur"


(*
add succ (add succ Leaf) は例外を投げる．Haskell の型システムなら弾ける
*)
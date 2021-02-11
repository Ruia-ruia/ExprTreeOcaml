type oper = Add | Sub
type tree =
        | Leaf of int
        | Tree of oper * tree * tree;;

let rec eval e =
        match e with
        | Tree (Add, left, right)
                -> (eval left + eval right)
        | Tree (Sub, left, right)
                -> (eval left - eval right)
        | Leaf v -> v

let x = Tree (Add, Leaf 3, Leaf 2)
let m = Tree (Add, Leaf 3, Leaf 2)
let n = Tree (Add, x, m)
let () = print_endline (
        string_of_int (eval n)
)

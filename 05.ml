(* Reverse a list. *)

let rev lst =
  let rec iter acc = function
    | [] -> acc
    | hd :: tl -> iter (hd :: acc) tl
  in
    iter [] lst

let () = assert(rev [] = [])
let () = assert(rev ["a" ; "b" ; "c"] = ["c"; "b"; "a"])
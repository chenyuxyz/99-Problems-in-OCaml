(* Find the number of elements of a list. *)

let length l =
  let rec iter acc = function
    | [] -> acc
    | _ :: tl -> iter (acc + 1) tl
  in
    iter 0 l

let _ = assert (length [ "a" ; "b" ; "c"] = 3)
let _ = assert (length [] = 0)
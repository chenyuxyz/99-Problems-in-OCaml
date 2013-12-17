(* Duplicate the elements of a list. *)

let duplicate lst =
  let rec iter acc = function
    | [] -> acc
    | hd :: tl -> iter (hd :: hd :: acc) tl
  in
    List.rev (iter [] lst)

let () =
  assert (duplicate ["a";"b";"c";"c";"d"]
    = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])
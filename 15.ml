(* Replicate the elements of a list a given number of times. *)

let replicate lst n =
  let rec gen x acc n = if n = 0 then acc else gen x (x :: acc) (n - 1) in
  let rec iter acc = function
    | [] -> acc
    | hd :: tl -> iter ((gen hd [] n) @ acc) tl
  in
    List.rev (iter [] lst)

let () = 
  assert (replicate ["a";"b";"c"] 3
    = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])
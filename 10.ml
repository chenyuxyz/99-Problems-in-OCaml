(* Run-length encoding of a list. *)

let encode lst =
  let rec iter acc = function
    | (None, []) -> acc
    | (Some cur, []) -> cur :: acc
    | (None, hd :: tl) -> iter acc ((Some (1, hd)), tl)
    | (Some ((n, c) as cur), (hd :: tl as l)) ->
        if hd = c then iter acc ((Some (n + 1, c)), tl)
                  else iter (cur :: acc) (None, l)
  in
    List.rev (iter [] (None, lst)) 

let _ =
  assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])
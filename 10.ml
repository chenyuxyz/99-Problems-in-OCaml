(* Run-length encoding of a list. *)

let encode lst =
  let rec iter acc cnt = function
    | [] -> []
    | [x] -> (cnt + 1, x) :: acc
    | h1 :: (h2 :: _ as tl) ->
        if h1 = h2
        then iter acc (cnt + 1) tl
        else iter ((cnt + 1, h1) :: acc) 0 tl
  in
    List.rev (iter [] 0 lst) 

let () =
  assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])
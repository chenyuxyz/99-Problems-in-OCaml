(* Pack consecutive duplicates of list elements into sublists. *)

let pack lst =
  let rec iter acc cur = function
    | [] -> cur :: acc
    | [x] -> (x :: cur) :: acc
    | h1 :: (h2 :: _ as tl) ->
        if h1 = h2 then iter acc (h1 :: cur) tl
                   else iter ((h1 ::cur) :: acc) [] tl
  in
    List.rev (iter [] [] lst)

let _ = assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
                = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
                   ["e"; "e"; "e"; "e"]])
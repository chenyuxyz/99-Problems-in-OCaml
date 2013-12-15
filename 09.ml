(* Pack consecutive duplicates of list elements into sublists. *)

let pack lst =
  let rec iter acc cur lst = match (cur, lst) with
    | (_, []) -> cur :: acc
    | ([], hd :: tl) -> iter acc [hd] tl
    | (hc :: _, hd :: tl) -> if hc = hd then iter acc (hd :: cur) tl
                                        else iter (cur :: acc) [hd] tl
  in
    List.rev (iter [] [] lst)

let _ = assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
                = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
                   ["e"; "e"; "e"; "e"]])
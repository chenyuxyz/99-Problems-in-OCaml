(* Eliminate consecutive duplicates of list elements. *)

let compress lst =
  let rec iter acc = function
    | [] -> acc
    | [x] -> x :: acc
    | h1 :: h2 :: tl -> if h1 = h2 then iter acc (h2 :: tl)
                                   else iter (h1 :: acc) (h2 :: tl)
  in
    List.rev (iter [] lst)

let () = assert(compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
               = ["a"; "b"; "c"; "a"; "d"; "e"])
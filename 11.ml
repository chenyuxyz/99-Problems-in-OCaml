(* Modified run-length encoding. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode lst =
  let wrap cnt name =
    if cnt = 1
    then One name
    else Many (cnt, name)
  in
  let rec iter acc cnt = function
    | [] -> []
    | [x] -> wrap (cnt + 1) x :: acc
    | h1 :: (h2 :: _ as tl) ->
        if h1 = h2
        then iter acc (cnt + 1) tl
        else iter (wrap (cnt + 1) h1 :: acc) 0 tl
  in
    List.rev (iter [] 0 lst) 

let () = assert
  (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
     Many (4, "e")])
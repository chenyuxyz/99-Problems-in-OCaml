let rec at k = function
    [] -> None
  | hd :: tl -> if k = 1 then Some hd else at (k - 1) tl

let _ = assert (at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c")
let _ = assert (at 3 [ "a" ] = None)
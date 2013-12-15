let rec last_two = function
    [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: tl -> last_two tl

let _ = assert (last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d"))
let _ = assert (last_two [ "a" ] = None)
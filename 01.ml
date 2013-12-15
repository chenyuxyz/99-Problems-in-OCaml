let rec last = function
    [] -> None
  | [ans] -> Some ans
  | _ :: tl -> last tl


let _ = assert (last [ "a" ; "b" ; "c" ; "d" ] = Some "d")
let _ = assert (last [] = None)
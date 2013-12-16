(* Find the last element of a list. *)

let rec last = function
  | [] -> None
  | [ans] -> Some ans
  | _ :: tl -> last tl


let () = assert (last [ "a" ; "b" ; "c" ; "d" ] = Some "d")
let () = assert (last [] = None)
(* Find the last but one (last and penultimate) elements of a list. *)

let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: tl -> last_two tl

let () = assert (last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d"))
let () = assert (last_two [ "a" ] = None)
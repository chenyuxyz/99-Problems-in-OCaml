(* Find out whether a list is a palindrome. *)

let is_palindrome lst =
  lst = List.rev lst

let () = assert(is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true)
let () = assert(not (is_palindrome [ "a" ; "b" ]) = true)
(* Find out whether a list is a palindrome. *)

let is_palindrome lst =
  lst = List.rev lst

let _ = assert(is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true)
let _ = assert(not (is_palindrome [ "a" ; "b" ]) = true)
let is_palindrome lst =
  let rev lst =
    let rec iter acc = function
        [] -> acc
      | hd :: tl -> iter (hd :: acc) tl
    in
      iter [] lst
  in
    lst = rev lst

let _ = assert(is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true)
let _ = assert(not (is_palindrome [ "a" ; "b" ]) = true)
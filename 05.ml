let rev lst =
  let rec iter acc = function
      [] -> acc
    | hd :: tl -> iter (hd :: acc) tl
  in
    iter [] lst

let _ = assert(rev [] = [])
let _ = assert(rev ["a" ; "b" ; "c"] = ["c"; "b"; "a"])
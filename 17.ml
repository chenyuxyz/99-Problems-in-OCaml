(* Split a list into two parts; the length of the first part is given. *)

let split lst n =
  let rec take acc n = function
    | [] -> (List.rev acc, [])
    | hd :: tl as rest ->
        if n = 0
        then (List.rev acc, rest)
        else take (hd :: acc) (n - 1) tl
  in
    take [] n lst


let () = assert (
  split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 
    = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
)

let () = assert (
  split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], [])
)
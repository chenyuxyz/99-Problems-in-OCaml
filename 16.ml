(* Drop every N'th element from a list. *)

let drop lst n =
  let rec iter acc n' = function
    | [] -> acc
    | hd :: tl ->
        if n' = n
        then iter acc 1 tl
        else iter (hd :: acc) (n' + 1) tl
  in
    List.rev (iter [] 1 lst)

let () =
  assert (
    drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
      = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
  )
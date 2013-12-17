(* Extract a slice from a list. *)

let slice lst h t =
  let rec iter acc n = function
    | _ :: tl when n < h -> iter acc (n + 1) tl
    | hd :: tl when n <= t -> iter (hd :: acc) (n + 1) tl
    | _ -> acc
  in
    List.rev (iter [] 0 lst)

let () = assert (
  slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 =
  ["c"; "d"; "e"; "f"; "g"]
)
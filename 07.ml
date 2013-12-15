(* Flatten a nested list structure. *)

type 'a node = One of 'a | Many of 'a node list

let flatten lst =
  let rec extract acc = function
    | [] -> acc
    | One hd :: tl -> extract (hd :: acc) tl
    | Many hd :: tl -> extract (extract acc hd) tl
  in
    List.rev (extract [] lst)

let _ = assert(flatten [One "a"; Many[One "b"; Many[One "c"; One "d"]; One "e"]] 
                = ["a"; "b"; "c"; "d"; "e"])
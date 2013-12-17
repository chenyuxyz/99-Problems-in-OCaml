(* Decode a run-length encoded list. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let decode lst =
  let rec gen x acc n = if n = 0 then acc else gen x (x :: acc) (n - 1) in
  let rec iter acc = function
    | [] -> acc
    | One x :: tl -> iter (x :: acc) tl
    | Many (n, x) :: tl -> iter ((gen x [] n) @ acc) tl
  in
    List.rev (iter [] lst)

let () =
  assert(decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d";
                 Many (4,"e")]
    = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
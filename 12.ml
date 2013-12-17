(* Decode a run-length encoded list. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let decode lst =
  let rec iter acc = function
    | [] -> acc
    | One x :: tl -> iter (x :: acc) tl
    | Many (n, x) :: tl ->
        if n > 1
        then iter (x :: acc) (Many (n - 1, x) :: tl)
        else iter (x :: acc) tl
  in
    List.rev (iter [] lst)

let () =
  assert(decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d";
                 Many (4,"e")]
    = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
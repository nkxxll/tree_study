(* that is a little proof of concept with a linked list *)
type 'a node = { value : 'a; next : 'a node option }

let traverse head =
  let rec aux = function
    | Some node ->
        print_endline (string_of_int node.value);
        aux node.next
    | None -> print_endline "finished"
  in
  aux head

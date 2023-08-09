open! Base

let rec _printy list =
  match list with
  | [] -> ()
  | h :: t ->
      Stdio.print_endline (Int.to_string h);
      _printy t

let () = Stdio.print_endline "hello see the tests"

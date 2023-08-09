open! Base

type binNode = { value : int; right : binNode option; left : binNode option }

let rec _print_list_nodes list =
  match list with
  | [] -> ()
  | h :: t ->
      Stdio.print_endline (Int.to_string h.value);
      _print_list_nodes t

let left_traverse head =
  let rec aux acc = function
    | None -> acc
    | Some node ->
        let list = acc @ [ node.value ] in
        let left_list = aux list node.left in
        aux left_list node.right
  in
  aux [] head

let in_order_traverse head =
  let rec aux acc = function
    | None -> acc
    | Some node ->
        let left_list = aux acc node.left in
        let list = left_list @ [ node.value ] in
        aux list node.right
  in
  aux [] head

let tree_balanced head =
  let rec dfs = function
    | None -> (true, 0)
    | Some node -> (
        let left = dfs node.left in
        let right = dfs node.right in
        match (left, right) with
        | (true, a), (true, b) when Int.abs (a - b) <= 1 ->
            (true, 1 + Int.max a b)
        | (_, a), (_, b) -> (false, 1 + Int.max a b))
  in
  dfs head

let match_or_minus integer = match integer with None -> -1 | Some a -> a

let balance_tree head =
  (* a tree balancing algorithm in O(n) space *)
  let nodes = in_order_traverse head in
  let rec aux start endnode =
    match start - endnode with
    | 0 ->
        let node =
          {
            value = match_or_minus (List.nth nodes start);
            left = None;
            right = None;
          }
        in
        (node, [ node ])
    | _ -> (
        let middle = start + ((endnode - start) / 2) in
        match middle with
        | value when value = start -> (
            let right = (None, []) in
            let left = aux (middle + 1) endnode in
            let middle_node =
              {
                value = match_or_minus (List.nth nodes middle);
                left = Some (match left with a, _ -> a);
                right = None;
              }
            in
            ( middle_node,
              (match left with _, a -> a)
              @ [ middle_node ]
              @ match right with _, a -> a ))
        | other -> (
            let right = aux start (other - 1) in
            let left = aux (other + 1) endnode in
            let middle_node =
              {
                value = match_or_minus (List.nth nodes other);
                left = Some (match left with a, _ -> a);
                right = Some (match right with a, _ -> a);
              }
            in
            ( middle_node,
              (match left with _, a -> a)
              @ [ middle_node ]
              @ match right with _, a -> a )))
  in
  aux 0 (List.length nodes - 1)

let%test_unit "tree_left" =
  (*
  1
  | \
  2  3
  |\ |\
  4 56 7
  *)
  let g = { value = 7; left = None; right = None } in
  let f = { value = 6; left = None; right = None } in
  let e = { value = 5; left = None; right = None } in
  let d = { value = 4; left = None; right = None } in
  let c = { value = 3; left = Some f; right = Some g } in
  let b = { value = 2; left = Some d; right = Some e } in
  let head = { value = 1; left = Some b; right = Some c } in
  let array = [ 1; 2; 4; 5; 3; 6; 7 ] in
  let traverse = left_traverse (Some head) in
  [%test_eq: int list] array traverse

let%test_unit "in_order_traverse_tree" =
  (*
  1
  | \
  2  3
  |\ |\
  4 56 7
  *)
  let g = { value = 7; left = None; right = None } in
  let f = { value = 6; left = None; right = None } in
  let e = { value = 5; left = None; right = None } in
  let d = { value = 4; left = None; right = None } in
  let c = { value = 3; left = Some f; right = Some g } in
  let b = { value = 2; left = Some d; right = Some e } in
  let head = { value = 1; left = Some b; right = Some c } in
  (* middle is the most upper node
     middle of the next two badges is the second row
     the rest is the last row *)
  let array = [ 4; 2; 5; 1; 6; 3; 7 ] in
  let traverse = in_order_traverse (Some head) in
  [%test_eq: int list] array traverse

let%test_unit "tree_balance_true" =
  (*
  1
  | \
  2  3
  |\ |\
  4 56 7
  *)
  let g = { value = 7; left = None; right = None } in
  let f = { value = 6; left = None; right = None } in
  let e = { value = 5; left = None; right = None } in
  let d = { value = 4; left = None; right = None } in
  let c = { value = 3; left = Some f; right = Some g } in
  let b = { value = 2; left = Some d; right = Some e } in
  let head = { value = 1; left = Some b; right = Some c } in
  [%test_eq: bool * int] (tree_balanced (Some head)) (true, 3)

let%test_unit "tree_balance_false" =
  (*
  1
  | \
  2  3
  |\ |\
  4 56 7
       |\
       N 8
         |\
         9 N
  *)
  let i = { value = 9; left = None; right = None } in
  let h = { value = 8; left = Some i; right = None } in
  let g = { value = 7; left = None; right = Some h } in
  let f = { value = 6; left = None; right = None } in
  let e = { value = 5; left = None; right = None } in
  let d = { value = 4; left = None; right = None } in
  let c = { value = 3; left = Some f; right = Some g } in
  let b = { value = 2; left = Some d; right = Some e } in
  let head = { value = 1; left = Some b; right = Some c } in
  [%test_eq: bool * int] (tree_balanced (Some head)) (false, 5)

let%test_unit "balance_tree" =
  let i = { value = 9; left = None; right = None } in
  let h = { value = 8; left = Some i; right = None } in
  let g = { value = 7; left = Some h; right = None } in
  let f = { value = 6; left = Some g; right = None } in
  let e = { value = 5; left = Some f; right = None } in
  let d = { value = 4; left = Some e; right = None } in
  let c = { value = 3; left = Some d; right = None } in
  let b = { value = 2; left = Some c; right = None } in
  let head = { value = 1; left = Some b; right = None } in
  let balanced_head, _ = balance_tree (Some head) in
  let rec printlist list =
    match list with
    | [] -> ()
    | h :: t ->
        Stdio.print_endline (Int.to_string h);
        printlist t
  in
  printlist (left_traverse (Some balanced_head));
  [%test_eq: bool * int] (tree_balanced (Some balanced_head)) (true, 4)

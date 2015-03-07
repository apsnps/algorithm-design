type 'a node = Node of 'a * 'a node list

type 'a graph = 'a node list

(* requires: the node list of each node is sorted in ascending order *)
let breadth_first_search s t g : bool =
  let n = List.length g in
  let explored_nodes = Array.make n false in
  let rec mark_as_explored = function
    | [] -> ()
    | (Node (v,_))::t ->
        explored_nodes.(v-1) <- true;
        mark_as_explored t
  in
  let rec add_unseen_nodes queue lst =
      match lst with
      | [] -> queue
      | h::t ->
          let (Node (v,n)) = h in
          if explored_nodes.(v-1) then add_unseen_nodes queue t
          else add_unseen_nodes (Queue.enqueue queue h) t
  in
  let rec bfs nodes_to_check = 
    not (Queue.is_empty nodes_to_check) && (
      let (h,nodes_to_check') = Queue.dequeue nodes_to_check in
      let (Node (val_h,nodes_h), Node (val_t,_)) = (h,t) in
      (print_int val_h; print_newline (); val_h = val_t) || (
        explored_nodes.(val_h-1) <- true;
        let nodes_to_check'' = add_unseen_nodes nodes_to_check' nodes_h in
        mark_as_explored nodes_h;
        bfs nodes_to_check'')
    )
  in
  bfs (Queue.enqueue Queue.empty s)

let () = 
  let rec n1 : int node = Node (1, [n2; n3])

  and n2 : int node = Node (2, [n1; n3; n4; n5])

  and n3 : int node = Node (3, [n1; n2; n5; n7; n8])

  and n4 : int node = Node (4, [n2; n5])

  and n5 : int node = Node (5, [n2; n3; n4; n6])

  and n6 : int node = Node (6, [n5])

  and n7 : int node = Node (7, [n3; n8])

  and n8 : int node = Node (8, [n3; n7])

  and n9 : int node = Node (9, [n10])

  and n10 : int node = Node (10, [n9])

  and n11 : int node = Node (11, [n12])

  and n12 : int node = Node (12, [n11; n13])

  and n13 : int node = Node (13, [n12]) in

  let g1 : int graph = [n1;n2;n3;n4;n5;n6;n7;n8;n9;n10;n11;n12;n13] in

  if breadth_first_search n1 n7 g1 then
    print_string "found\n"
  else
    print_string "not found\n"

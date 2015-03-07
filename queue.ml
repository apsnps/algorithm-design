(* a queue is implemented as two lists:
 *   the first list is the front the queue,
 *   the second list is the back of queue reversed.
 * Representation invariant: front is empty => back is empty. *)
type 'a t = 'a list * 'a list

exception EmptyQueue

let empty = ([],[])

let is_empty (front,_) =
  front = []

let enqueue (front,back) element =
  match front with
  | [] -> (element::[], [])
  | _ -> (front, element::back)

let dequeue (front,back) =
  match front with
  | [] -> raise EmptyQueue
  | [h] -> (h, (List.rev back, []))
  | h::t -> (h, (t,back))

let peek (front,back) =
  match front with
  | h::t -> h
  | [] -> raise EmptyQueue

let append_tail_recursive = function
  | l, []
  | [], l -> l
  | l1, l2 -> List.rev_append (List.rev l1) (List.rev l2)

let to_list = append_tail_recursive

let append (f1,b1) (f2,b2) =
  let q1 = append_tail_recursive (b1,f1) in
  let q2 = append_tail_recursive (f2,b2) in
  (List.rev_append q1 q2, [])

let fold f acc q =
  let lst = append_tail_recursive q in
  List.fold_left f acc lst

let map f (front,back) = 
  let lst = append_tail_recursive (back,front) in
  (List.map f lst, [])

let rec iter f q =
  if is_empty q then ()
  else begin
    let (h,t) = dequeue q in
    f h;
    iter f t
  end

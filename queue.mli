type 'a t

exception EmptyQueue

val empty : 'a t

val is_empty : 'a t -> bool

val enqueue : 'a t -> 'a -> 'a t

val dequeue : 'a t -> 'a * 'a t

val peek : 'a t -> 'a

val to_list : 'a t -> 'a list

val append : 'a t -> 'a t -> 'a t

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val map : ('a -> 'b) -> 'a t -> 'b t

val iter : ('a -> unit) -> 'a t -> unit

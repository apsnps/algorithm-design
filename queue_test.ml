open Queue
open Assertions

let q = empty
let q = enqueue q 1
let q = enqueue q 2
let q = enqueue q 3
let q = enqueue q 4
let q = enqueue q 5

let (x1,q2) = dequeue q
TEST_UNIT "q1" = assert_true (x1 = 1)

let (x2,q3) = dequeue q2
TEST_UNIT "q2" = assert_true (x2 = 2)

let (x3,q4) = dequeue q3
TEST_UNIT "q3" = assert_true (x3 = 3)

let (x4,q5) = dequeue q4
TEST_UNIT "q4" = assert_true (x4 = 4)

let (x5,q0) = dequeue q5
TEST_UNIT "q5" = assert_true (x5 = 5)

TEST_UNIT "q0" = assert_true (is_empty q0)
TEST_UNIT "q0" = assert_raises (Some EmptyQueue) dequeue q0

let q1 = empty
let q1 = enqueue q1 1
let q1 = enqueue q1 1
let q1 = enqueue q1 2
let q1 = enqueue q1 3
let (_,q1) = dequeue q1
let q1 = enqueue q1 4
let q1 = enqueue q1 5
let q1 = enqueue q1 6
let q2 = empty
let q2 = enqueue q2 7
let q2 = enqueue q2 7
let q2 = enqueue q2 8
let q2 = enqueue q2 9
let (_,q2) = dequeue q2
let q2 = enqueue q2 10
let q2 = enqueue q2 11
let q2 = enqueue q2 12
let qa = append q1 q2

let sum = fold (+) 0 qa
TEST_UNIT "fold" = assert_true(sum = (12*13 / 2))

let _ =
  iter (fun x -> print_int x; print_char ' ') qa;
  print_newline ()

let qb = map (fun x -> x+10) qa
let _ =
  iter (fun x -> print_int x; print_char ' ') qb;
  print_newline ()
let lst = to_list qb
TEST_UNIT "map" = assert_true(lst = [11;12;13;14;15;16;17;18;19;20;21;22])

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 1)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 2)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 3)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 4)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 5)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 6)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 7)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 8)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 9)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 10)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 11)

let (x,qa) = dequeue qa
TEST_UNIT "qa" = assert_true (x = 12)

let () = Pa_ounit_lib.Runtime.summarize()

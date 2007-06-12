type t = int

let mark_counter = ref 0

let fresh () =
  incr mark_counter;
  !mark_counter

let equal m1 m2 = 
  m1 = m2

let none = 
  -1

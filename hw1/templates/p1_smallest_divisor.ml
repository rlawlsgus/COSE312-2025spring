exception NotImplemented;;

let rec sd = fun n i ->
  if i * i > n then n
  else if n mod i = 0 then i
  else sd n (i + 1);;

let smallest_divisor: int -> int
  = fun n -> sd n 2;;
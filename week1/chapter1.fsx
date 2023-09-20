
// Exercise 1.4
let rec f = function
  | 0 -> 0
  | n -> n + f (n - 1)

// Exercise 1.6
let rec sum = function
  | (m, 0) -> m
  | (m, n) -> (m + n) + sum (m, n - 1)

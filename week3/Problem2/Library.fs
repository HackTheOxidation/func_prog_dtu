namespace Problem2

  // Exercise 1
  (*
          splitAt -1 [1;2;3] => ([], [1;2;3])
          splitAt 3 [1;2;3;4;5] => ([1;2;3], [4;5])
          splitAt 4 [1;2;3] => ([1;2;3], [])
  *)

  // Exercise 2
  (*
          val splitAt: int -> 'a list -> 'a list * a' list

          We know that the first argument is definitely an int
          and the second argument is a list. The return type
          is a pair of lists, which must be of the same type as
          the second argument, since it is constructed only from
          the value of that argument without any conversions/casts.
          There are no function applications that depend on the
          contained type of the second argument, so it must have
          a generic type: 'a list.
  *)

  // Exercise 3
  (* *)

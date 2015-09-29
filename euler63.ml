open Core.Std

let digits_in_number num =
  Float.of_int (Int.of_float (log10 num) + 1)

let n_power_n_digit_integer_count n =
  let rec find n x acc =
    if x > 0. then
      if digits_in_number (x**n) = n then
        find n (x -. 1.) (acc + 1)
      else
        acc
    else
      acc
  in
  find (Float.of_int n) 9. 0

let solve =
  let rec find n acc =
    let count = n_power_n_digit_integer_count n in
    if count > 0 then
      find (n + 1) (acc + count)
    else
      acc
  in
  find 1 0

let () =
  printf "%d\n%!" solve

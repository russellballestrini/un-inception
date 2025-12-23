let rec fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let () =
  for i = 0 to 10 do
    Printf.printf "fib(%d) = %d\n" i (fib i)
  done

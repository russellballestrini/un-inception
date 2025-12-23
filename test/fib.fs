let rec fib n =
    if n <= 1 then n
    else fib(n-1) + fib(n-2)

for i in 0 .. 10 do
    printfn "fib(%d) = %d" i (fib i)

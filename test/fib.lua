function fib(n)
    if n <= 1 then
        return n
    end
    return fib(n - 1) + fib(n - 2)
end

for i = 0, 10 do
    print(string.format("fib(%d) = %d", i, fib(i)))
end

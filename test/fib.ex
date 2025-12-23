defmodule Fib do
  def fib(n) when n <= 1, do: n
  def fib(n), do: fib(n-1) + fib(n-2)

  def main do
    Enum.each(0..10, fn i ->
      IO.puts("fib(#{i}) = #{fib(i)}")
    end)
  end
end

Fib.main()

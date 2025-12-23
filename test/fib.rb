#!/usr/bin/env ruby
def fib(n)
  return n if n <= 1
  fib(n-1) + fib(n-2)
end

(0..10).each do |i|
  puts "fib(#{i}) = #{fib(i)}"
end

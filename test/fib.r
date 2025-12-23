#!/usr/bin/Rscript
fib <- function(n) {
  if (n <= 1) return(n)
  return(fib(n-1) + fib(n-2))
}

for (i in 0:10) {
  cat(sprintf("fib(%d) = %d\n", i, fib(i)))
}

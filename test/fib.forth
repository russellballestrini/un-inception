: fib ( n -- fib )
  dup 2 < if exit then
  dup 1 - recurse
  swap 2 - recurse
  + ;

: print-fib ( n -- )
  dup ." fib(" 0 .r ." ) = " fib 0 .r cr ;

: main
  11 0 do
    i print-fib
  loop ;

main
bye

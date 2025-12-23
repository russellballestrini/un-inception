fib :: Int -> Int
fib n
    | n <= 1    = n
    | otherwise = fib (n-1) + fib (n-2)

main :: IO ()
main = mapM_ (\i -> putStrLn $ "fib(" ++ show i ++ ") = " ++ show (fib i)) [0..10]

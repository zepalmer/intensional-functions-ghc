module Main where


main :: IO ()
main = putStrLn . show $ qqq ['c']

qqq :: [a] -> Maybe (a, [a])
qqq ts = do { (a:b:as) <- Just ts
            ; return (a, as) }

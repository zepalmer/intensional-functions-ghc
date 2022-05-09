main :: IO ()
main = do
    print $ (asinh 0 :: Double)
    print $ (atanh (-0) :: Double)
    print $ (asinh 1e300 :: Double)

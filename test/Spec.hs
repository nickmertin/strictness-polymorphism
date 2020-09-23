{-# LANGUAGE BangPatterns #-}

import Data.StrictnessPolymorphism.List

main :: IO ()
main = do
    putStrLn "a"
    !l <- return $ lazyFromList [1..10000000]
    -- Uncomment to use strict version instead:
    -- !l <- return $ strictFromList [1..10000000]
    putStrLn "b"
    putStrLn $ show l

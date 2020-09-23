{-# LANGUAGE DataKinds, DeriveFunctor #-}

module Data.StrictnessPolymorphism.List
    ( module Data.StrictnessPolymorphism
    , List()
    , strictFromList
    , lazyFromList
    , toList
    ) where

import Data.StrictnessPolymorphism

data List s a = Nil | Cons a (StrictPoly s (List s a))
    deriving (Show, Functor)

fromList :: KnownStrictness s => [a] -> List s a
fromList = fromList' id

fromList' :: KnownStrictness s => (List s a -> b) -> [a] -> b
fromList' f [] = f Nil
fromList' f (a:as) = newStrictPoly (f . Cons a) (fromList as)

strictFromList :: [a] -> List Strict a
strictFromList = fromList

lazyFromList :: [a] -> List Lazy a
lazyFromList = fromList

toList :: List s a -> [a]
toList Nil = []
toList (Cons a l) = a : toList (runStrictPoly l)

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

strictFromList :: [a] -> List Strict a
strictFromList [] = Nil
strictFromList (a:as) = newStrictPoly (strictFromList as) (Cons a)

lazyFromList :: [a] -> List Lazy a
lazyFromList [] = Nil
lazyFromList (a:as) = newStrictPoly (lazyFromList as) (Cons a)

toList :: List s a -> [a]
toList Nil = []
toList (Cons a l) = a : toList (runStrictPoly l)

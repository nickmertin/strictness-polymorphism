{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module Data.StrictnessPolymorphism
    ( Strictness(..)
    , StrictPoly()
    , KnownStrictness
    , newStrictPoly
    , newStrict
    , newLazy
    , runStrictPoly
    ) where

import Data.Proxy

data Strictness = Strict | Lazy

class KnownStrictness (s :: Strictness) where
    constructStrictPoly' :: (StrictPoly s a -> b) -> a -> b

instance KnownStrictness Strict where
    constructStrictPoly' = newStrict

instance KnownStrictness Lazy where
    constructStrictPoly' = newLazy

data StrictPoly (s :: Strictness) a where
    StrictPoly :: KnownStrictness s => a -> StrictPoly s a

instance Show a => Show (StrictPoly s a) where
    show = show . runStrictPoly

instance KnownStrictness s => Functor (StrictPoly s) where
    fmap f s = newStrictPoly id (f $ runStrictPoly s)

newStrictPoly :: KnownStrictness s => (StrictPoly s a -> b) -> a -> b
newStrictPoly = constructStrictPoly'

newStrict :: (StrictPoly Strict a -> b) -> a -> b
newStrict f a = a `seq` f $ StrictPoly a

newLazy :: (StrictPoly Lazy a -> b) -> a -> b
newLazy f a = f $ StrictPoly a

runStrictPoly :: StrictPoly s a -> a
runStrictPoly (StrictPoly a) = a

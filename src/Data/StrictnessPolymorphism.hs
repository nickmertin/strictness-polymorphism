{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module Data.StrictnessPolymorphism
    ( Strictness(..)
    , StrictPoly()
    , KnownStrictness
    , newStrictPoly
    , runStrictPoly
    ) where

import Data.Proxy

data Strictness = Strict | Lazy

class KnownStrictness (s :: Strictness) where
    constructStrictPoly' :: a -> (StrictPoly s a -> b) -> b

instance KnownStrictness Strict where
    constructStrictPoly' a f = a `seq` f $ StrictPoly a

instance KnownStrictness Lazy where
    constructStrictPoly' a f = f $ StrictPoly a

data StrictPoly (s :: Strictness) a where
    StrictPoly :: KnownStrictness s => a -> StrictPoly s a

instance Show a => Show (StrictPoly s a) where
    show = show . runStrictPoly

instance KnownStrictness s => Functor (StrictPoly s) where
    fmap f s = newStrictPoly (f $ runStrictPoly s) id

newStrictPoly :: KnownStrictness s => a -> (StrictPoly s a -> b) -> b
newStrictPoly = constructStrictPoly'

runStrictPoly :: StrictPoly s a -> a
runStrictPoly (StrictPoly a) = a

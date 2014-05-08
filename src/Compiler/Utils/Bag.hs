module Compiler.Utils.Bag (
  emptyBag) where

data Bag a = EmptyBag

emptyBag :: Bag a
emptyBag = EmptyBag

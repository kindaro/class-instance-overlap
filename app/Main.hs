{-# LANGUAGE
    ExistentialQuantification
  #-}

module Main where

import Lib

data S = forall a. (Show a) => S a

instance Show S where
    show (S x) = show x

main :: IO ()
main = sequence_ $ fmap (putStrLn . show) examples
    where examples = [ S exampleInteger
                     , S exampleArbitrary
                     , S exampleFoldable
                     ]

{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  #-}

module Lib
    ( exampleInteger, exampleArbitrary, exampleFoldable
    ) where

import qualified Data.Set as S

class (Monoid m) => M s m where
    lift :: s -> m
    join :: s -> m -> m
    join = mappend.lift

instance {-# OVERLAPPABLE #-} (Foldable f, M s m) => M (f s) m where
    lift = foldMap lift

instance (Monoid m) => M m m where
    lift = id

-- instance M Integer [Integer] where
--     lift = (:[])
-- -- I suppose this one would not be applied in the presence of a more general one.

instance M a [a] where
    lift = (:[])

exampleInteger :: [Integer]
exampleInteger = (1 :: Integer) `join` [2,3]

exampleArbitrary :: [Bool]
exampleArbitrary = True `join` [False, True]

-- exampleArbitrary' :: [Maybe Bool]
-- exampleArbitrary' = Just True `join` [Just False, Just True]
-- -- This would not compile:
-- --
-- --     Overlapping instances for M (Maybe Bool) [Maybe Bool]
-- --       arising from a use of ‘join’
-- --     Matching instances:
-- --       instance [overlappable] (Foldable f, M s m) => M (f s) m
-- --         -- Defined at src/Lib.hs:17:31
-- --       instance M a [a] -- Defined at src/Lib.hs:27:10

exampleFoldable :: [Integer]
exampleFoldable = S.fromList [(1 :: Integer), 2] `join` [3, 4]

-- exampleFoldable' :: [Maybe Integer]
-- exampleFoldable' = S.fromList [Just (1 :: Integer), Just 2] `join` [Just 3, Nothing]
-- -- This would not compile:
-- --     Overlapping instances for M (Maybe Integer) [Maybe Integer]
-- --       arising from a use of ‘join’
-- --     Matching instances:
-- --       instance [overlappable] (Foldable f, M s m) => M (f s) m
-- --         -- Defined at src/Lib.hs:17:31
-- --       instance M a [a] -- Defined at src/Lib.hs:27:10


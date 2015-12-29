module Utils where

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then []
                                else takeUntil p xs

-- | TakeUntil with an other predicate (`hard` constraint) that doesn't take the last element
takeUntilWithHard :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeUntilWithHard _ _ [] = []
takeUntilWithHard hard soft (x:xs) = if hard x then []
                                           else x : (if soft x then []
                                                     else takeUntilWithHard hard soft xs)

import Data.List

smallestMaybeList :: Maybe [Integer] -> Maybe[Integer] -> Maybe[Integer]
smallestMaybeList (Just a) Nothing = (Just a)
smallestMaybeList Nothing (Just b) = (Just b)
smallestMaybeList Nothing Nothing = Nothing
smallestMaybeList (Just a) (Just b) = if (length a) < (length b) then (Just a) else (Just b)

makeChange :: Integer -> [Integer] -> Maybe [Integer]
makeChange x coins = memoMake x
  where 
    makeList:: [Maybe [Integer]]
    makeList =  make <$> [0 ..]
    make :: Integer -> Maybe [Integer]
    make 0 = Just []
    make amount
      | amount  < 0 = Nothing
      | otherwise = foldl' smallestMaybeList Nothing [(++) [c] <$> memoMake (amount -c) | c <- coins, c<=amount]
    memoMake :: Integer -> Maybe [Integer]
    memoMake n = makeList !! (fromInteger n)

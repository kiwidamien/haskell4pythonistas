module Bowling (score, BowlingError(..)) where 
import Control.Monad


data BowlingError = IncompleteGame | InvalidRoll {rollIndex::Int, rollValue:: Int}
    deriving (Eq, Show)

type IndexedRoll = (Int, Int)
type Frame = [IndexedRoll]


takeFrame :: [IndexedRoll] -> (Either BowlingError Frame, [IndexedRoll])
takeFrame (first:second:rest)
  | (snd first) == 10 = (Right [first, second, head rest], second:rest)
  | (snd first) > 10 = (Left (InvalidRoll (fst first) (snd first)), second: rest)
  | (snd first) + (snd second) == 10 = (Right [first, second, head rest], rest)
  | (snd first) + (snd second) > 10 = (Left (InvalidRoll (fst second) (snd second)), rest)
  | otherwise = (Right [first, second], rest)
takeFrame _ = error "This is where we would make the incomplete game"

scoreFrame :: Frame -> Int
scoreFrame = sum . map snd 

-- validation
isStrike :: Frame -> Bool 
isStrike frame = snd (head frame) == 10

isSpare :: Frame -> Bool 
isSpare frame = (not (isStrike frame)) && ((sum (map snd (take 2 frame)))==10)

validateLastFrame:: Frame -> [IndexedRoll] -> Bool
validateLastFrame lastFrame remainder
  | (isStrike lastFrame) = ((length remainder) == 2) && ((isStrike remainder) || ((scoreFrame remainder) <= 10))
  | (isSpare lastFrame) = (length remainder) == 1
  | otherwise = (length remainder) == 0

  
validateGame :: ([Either BowlingError Frame], [IndexedRoll]) -> Either BowlingError [Frame]
validateGame (frames, rest) = go (sequence frames) rest 
  where go (Left err) _ = Left err
        go (Right frames) rest
          | length frames /= 10 = error "Incomplete Game"
          | not (validateLastFrame lastFrame rest) = error "Last Frame is fishy"
          | otherwise = Right frames
          where lastFrame = last frames

takeGame :: [Int] -> ([Either BowlingError Frame], [IndexedRoll])
takeGame rolls = go [] $ zip [0..] rolls
    where go acc [] = (acc, [])
          go acc r 
            | (length acc) < 10 = go (acc ++ [frame]) rest
            | otherwise = (acc, r)
                  where (frame, rest) = takeFrame r


score :: [Int] -> Either BowlingError Int
score rolls = if (length invalidRolls > 0) then (head invalidRolls) else (go game)
  where go (Left err) = Left err
        go (Right frames) = Right (sum $ map scoreFrame frames)
        game = validateGame $ takeGame rolls 
        invalidRolls = map (\ (pos, val) -> Left (InvalidRoll pos val)) $ filter (\ (pos, val) -> (val < 0) || (val > 10)) $ zip [0..] rolls


a = [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6]::[Int]
perfect = replicate 12 (10::Int)
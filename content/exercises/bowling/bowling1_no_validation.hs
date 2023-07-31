module Bowling (score, BowlingError(..)) where 

data BowlingError = IncompleteGame | InvalidRoll {rollIndex::Int, rollValue:: Int}
    deriving (Eq, Show)

type IndexedRoll = (Int, Int)
type Frame = [IndexedRoll]


takeFrame :: [IndexedRoll] -> (Frame, [IndexedRoll])
takeFrame (first:second:rest)
  | (snd first) == 10 = ([first, second, head rest], second:rest)
  | (snd first) > 10 = error "Invalid roll for the first roll"
  | (snd first) + (snd second) == 10 = ([first, second, head rest], rest)
  | (snd first) + (snd second) > 10 = error "Invalid roll for the second"
  | otherwise = ([first, second], rest)
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
  | (isStrike lastFrame) = (length remainder) == 2
  | (isSpare lastFrame) = (length remainder) == 1
  | otherwise = (length remainder) == 0

validateGame :: ([Frame], [IndexedRoll]) -> [Frame]
validateGame (frames, rest)
  | length frames /= 10 = error "Incomplete Game"
  | not (validateLastFrame lastFrame rest) = error "Last Frame is fishy"
  | otherwise = frames
  where lastFrame = last frames

takeGame :: [Int] -> ([Frame], [IndexedRoll])
takeGame rolls = go [] $ zip [0..] rolls
    where go acc [] = (acc, [])
          go acc r 
            | (length acc) < 10 = go (acc ++ [frame]) rest
            | otherwise = (acc, r)
                  where (frame, rest) = takeFrame r

score rolls = sum $ map scoreFrame $ validateGame $ takeGame rolls

a = [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6]::[Int]
perfect = replicate 12 (10::Int)
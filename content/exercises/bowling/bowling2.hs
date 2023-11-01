module Bowling (score, BowlingError(..)) where 


data BowlingError = IncompleteGame | InvalidRoll {rollIndex::Int, rollValue:: Int}
    deriving (Eq, Show)

type IndexedRoll = (Int, Int)
type Frame = [IndexedRoll]

validateRoll :: Int -> Bool
validateRoll val = (val >= 0) && (val<=10)

validateLastFrame:: Frame -> [IndexedRoll] -> Either BowlingError Frame 
validateLastFrame lastFrame remainder
  | (length invalidRolls) > 0 = (Left $ InvalidRoll (fst $ head invalidRolls) (snd $ head invalidRolls))
  | (isStrike lastFrame) = processStrike lastFrame remainder
  | (isSpare lastFrame) = processSpare lastFrame remainder 
  | otherwise = if (length remainder)==0 then Right lastFrame else Left IncompleteGame
  where invalidRolls = filter (\ (pos, val) -> not $ validateRoll val) remainder
        processStrike lastFrame r
          | (length r) /= 2 = Left IncompleteGame
          | isStrike r = Right lastFrame
          | (scoreFrame r) > 10 = Left (uncurry InvalidRoll (r!!1))
          | otherwise = Right lastFrame 
        processSpare lastFrame r 
          | (length r) /= 1 = Left IncompleteGame
          | otherwise = Right lastFrame


-- Let's do all validation here!
takeFrame :: [IndexedRoll] -> (Either BowlingError Frame, [IndexedRoll])
takeFrame ((fp,fv):(sp,sv):rest)
  | not (validateRoll fv) = (Left (InvalidRoll fp fv), rest)
  | not (validateRoll sv) = (Left (InvalidRoll sp sv), rest)
  | fv == 10 = (Right [(fp,fv), (sp,sv), head rest], (sp, sv):rest)
  | fv > 10 = (Left (InvalidRoll fp fv), (sp,sv): rest)
  | fv + sv == 10 = (Right [(fp,fv), (sp,sv), head rest], rest)
  | fv + sv > 10 = (Left (InvalidRoll sp sv), rest)
  | otherwise = (Right [(fp,fv), (sp,sv)], rest)
takeFrame _ = error "This is where we would make the incomplete game"



scoreFrame :: Frame -> Int
scoreFrame = sum . map snd 

-- validations
isStrike :: Frame -> Bool 
isStrike frame = snd (head frame) == 10

isSpare :: Frame -> Bool 
isSpare frame = (not (isStrike frame)) && ((sum (map snd (take 2 frame)))==10)

validateGame :: (Either BowlingError [Frame], [IndexedRoll]) -> Either BowlingError [Frame]
validateGame (frames, rest) = go frames rest
  where go (Left err) _ = Left err
        go (Right f) rest 
          | length f /= 10 = Left IncompleteGame
          | otherwise = dependOnLastFrame validatedLastFrame
          where lastFrame = last f
                validatedLastFrame = validateLastFrame lastFrame rest
                dependOnLastFrame (Left err) = (Left err)
                dependOnLastFrame _ = Right f
          

takeGame :: [Int] -> (Either BowlingError [Frame], [IndexedRoll])
takeGame rolls = (sequence (fst output), (snd output))
    where output = go [] ((zip [0..] rolls))
          go acc [] = (acc, [])
          go acc r 
            | (length acc) < 10 = go (acc ++ [frame]) rest
            | otherwise = (acc, r)
                  where (frame, rest) = takeFrame r

score :: [Int] -> Either BowlingError Int
score rolls 
  | (length invalidRolls) > 0 = Left (InvalidRoll (fst firstInvalidRoll) (snd firstInvalidRoll))
  | otherwise = go game
  where 
        invalidRolls = filter (not . validateRoll . snd) $ zip [0..] rolls
        firstInvalidRoll = head invalidRolls
        game = validateGame $ takeGame rolls
        go (Left err) = Left err 
        go (Right frames) = Right (sum $ map scoreFrame frames)
  
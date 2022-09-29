import Data.Maybe (catMaybes)
import Control.Monad ( replicateM )

newtype TotalScore = TotalScore Integer deriving Show

data Score = NotTen Integer | Spare | Strike | Blank deriving (Show, Eq)

data Frame = RegFrame (Score, Score) | StrikeFrame deriving Show
newtype LastFrame = LastFrame (Score, Score, Score) deriving Show


turnOne :: Integer -> Maybe Score
turnOne i
 | i == 10 = Just Strike
 | i > 10 || i < 0 = Nothing
 | otherwise = Just (NotTen i)

turnTwo :: Score -> Integer -> Maybe Frame
turnTwo Strike _ = Just StrikeFrame
turnTwo (NotTen i) j
 | i + j == 10 = Just $ RegFrame (NotTen i, Spare)
 | i + j < 10 && i + j > 0 = Just $ RegFrame (NotTen i, NotTen (i+j))
 | otherwise = Nothing
turnTwo _ _ = Nothing


mkFrame :: Integer -> Integer -> Maybe Frame
mkFrame score1 score2 = turnOne score1 >>= \x -> if x == Strike
                                                 then Just StrikeFrame
                                                 else turnTwo x score2

inputScore :: IO Score
inputScore = do inp <- readLn :: IO Integer
                let Just val = turnOne inp
                return val

inputFrame :: IO (Maybe Frame)
inputFrame = do a <- inputScore
                case a of
                    Strike -> return $ Just StrikeFrame
                    (NotTen i) -> do j <- readLn :: IO Integer
                                     return (mkFrame i j)
                    _ -> undefined -- not possible


nineFrames :: IO [Maybe Frame]
nineFrames = replicateM 9 inputFrame

mkLastFrame :: Frame -> IO LastFrame
mkLastFrame (RegFrame (NotTen i, NotTen j)) = return $ LastFrame (NotTen i, NotTen j, Blank)

mkLastFrame StrikeFrame = nextTwo Strike =<< inputFrame
                                   where nextTwo s g = case g of
                                                        (Just StrikeFrame) -> do a <- inputScore
                                                                                 return $ LastFrame (Strike, Strike, a)
                                                        (Just (RegFrame (a,b))) -> return $ LastFrame (s,a,b)
                                                        Nothing -> undefined

mkLastFrame (RegFrame (NotTen i, Spare)) = combine (NotTen i, Spare) <$> inputScore
                            where combine (a,b) c = LastFrame (a,b,c)

mkLastFrame _ = undefined

toScore :: [Frame] -> LastFrame -> TotalScore
toScore frames last = TotalScore $ go (NotTen 0) allPins
                    where allPins = concatMap toScore frames ++ toScoreLast last

                            where toScore StrikeFrame = [Strike]
                                  toScore (RegFrame (a,b)) = [a,b]
                                  toScoreLast (LastFrame (a,b,c)) = (toList (a,b,c))
                                                                where toList (a,b, Blank) = [a,b]
                                                                      toList (a,b,c) = [a,b,c]


                          go :: Score -> [Score] -> Integer
                          go Strike (x:y:xs) = 10 + scoreToNum x + scoreToNum y + go x (y:xs)
                          go x (Spare:y:xs) =  10 + scoreToNum y + go y xs
                          go (NotTen i) (x:xs) = i + go x xs
                          go x [] = scoreToNum x
                          go _ _ = 0

scoreToNum :: Score -> Integer
scoreToNum Strike = 10
scoreToNum Spare = 10
scoreToNum (NotTen i) = i
scoreToNum _ = 0

bowlingGame :: IO TotalScore
bowlingGame  = do a <- nineFrames
                  let listFrames = catMaybes a
                  (Just b) <- inputFrame
                  c <- mkLastFrame b
                  mapM_ print listFrames
                  print c
                  return $ toScore listFrames c

sparesGame :: TotalScore
sparesGame = let a = catMaybes $ replicate 9 (mkFrame 8 2)
              in toScore a (LastFrame (NotTen 8, NotTen 2, NotTen 8))

strikesGame :: IO TotalScore
strikesGame = do  let a = replicate 9 (Just StrikeFrame)
                  let listFrames = catMaybes a
                  let c = LastFrame (Strike, Strike, Strike)
                  mapM_ print listFrames
                  print c
                  return $ toScore listFrames c

tests = do let a = sparesGame
           print a
           strikesGame

main = do a <- bowlingGame
          print a
letterFits a b
 | b == '_' = [True]
 | a == b = [True]
 | a == '_' = [True]
 | otherwise = [False]

wordFits [] [] = []
wordFits a b = letterFits (head a) (head b) ++ wordFits (tail a) (tail b)

wordFits2 a b
 | False `elem` wordFits a b = False
 | otherwise = True

pairPossibleDiv a b
 | b == 0 || a == 0 = []
 | b `rem` a == 0 = [b `quot` a]
 | a `rem` b == 0 = [a `quot` b]
 | otherwise = []

pairPossible a b = [a+b, a-b, b-a, a * b] ++ pairPossibleDiv a b

allPairs [_] = []
allPairs [] = []
allPairs (x:xs) = [ (a, b) | let a = x, b <- xs] ++ allPairs xs

allpairPossible x = [pairPossible a b | (a, b) <- allPairs x]

pos1 x = [(a, b) | a <- head (allpairPossible x), b <- allpairPossible x!!5]
pos2 x = [(a, b) | a <- allpairPossible x!!1, b <- allpairPossible x!!4]
pos x = pos1 x ++ pos2 x
allTogether x = [pairPossible a b | (a, b) <- pos x]
removeOuterList [] = []
removeOuterList (x:xs) = x ++ removeOuterList xs
final24 x = 24 `elem` removeOuterList (allTogether x)

main = do
  print(final24 [1, 2, 2, 2])
  print(final24 [100, 2, 3, 2])
  print(final24 [1, 2, 3, 4])

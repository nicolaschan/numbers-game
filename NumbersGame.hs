import Data.List

data Operation a = Operation {  operation :: (a -> a -> a),
                                toString  :: String,
                                commutative :: Bool }

{-
            
            *
          /   \
         4     +
              / \
             3   2

             = (4*(3+2))

-}
data Expression a = Number a | Expression (Operation a) (Expression a) (Expression a)

instance Show (Operation a) where
  show = toString

instance Eq (Operation a) where
  (==) op1 op2 = (show op1 == show op2)

instance (Show a) => Show (Expression a) where
  show (Number a) = show a
  show (Expression op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

instance (Eq a, Num a) => Eq (Expression a) where
  (==) (Number n) (Number m) = n == m
  (==) (Expression op1 e1 e2) (Expression op2 e3 e4)
    | op1 /= op2 = False
    | commutative op1 = (and [ e1 == e3, e2 == e4 ]) || (and [ e1 == e4, e2 == e3 ])
    | otherwise = and [ e1 == e3, e2 == e4 ]
  (==) _ _ = False

instance (Eq a, Num a, Ord a) => Ord (Expression a) where
  (>) e1 e2 = (evaluate e1) > (evaluate e2)
  (<) e1 e2 = (evaluate e1) < (evaluate e2)
  (<=) e1 e2 = (evaluate e1) <= (evaluate e2)

expressionFromString :: (Num a, Read a) => String -> Expression a
expressionFromString str
  | head str == '(' = Number 0
  | otherwise = Number (read str)

operations :: (Fractional a, Num a, Read a, Show a) => [Operation a]
operations = [  Operation { operation = (+), toString = "+", commutative = True },
                Operation { operation = (-), toString = "-", commutative = False },
                Operation { operation = (*), toString = "*", commutative = True },
                Operation { operation = (/), toString = "/", commutative = False }]

standardOrder :: Expression a -> Expression a
standardOrder e = e

evaluate :: (Num a) => Expression a -> a
evaluate (Number n) = n
evaluate (Expression op e1 e2) = (operation op) (evaluate e1) (evaluate e2)

allPossibleExpressions :: [a] -> [Operation a] -> [Expression a]
allPossibleExpressions ns ops = concat [ allPossibleExpressions' perm ops | perm <- permutations (map Number ns) ]

allPossibleExpressions' :: [Expression a] -> [Operation a] -> [Expression a]
allPossibleExpressions' [] _ = []
allPossibleExpressions' [e] _ = [e]
allPossibleExpressions' [e1,e2] ops = [ Expression op e1 e2 | op <- ops ]
allPossibleExpressions' (e1:et) ops = 
  [ Expression op e1 e | op <- ops, e <- allPossibleExpressions' et ops ] ++
  [ Expression op e (last et) | op <- ops, e <- allPossibleExpressions' (e1:init et) ops ]

solve :: (Eq a, Num a) => [a] -> [Operation a] -> a -> [Expression a]
solve ns ops target = filter (\e -> (evaluate e) == target) (allPossibleExpressions ns ops)

findDifficultProblems :: (Enum a, Eq a, Num a) => [a] -> [Operation a] -> Int -> [a]
findDifficultProblems ns ops difficulty = [ x | x <- [0..], (length . nub $ solve ns ops x) == difficulty ]

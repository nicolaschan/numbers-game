{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Exception (try, evaluate)
import Data.List (nub, permutations)
import Data.Ratio (numerator, denominator)
import GHC.Exception (SomeException)
import System.IO.Unsafe (unsafePerformIO)

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

newtype PrettyRational = PrettyRational Rational deriving (Eq, Enum, Ord, Num, Fractional, Read) 
default (PrettyRational)

instance Show (PrettyRational) where
  show (PrettyRational r)
    | denominator r == 1 = show $ numerator r
    | otherwise = show r

instance (Eq a, Num a) => Eq (Expression a) where
  (==) (Number n) (Number m) = n == m
  (==) (Expression op1 e1 e2) (Expression op2 e3 e4)
    | op1 /= op2 = False
    | commutative op1 = (and [ e1 == e3, e2 == e4 ]) || (and [ e1 == e4, e2 == e3 ])
    | otherwise = and [ e1 == e3, e2 == e4 ]
  (==) _ _ = False

instance (Eq a, Num a, Ord a) => Ord (Expression a) where
  (>) e1 e2 = (eval e1) > (eval e2)
  (<) e1 e2 = (eval e1) < (eval e2)
  (<=) e1 e2 = (eval e1) <= (eval e2)

expressionFromString :: (Num a, Read a) => String -> Expression a
expressionFromString str | head str == '(' = Number 0
  | otherwise = Number (read str)

operations :: (Fractional a, Num a, Read a, Show a) => [Operation a]
operations = [  Operation { operation = (+), toString = "+", commutative = True },
                Operation { operation = (-), toString = "-", commutative = False },
                Operation { operation = (*), toString = "*", commutative = True },
                Operation { operation = (/), toString = "/", commutative = False }]

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

safeEvaluate :: (a -> a -> a) -> a -> a -> IO (Either SomeException a)
safeEvaluate op left right = try (evaluate $ op left right)

-- Ironic that the "safeOperation" function uses "unsafePerformIO"
-- Let me know if this is bad and how it should be improved.
-- Remember, I want it to work with arbitrary (Num a => a -> a -> a) functions
safeOperation :: (a -> a -> a) -> a -> a -> Maybe a
safeOperation op left right = unsafePerformIO $ do
  result <- safeEvaluate op left right
  case result of
    Left _ -> return Nothing
    Right x -> return (Just x)

eval :: (Num a) => Expression a -> Maybe a
eval (Number n) = Just n
eval (Expression op e1 e2) = do
  left <- eval e1
  right <- eval e2
  safeOperation (operation op) left right

allPossibleExpressions :: [a] -> [Operation a] -> [Expression a]
allPossibleExpressions ns ops = concat [ allPossibleExpressions' perm ops | perm <- permutations (map Number ns) ]

allPossibleExpressions' :: [Expression a] -> [Operation a] -> [Expression a]
allPossibleExpressions' [] _ = []
allPossibleExpressions' [e] _ = [e]
allPossibleExpressions' [e1,e2] ops = [ Expression op e1 e2 | op <- ops ]
allPossibleExpressions' (e1:et) ops = 
  [ Expression op e1 e | op <- ops, e <- allPossibleExpressions' et ops ] ++
  [ Expression op e (last et) | op <- ops, e <- allPossibleExpressions' (e1:init et) ops ]

matches :: (Num a, Eq a) => a -> Expression a -> Bool
matches target expr = case (eval expr) of
  Nothing -> False
  Just result -> result == target

solve :: (Eq a, Num a) => [a] -> [Operation a] -> a -> [Expression a]
solve ns ops target = filter (matches target) (allPossibleExpressions ns ops)

findDifficultProblems :: (Enum a, Eq a, Num a) => [a] -> [Operation a] -> Int -> [a]
findDifficultProblems ns ops difficulty = [ x | x <- [0..], (length . nub $ solve ns ops x) == difficulty ]

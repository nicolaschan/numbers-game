# Numbers Game
Solver for number operations game

## Rules
_Objective:_ With the following constraints, find an expression that equals the target number.
- Use any provided digit exactly once for each time it is provided
- Use any operation any number of times
- Grouping with parentheses allowed

## Example
Using the numbers `[6,7,8,9]` and the operations `[+,-,*,/]`, how can you make `44`?
```hs 
ghci> :l NumbersGame.hs
ghci> default (PrettyRational) -- Display numbers nicely and with perfect precision
ghci> solve [6,7,8,9] operations 44
[(8*(7-(9/6))),((7-(9/6))*8)]
```

By default, the program supplies the four basic arithmetic operations `[+,-,*,/]` as a list called `operations`, but it is possible to add more operations by defining your own. For example, you can define exponentiation:
```hs
ghci> default (Integer, Double) -- (**) doesn't work on Rationals
ghci> let exp = Operation { operation = (**), toString = "^", commutative = False }
ghci> solve [2,3,4] (exp:operations) 8
[(4.0^(3.0/2.0)),((4.0-2.0)^3.0),((4.0/2.0)^3.0)]
```

Sometimes you will get lots of results that are very similar because operations commute. You can filter out expressions that are similar to each other with `nub` because they are considered equal.
```hs
ghci> solve [6..9] operations 68
[(8*(7+(9/6))),(((9/6)+7)*8),((7+(9/6))*8),(8*((9/6)+7))]
ghci> nub $ solve [6..9] operations 68
[(8*(7+(9/6)))]
```

To challenge yourself or your friends, you can search for difficult target numbers. In the example below, `findDifficultProblems` searches `[0..]` for target numbers such that the length of the list of unique results is `1`.
```hs
ghci> take 5 $ findDifficultProblems [6,7,8,9] operations 1
[7,8,44,51,68]
```

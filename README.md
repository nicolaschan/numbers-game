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
ghci> solve [6,7,8,9] operations 44
[(8.0*(7.0-(9.0/6.0))),((7.0-(9.0/6.0))*8.0)]
```

By default, the program supplies the four basic arithmetic operations `[+,-,*,/]` as a list called `operations`, but it is possible to add more operations by defining your own. For example, you can define exponentiation:
```hs
ghci> let exp = Operation { operation = (**), toString = "^" }
ghci> solve [2,3,4] (exp:operations) 8
[(4.0^(3.0/2.0)),((4.0-2.0)^3.0),((4.0/2.0)^3.0)]
```

To challenge yourself or your friends, you can search for difficult target numbers. In the example below, `findDifficultProblems` searches `[0..]` for target numbers such that the length of the list of results is `2`.
```hs
ghci> take 3 $ findDifficultProblems [6,7,8,9] operations 2
[7.0,44.0,51.0]
```

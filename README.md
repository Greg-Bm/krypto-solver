# About
This is a program to solve games such as Krypto and 24. In such games, you are given a list of numbers and a 'goal' number, and need to use each number exactly once in an arithmetic expression to get a target number.
Given a list of numbers and a goal, the solver does the following:
- Generates every possible expression from the numbers using a brute force approach
- Selects expressions which evaluate to the target number. Expressions which evaluate to a different number are discarded. Expressions which do not evaluate at all, for example 1/0, are also discarded.
- Identifies sets of solutions which are equilavent up to simple mathematical identities (e.g. associative and commutative laws, or adding vs subtracting an identity), and leaves only one representative from each set. (This ensures that solutions are more or less unique).
- Returns all unique solutions

The solver may use the operations '+', '-', '*' and '/'. It assumes that any rational value is allowed at an intermediate result. (Note that this differs from standard Krypto)

# Example usage

```
$ cabal run krypto-solver -- [1,2,3,4] 24
3 unique solutions found.
1 * 2 * 3 * 4
4 * (1 + 2 + 3)
(1 + 3) * (2 + 4)
```

# Performance
Fairly fast for up to 5 numbers. 6 numbers may take a while.

# Ideas for improvements and modifications
- Add new binary operations, such as exponent ^, or concatenation <>
- Add support for unary operations such as sqrt or factorial, and a way to limit the # of times that they can appear.
- Add support for only some numbers being used.
- Make it faster. (I expect this to be pretty hard)

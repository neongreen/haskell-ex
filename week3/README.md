## Week 3 (September 12 – September 18)

This week's tasks are easier.

### 11. Binary conversion `{binary}`

Convert a number to binary and back:

```
> bin 123
"1111011"

> dec "1111011"
123
```

You don't have to handle negative numbers.

### 12. Working with expressions `{expr}`

An arithmetic expression can be represented with the following datatype:

``` haskell
data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
```

Write functions to print and evaluate expressions:

```
> showExpr (Mul (Number 3) (Add (Number 5) (Number 7)))
"3*(5+7)"

> evalExpr (Mul (Number 3) (Add (Number 5) (Number 7)))
36
```

(Use `div` for division.)

You get bonus points if you only print parentheses when they are needed – e.g. `3+5*7` doesn't need any parentheses. However, it's an optional requirement.

Also, don't forget about parentheses around negative numbers.

### 13. Compute a moving average `{average}`

A simple [moving average](https://en.wikipedia.org/wiki/Moving_average) is a way to smooth data points. Assume that you have a list:

```
[1,5,3,8,7,9,6]
```

Then a moving average with window size 4 will work like this:

```
average [1]         = 1
average [1,5]       = 3
average [1,5,3]     = 3
average [1,5,3,8]   = 4.25
-- now we're starting to lose one element as we go forward
average [5,3,8,7]   = 5.75
average [3,8,7,9]   = 6.75
average [8,7,9,6]   = 7.5

averaged list = [1,3,3,4.25,5.75,6.75,7.5]
```

Implement moving average with an arbitrary window size:

```
> moving 4 [1, 5, 3, 8, 7, 9, 6]
[1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]

> moving 2 [1, 5, 3, 8, 7, 9, 6]
[1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
```

### 14. XOR encryption `{xor}`

Write a program that encrypts a file by [XORing](https://en.wikipedia.org/wiki/XOR_cipher) it with a key. For instance, if the file is “abracadabra” and the key is “XYZ”, then the result would be

```
      abracadabra
XOR   XYZXYZXYZXY
  =   9;(9:;<88*8
```

It should accept the file name and the key as command-line arguments, and overwrite the file. (Due to the way XOR works, encrypting an already encrypted file will decrypt it.) Use [bytestring](https://hackage.haskell.org/package/bytestring) to read the file, and encode the key as UTF8 (with [`encodeUtf8`](http://hackage.haskell.org/package/text/docs/Data-Text-Encoding.html#v:encodeUtf8)).

### 15. Table formatting `{table}`

The user enters a table of numbers (they're all integers but they can be negative). Your task is to format it nicely by aligning the numbers and making sure you don't output any leading zeroes. An example log:

```
Enter a table:
01 200 -03
5 60 700
080 900 1000

 1 200   -3
 5  60  700
80 900 1000
```

You can assume that the table will be rectangular; handling any extra cases is not needed. The end of the input is marked with a blank line (i.e. you should read the rows until you encounter a blank line).

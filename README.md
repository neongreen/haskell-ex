# Haskell exercises

## What is it

These are exercises for the Alpha study group (if you want to participate, first get into the [Haskell Learning Group](https://github.com/haskell-learning-group/haskell-learning-group) and then ask @neongreen). You have a week to solve each set. Cheating will be punishable by spiders.

The exercises are beginner-to-intermediate level. Expect to learn how to write algorithms, solve simple problems with Haskell, use common libraries, write sites, talk to databases, create interfaces, parse things, do weird type-level stuff, and more.

## Stuff that you should know if you're already in

To submit a solution, create a folder named `<exercise code>/<your Github nick>`. It doesn't matter how to call the `.hs` file, but `Main.hs` is a good choice. If you have commit access – and you should – forking the repository is *not* needed. If you can't do `git push`, it might be because others have changed the repository in the meantime; doing `git pull --rebase` should fix it.

Don't forget to use [hlint](https://github.com/ndmitchell/hlint) on your code – it often gives good suggestions on how to improve it. (They aren't *always* good, however! If you're unsure, ask.)

You can see yours (and others') progress in [this table](https://docs.google.com/spreadsheets/d/1PEF7K42M-cq1XgiAaqwf-XLeJP2wo3Dc8pU3SsD_R8s/edit?usp=sharing).

At the end of each week each exercise is explained by someone who has solved it.

## Week 1 (August 26 – September 4)

### 1. Find scary words `{scary}`

If you assign numbers to letters (A=1, B=2, ..., Z=26), then a word is scary if the sum of its letters is 13. “baaed”, for instance, is scary (especially when at first you don't understand it's a silly verb and think it's an ancient god's name).

Find all scary words in the `words` file (it's usually in `/usr/share/dict/words` or `/usr/dict/words`). If you're on Windows, you can [download it](https://raw.githubusercontent.com/eneko/data-repository/master/data/words.txt).

Common mistakes:

* Treating `zip's` as scary (the mistake is in assigning a numbers to *all* characters, not just non-letter ones, and then `'` usually gets a negative code).

* Treating `Iraq` as scary (uppercase characters should be treated the same as lowercase ones).

### 2. Calculate probability of winning using simulation `{reposts}`

There's a contest going on in a Russian social network: seven prizes will be given to seven randomly chosen people among those who have reposted a certain post. (There are actually 100 prizes, but the other 93 suck, so we'll ignore them.) There are already ~1000000 reposts. My sister wonders: what's the probability of her winning at least one prize (out of those seven) if she reposts the post 10 times (from different accounts)? What about 100 times? 1000 times?

Calculate the answer by running a simulation some number of times (for instance, 10000 times). You can use [`System.Random`](https://hackage.haskell.org/package/random/docs/System-Random.html) or some other random library (e.g. [`Data.Random`](https://hackage.haskell.org/package/random-fu/docs/Data-Random.html)).

If you're not good at probabilistic simulations, [here's a hint](HINTS.md#reposts).

### 3. Write a tic-tac-toe game `{tictactoe}`

Here's a sample log that the player should see (Github might be rendering box characters weirdly but they will look okay in terminal):

~~~
  A B C
 ┏━┯━┯━┓
1┃ │ │ ┃
 ┠─┼─┼─┨
2┃ │ │ ┃
 ┠─┼─┼─┨
3┃ │ │ ┃
 ┗━┷━┷━┛

Your move:
> A1

  A B C
 ┏━┯━┯━┓
1┃X│ │ ┃
 ┠─┼─┼─┨
2┃ │O│ ┃
 ┠─┼─┼─┨
3┃ │ │ ┃
 ┗━┷━┷━┛

Your move:
> B2

This cell is already taken!

Your move:
> B1

  A B C
 ┏━┯━┯━┓
1┃X│X│O┃
 ┠─┼─┼─┨
2┃ │O│ ┃
 ┠─┼─┼─┨
3┃ │ │ ┃
 ┗━┷━┷━┛

Your move:
> A2

  A B C
 ┏━┯━┯━┓
1┃X│X│O┃
 ┠─┼─┼─┨
2┃X│O│ ┃
 ┠─┼─┼─┨
3┃O│ │ ┃
 ┗━┷━┷━┛

Computer won.
~~~

Use [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal) to color `X`s green and `O`s – red.

You can implement any algorithm for computer's moves. Here are some suggestions (ranked from easy to hard):

* Just make a move into any of the empty cells.

* A simple heuristic (if the human puts two in a row, block them).

* Minimax (evaluate all possible boards recursively, pick the move that leads to the situation where no boards are winning for the human).

* More complicated minimax (even if the human can make a tie at any board, choose the move that would lead to the longest game – what if the human would make a mistake later?). See the “A Perfect but Fatalist Player” section in [this article](http://neverstopbuilding.com/minimax).

### 4. Generate a maze using Wilson's algorithm `{wilson}`

The description of the algorithm is quite accessible: http://weblog.jamisbuck.org/2011/1/20/maze-generation-wilson-s-algorithm. Don't try to make it fast – if it can find a 15×15 maze, it's good enough.

Here's a sample 10×10 maze:

```
 ___________________
| | |_     _   _|_  |
| | | | | | |_|_   _|
| |  _|_|_   ___   _|
|  _   ___|  _|_  | |
|  _|_    |   | |_  |
|     |_|_|_| | | |_|
|_| |  _|_  |___   _|
| | |  _    | |  ___|
| | |_|  _|_  |  _| |
|___|_____|_________|

```

### 5. Solve a logic problem using brute-force `{logic-brute}`

Two integer numbers A and B are picked, so that A ≥ B and both numbers are within the range [2, 99]. We tell Mr. P their product (A×B) and Mr. S – their sum (A+B). The following dialog takes place:

P: I don't know the numbers.  
S: I knew you didn't know. I don't know either.  
P: Now I know the numbers.  
S: Now I know them too.

Find A and B. If you can't, [here's a hint](HINTS.md#logic-brute).

For more info on the topic, see Oleg Kiselyov's [“Representing knowledge about knowledge”](http://okmij.org/ftp/Algorithms.html#mr-s-p).

## Week 2 (September 5 – September 11)

### 6. Merge sort `{mergesort}`

Implement a [merge sort](https://en.wikipedia.org/wiki/Merge_sort):

1. Split the list into two sublists (in any way).
2. Recursively sort the two sublists (using the function that you're writing, not a standard `Data.List.sort`).
3. Merge the sublists.

(This variant of the merge sort is called a “top-down sort”. There's also a bottom-up sort, which first splits the list into 1-sized sublists and then proceeds to merge them, two at at time, until after several passes there's only list left. You may implement a bottom-up sort in addition to the top-down sort, but it's not necessary.)

### 7. Silly compression `{compress}`

Consider a string (indexed here for convenience):

```
          111111111122222222223333333333444444444455555555556
0123456789012345678901234567890123456789012345678901234567890
Consider a string. No, consider a different string. Whatever.
```

We can compress it by finding all repetitive substrings and writing their indices instead:

```
Consider a string. No, c[1/10]different[10/9]Whatever.
```

* `[1/10]` refers to 10 characters of the original string starting from index 1 (i.e. 1..10) – `onsider a␣` (with a space at the end)

* `[10/9]` refers to 9 characters starting from index 10 (i.e. 10..18) – `␣string.␣`.

The task is to compress a string into a list of `Either String (Int, Int)` and decompress it:

```
> compress "Consider a string. No, consider a different string. Whatever."
[Left "Consider a string. No, c",
 Right (1,10),
 Left "different",
 Right (10,9),
 "Whatever."]

> decompress it
"Consider a string. No, consider a different string. Whatever."
```

Matches shorter than 3 characters should remain unreplaced (e.g. we could've replaced `a` throughout the text with `Right (9,1)`, but that would be worse than leaving the original `a` in).

Since it's silly compression, don't bother with performance (a quadratic algorithm is okay). If it can compress the first 10 kilobytes of `README.md` in less than 5 minutes, it works.

Use [QuickCheck](https://hackage.haskell.org/package/QuickCheck) to test that decompressing a compressed input always works.

Be sure to test your solution on these tricky tests:

```
> compress "foo|bar|foobar"
[Left "foo|bar|",Right (0,3),Right (4,3)]

> compress "foo|foox:foox"
[Left "foo|",Right (0,3),Left "x:",Right (4,4)]
```

### 8. Big integers `{bigint}`

Implement a type for big integers (like `Integer`) that would be an instance of `Num` – that is, you'll have to write `(+)`, `(*)`, `abs`, `signum`, `fromInteger`, and either `(-)` or `negate`. It should also have instances of `Eq` and `Ord`. (The instance of `Show` can be autoderived to make debugging easier.)

Implementing simple algorithms is fine, you don't have to do a [Karatsuba](https://en.wikipedia.org/wiki/Karatsuba_algorithm).

Test your implementation with [hspec](http://hspec.github.io/) and [QuickCheck](https://hackage.haskell.org/package/QuickCheck). (You might want to create a `cabal`/`stack` project for that, but it's not necessary.)

### 9. Biased shuffle `{shuffle}`

An easy way to shuffle an array is to do the following:

```
n = array.length
for i in 0..n-1
  array.swap(i, random(0, n-1))   // 'random' is inclusive
```

It's also wrong – some permutations will occur more often than others. Prove this by plotting the probability that the element at position `x` will be at position `y` after the shuffle.

If you use the `gnuplot` package, you should be able to draw a plot like this:

``` haskell
import Graphics.Gnuplot.Simple

main = do
  ...
  let plotAttrs = [Plot3dType ColorMap, CornersToColor Corner1]
  plotFunc3d [] plotAttrs [0..n-1] [0..n-1] $
    \x y -> <probability that array[x] will be at y>
  getLine  -- wait (otherwise the plot window will close)
```

(On Windows, you might have to rename the gnuplot binary to “pgnuplot”.)

(If you want to check yourself, here's a plot I got for n=50 and 100000 trials: <http://imgur.com/9eHtiqJ>. If you want to know more about shuffles, read about the [Fisher–Yates shuffle](https://en.wikipedia.org/wiki/Fisher–Yates_shuffle).)

(You can, if you want, also try implementing another often-used shuffle – a quicksort with the comparison step randomly returning `LT`, `EQ` or `GT` – and checking whether it's biased or not.)

### 10. JSON extractor `{jpath}`

Write a parser (with [megaparsec](https://hackage.haskell.org/package/megaparsec), for instance) for a subset of [JSONPath](http://goessner.net/articles/JsonPath/) and evaluate it against actual JSON (parsed with [aeson](https://hackage.haskell.org/package/aeson)). Assuming some JSON in `store.js`, the end result should work like this:

```
{ "store": {
    "book": [
      { "category": "reference",
        "author": "Nigel Rees",
        "title": "Sayings of the Century",
        "price": 8.95 },
      { "category": "fiction",
        "author": "Evelyn Waugh",
        "title": "Sword of Honour",
        "price": 12.99 } ],
    "bicycle": {
      "color": "red",
      "price": 19.95 } } }
```

```
$ jp "$.store.book[0].title" store.json
"Sayings of the Century"

$ jp "$.store.book[0]" store.json
{
  "category": "reference",
  "author": "Nigel Rees",
  "title": "Sayings of the Century",
  "price": 8.95
}
```

Your parser should support at least `$` (“the root element”), `.<name>` for object access, and `[<number>]` for array access. You can add more JSONPath elements if you want to (such as `..`, `*`, and slices). Expressions are hard, don't bother with them.

You can use [aeson-pretty](https://hackage.haskell.org/package/aeson-pretty) to pretty-print JSON.

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

## Week 4 (September 19 – September 25)

### Draw a spiral `{spiral}`

Given width of a spiral, draw a spiral:

```
Size? 9

*********
        *
******* *
*     * *
* *** * *
* * * * *
* *   * *
* ***** *
*       *
*********
```

### Justify text `{justify}`

Given a string, format it to fit N character lines and justify text inside.

Input:

```
65
It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness...
```

Output:

```
It was the best of  times, it was the worst  of times, it was the
age of wisdom, it was the age of foolishness, it was the epoch of
belief, it was  the epoch of  incredulity, it  was the  season of
Light, it was the season of Darkness...
```

To justify text, you can simply make some spaces double (or triple if needed, etc). For bonus points, try to choose positions of spaces nicely (e.g. in the first line there are 14 spaces, and 2 of them have to be double if we want to make the line be 65 characters long; we could choose first and second space for that, but instead we chose 5th and 10th so that the lengths of word groups would be approximately equal).

Don't justify the last line or it'll look really ugly.

### Trie `{trie}`

Construct a trie from all words in a dictionary and implement search for words by prefix. Here's an example of a trie for `{cool, cat, coal, bet, bean}`:

```
    b       c
   /       / \
  e       a   o
 / \     /   / \
t   a   t   a   o
    |       |   |
    n       l   l
```

You should read the words file, construct a trie, say how many nodes are in the trie (e.g. in the sample one there are 13 nodes), and then answer user's queries to find all words starting with given letters:

```
Trie created. There are 13 nodes.

> be
bean bet

> c
cat coal cool

> co
coal cool
```

You can use the following type for the trie (but feel free to use something else):

```
data Trie a = Empty | Node (Map a (Trie a))
```

The list of words in available in the `data/` folder in the repository.

### Path finding `{path}`

Read a grid with obstacles, a starting point, and an ending point from a file. Here's a sample grid (obstacles are denoted with `#`, starting point with `A`, ending point with `B`):

```
A.........#.....#....
...####...#.....#....
...#........#####....
...#..#.....#.......#
...####.##......#....
#.......#............
...#........#.....#..
......#..............
...#.......#B..#.....
.....#..####.........
#..........###.......
```

Find the shortest path from A to B and output the grid with the path drawn (or say that there's no path if there's no path):

```
A+++++++..#.....#....
...####+..#.....#....
...#...+....#####....
...#..#+....#........
...####+##......#...#
#......+#............
...#...+....#.....#..
......#++++++........
...#.......#B..#.....
.....#..####.........
#..........###.......
```

You can use [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).

### JSON printing `{json-print}`

Define a data type for JSON and print it as JSON (without indentation). Don't forget that you should support floating-point numbers and escaping in strings.

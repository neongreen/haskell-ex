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

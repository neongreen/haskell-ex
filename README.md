# Haskell exercises

## Prelude

These are exercises for the Alpha study group (if you want to participate, first get into the [Haskell Learning Group](https://github.com/haskell-learning-group/haskell-learning-group) and then ask @neongreen). You have a week to solve each set. Cheating will be punishable by spiders.

To submit a solution, create a folder named `<exercise code>/<your Github nick>`. It doesn't matter how to call the `.hs` file, but `Main.hs` is a good choice. If you have commit access – and you should – forking the repository is *not* needed. If you can't do `git push`, it might be because others have changed the repository in the meantime; doing `git pull --rebase` should fix it.

Don't forget to use [hlint](https://github.com/ndmitchell/hlint) on your code – it often gives good suggestions on how to improve it. (They aren't *always* good, however! If you're unsure, ask.)

You can see yours (and others') progress in [this table](https://docs.google.com/spreadsheets/d/1PEF7K42M-cq1XgiAaqwf-XLeJP2wo3Dc8pU3SsD_R8s/edit?usp=sharing).

## Week 1 (August 26 – September 5)

### 1. Find scary words `{scary}`

If you assign numbers to letters (A=1, B=2, ..., Z=26), then a word is scary if the sum of its letters is 13. “baaed”, for instance, is scary (especially when at first you don't understand it's a silly verb and think it's an ancient god's name).

Find all scary words in the `words` file (it's usually in `/usr/share/dict/words` or `/usr/dict/words`). If you're on Windows, you can [download it](https://raw.githubusercontent.com/eneko/data-repository/master/data/words.txt).

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

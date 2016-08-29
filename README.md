# Haskell exercises

## Prelude

These are exercises for the Alpha study group (if you want to participate, first get into the [Haskell Learning Group](https://github.com/haskell-learning-group/haskell-learning-group) and then ask @neongreen). You have a week to solve each set. Cheating will be punishable by spiders.

To submit a solution, create a folder named `<exercise code>/<your Github nick>`. It doesn't matter how to call the `.hs` file, but `Main.hs` is a good choice.

Don't forget to use [hlint](https://github.com/ndmitchell/hlint) on your code – it often gives good suggestions on how to improve it. (They aren't *always* good, however! If you're unsure, ask.)

You can see yours (and others') progress in [this table](https://docs.google.com/spreadsheets/d/1PEF7K42M-cq1XgiAaqwf-XLeJP2wo3Dc8pU3SsD_R8s/edit?usp=sharing).

## Week 1 (August 26 – September 5)

### 1. Find scary words {`scary`}

If you assign numbers to letters (A=1, B=2, ..., Z=26), then a word is scary if the sum of its letters is 13. “baaed”, for instance, is scary (especially when at first you don't understand it's a silly verb and think it's an ancient god's name).

Find all scary words in the `words` file (it's usually in `/usr/share/dict/words` or `/usr/dict/words`). If you're on Windows, you can [download it](https://raw.githubusercontent.com/eneko/data-repository/master/data/words.txt).

### 2. Calculate probability of winning using simulation {`reposts`}

There's a contest going on in a Russian social network: seven prizes will be given to seven randomly chosen people among those who have reposted a certain post. (There are actually 100 prizes, but the other 93 suck, so we'll ignore them.) There are already ~1000000 reposts. My sister wonders: what's the probability of her winning at least one prize (out of those seven) if she reposts the post 10 times (from different accounts)? What about 100 times? 1000 times?

Calculate the answer by running a simulation some number of times (for instance, 10000 times). You can use [`System.Random`](https://hackage.haskell.org/package/random/docs/System-Random.html) or some other random library (e.g. [`Data.Random`](https://hackage.haskell.org/package/random-fu/docs/Data-Random.html)).

A code-free spoiler: if you're not good at probabilistic simulations, <a href="#" title="You can assume that sister's reposts have numbers 1–N, then then generate 7 random numbers between 1 and 1000000+N and check that at least one of them is between 1 and N. Make sure that generated numbers aren't equal."> here's one way it could be done</a> (answer in the link tooltip).

### 3. Write a tic-tac-toe game {`tictactoe`}

Here's a sample log that the player should see (Github might be rendering box characters weirdly but they will look okay in terminal):

~~~
Who goes first? (human/computer)?
> h

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

Use [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal) to:

* Color computer's chips red and human's – green.
* Make the winning sequence stand out when the game ends (by changing the background color instead of the foreground color).

<img src="http://i.imgur.com/GsJ3yP6.png" width="365">

### 4. Generate a maze using Wilson's algorithm {`wilson`}

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

### 5. Solve a logic problem using brute-force {`logic-brute`}

Two integer numbers A and B are picked, so that A ≥ B and both numbers are within the range [2, 99]. We tell Mr.P their product (A×B) and Mr.S – their sum (A+B). The following dialog takes place:

P: I don't know the numbers.  
S: I knew you didn't know. I don't know either.  
P: Now I know the numbers.  
S: Now I know them too.  

Find A and B.

<a href="#" title="“I knew you didn't know” means that in every possible world consistent with what S knows about the numbers, there are several possible worlds for P. For instance, let's say that the numbers are 4 and 4. The possible worlds for S are (6,2), (5,3), and (4,4). In the case of (6,2) P wouldn't know the numbers, because 12 (i.e. the only thing that P knows) can mean both 6×2 and 4×3. However, in the case of (5,3) P would know the numbers (because only (5,3) gives 15). Hence, S can't be sure that P doesn't know the numbers. Therefore, the numbers aren't (4,4). By applying bruteforce to all possible pairs, and modelling knowledge, the problem can be solved.">A hint is in this link tooltip.</a>

For more info on the topic, see Oleg Kiselyov's [“Representing knowledge about knowledge”](http://okmij.org/ftp/Algorithms.html#mr-s-p).

## Week 4 (September 19 – September 25)

### 16. Draw a spiral `{spiral}`

Given width of a spiral, draw a spiral (the height will be n+1):

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

You can also make the spiral look a bit nicer by adding spaces, but it's optional:

```
* * * * * * * * *
                *
* * * * * * *   *
*           *   *
*   * * *   *   *
*   *   *   *   *
*   *       *   *
*   * * * * *   *
*               *
* * * * * * * * *
```

### 17. Justify text `{justify}`

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

### 18. Trie `{trie}`

Construct a [trie](https://en.wikipedia.org/wiki/Trie) from all words in a dictionary and implement search for words by prefix. Here's an example of a trie for `{cool, cat, coal, bet, bean}`:

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
data Trie a = Empty | Node Bool (Map a (Trie a))
```

The `Bool` parameter is needed to store words that are prefixes of other words. E.g. `{a, an, be}` will be represented like this (in pseudocode):

```
Node False {
  'a': Node True {
    'n': Empty }
  'b': Node False {
    'e': Empty } }
```

The `True` at `'a'` signals that “a” is a word in the trie; the `False` at `'b'` signals that “b” isn't a word in the tree.

The list of words in available in the `data/` folder in the repository.

### 19. Path finding `{path}`

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

You can use [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) or the [flood fill algorithm](https://en.wikipedia.org/wiki/Flood_fill).

### 20. JSON printing `{json-print}`

Define a data type for JSON and print it as JSON (without indentation). Don't forget that you should support floating-point numbers and escaping in strings.

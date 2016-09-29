{- README
-- Trie

Construct a trie of all words in a dictionary.
Use it for searching words by prefix.

For example, a trie for words: cool, cat, coal, bet, bean.

        b       c
       /       / \
      e       a   o
     / \     /   / \
    t   a   t   a   o
        |       |   |
        n       l   l
-}

{- Trie type.

In our model it might at first seem that one word coresponds to one path within a trie of letters, but that would be wrong; Paths can actually naturally represent multiple words since words can also be the prefix of other words. For example:

     a and
    be bean

As a visualized trie:

      a       b
     /         \
    n           e
                 \
                  a
                  |
                  n

So our solution is to model each Node with a flag indicating whether it represents a valid word or not at that point in the path. For example, the above visualization in pseudo code:

    Node False
      a : Node True
        n : Empty
      b : Node False
        e : Node True
          a : Node False
            n : Empty

Finally each letter (a Node) may branch out to zero, one, or many other letters (nodes) which themselves finish or continue toward another word. For example:

        b
       /
      e
     /|\
    t e  a
      |  |
      r  n

Our solution is to use a Map data type which permits us to model arbitrary number of children per node.
-}

data Trie a =
  Empty |
  Node Bool (Map a (Trie a))
  deriving (Show)

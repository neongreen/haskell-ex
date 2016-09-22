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

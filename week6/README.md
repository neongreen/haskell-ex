## Week 6 (October 11 – October 16)

### 23. Write QuickCheck `{quickcheck}`

You need to write a function called `check` that would generate arbitrary testcases, pass them to a property, and print the first testcase for which the property has failed:

```haskell
allNumbersDivideEachOther :: (Int, Int) -> Bool
allNumbersDivideEachOther (a, b) = a `mod` b == 0
```

```haskell
> check allNumbersDivideEachOther
False! Testcase: (5,3)
```

You'll need to write your own class (e.g. `Arbitrary`) for generating random testcases for various types.

Once you have an initial implementation, here are ideas on how to improve it:

* Count exceptions as test failures too.

* Add shrinking of testcases by adding a function called `shrink` to the typeclass, which would attempt to generate “smaller” testcases from a testcase. (For instance, it might decrease the number for `Int`, or remove some elements from `[Bool]`.) Testcases like `(3,2)` are usually nicer to investigate than `(13426634,234623)`.

* Handle functions with several parameters (in the same `check` function). You'll need to write another typeclass for that, `Testable`.

### 24. Choosing serialization method `{serialize}`

Let's say you've got a huge `Tree` (from `Data.Tree`) and you want to write it to a file. To do that, you first serialize it (i.e. convert to a `ByteString`). There are lots of ways to do that – you can `show` it and then convert the result to UTF-8, you can convert it to JSON with Aeson, you can use `binary` or `cereal` (or even `binary-serialise-cbor`), MessagePack, etc. Which of those is the fastest? Investigate by benchmarking various methods with [criterion](https://hackage.haskell.org/package/criterion). (It can generate nice HTML reports and you're advised to look at them – but also do include text results in comments.) You can benchmark deserialization as well, but it's not mandatory. Of course, it might turn out that different libraries are better for different trees – if your benchmarks discover that, it would be even better.

(Don't forget that performance-critical code should be compiled with `-O2`. Also, don't accidentally benchmark writing into the file itself.)

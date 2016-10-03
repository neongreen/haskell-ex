# Haskell exercises

## What is it

These are exercises for the Alpha study group (if you want to participate, first get into the [Haskell Learning Group](https://github.com/haskell-learning-group/haskell-learning-group) and then ask @neongreen). You have a week to solve each set. Cheating will be punishable by spiders.

The exercises are beginner-to-intermediate level. Expect to learn how to write algorithms, solve simple problems with Haskell, use common libraries, write sites, talk to databases, create interfaces, parse things, do weird type-level stuff, and more.

## Workflow

### Submitting Solutions

1. **Setup your namespace**

  For instance, if your Github handle is **john** and you are solving task **table** from **week 3**, create a branch called `table/john` and put your solution into `week3/table/john`:

  ```
  $ git checkout -b table/john
  $ mkdir -p week3/table/john
  ```

  Recap:
  1. Branch `<exercise-code>/<your-gh-handle>`
  2. Folder `week<n>/<exercise-code>/<your-gh-handle>`

1. **Author your solution, get feedback**

    Work in your namespace. It doesn't matter what you call your `.hs` file, but `Main.hs` is a good default.

    Create a pull request once you want feedback on your code and/or are ready to submit it.

    * _Suggestion_ If you are not done then consider writing a [task list](https://github.com/blog/1375-task-lists-in-gfm-issues-pulls-comments) in the description. This convention transparently communicates your progress.

    * _Suggestion_ Feedback will probably result in additional tasks to do so updating the list may be desirable, but then again certain tasks may not be worth such detail. Use judgement, whatever helps.

1. **Iterate**

    Discuss feedback with the reviewer. Integrate changes into your solution. Repeat as needed.

1. **Finish**

    Once you and your reviewer are satisfied with your solution, merge! From then on you can make changes to your solution's code in the `master` branch (refactors, etc.).

### Weekly Review

At the end of each week each exercise is explained by someone who has solved it.

### Stuff that you should know if you're already in

* Don't forget to use [hlint](https://github.com/ndmitchell/hlint) on your code – it often gives good suggestions on how to improve it. (They aren't *always* good, however! If you're unsure, ask.)

* You can see yours (and others') progress in [this table](https://docs.google.com/spreadsheets/d/1PEF7K42M-cq1XgiAaqwf-XLeJP2wo3Dc8pU3SsD_R8s/edit?usp=sharing).

## Exercises

* [**Week 5 (October 3–9)**](week5)

    * 21. Write a quine `{quine}`
    * 22. Write a database engine `{db}`

* [Week 4 (September 19 – October 2)](week4)

    * 16. Draw a spiral `{spiral}`
    * 17. Justify text `{justify}`
    * 18. Trie `{trie}`
    * 19. Path finding `{path}`
    * 20. JSON printing `{json-print}`

* [Week 3 (September 12–18)](week3)

    * 11. Binary conversion `{binary}`
    * 12. Working with expressions `{expr}`
    * 13. Compute a moving average `{average}`
    * 14. XOR encryption `{xor}`
    * 15. Table formatting `{table}`

* [Week 2 (September 5–11)](week2)

    * 6. Merge sort `{mergesort}`
    * 7. Silly compression `{compress}`
    * 8. Big integers `{bigint}`
    * 9. Biased shuffle `{shuffle}`
    * 10. JSON extractor `{jpath}`

* [Week 1 (August 26 – September 4)](week1)

    * 1. Find scary words `{scary}`
    * 2. Calculate probability of winning using simulation `{reposts}`
    * 3. Write a tic-tac-toe game `{tictactoe}`
    * 4. Generate a maze using Wilson's algorithm `{wilson}`
    * 5. Solve a logic problem using brute-force `{logic-brute}`

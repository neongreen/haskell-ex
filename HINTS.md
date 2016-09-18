# Hints

## reposts

You can assume that sister's reposts have numbers 1–N, then then generate 7 random numbers between 1 and 1000000+N and check that at least one of them is between 1 and N. Make sure that generated numbers aren't equal.

## logic-brute

“I knew you didn't know” means that in every possible world consistent with what S knows about the numbers, there are several possible worlds for P. For instance, let's say that the numbers are 4 and 4. The possible worlds for S are (A=6, B=2), (A=5, B=3), and (A=4, B=4). In the case of (6,2) P wouldn't know the numbers, because 12 (i.e. the only thing that P knows) can mean both 6×2 and 4×3. However, in the case of (5,3) P would know the numbers (because only (5,3) gives 15). Hence, S can't be sure that P doesn't know the numbers. Therefore, the numbers aren't (4,4).

The problem can be solved in this fashion by checking all possible pairs.

A tool for calculating probabilities of winning in the board game Risk. Assume that fights are until either the defender is eliminated, or attacker is left with 1 army, and that all players roll the maximum number of dice possible.

# Usage

`printrisk(atk,def)` where `atk` is the attacker army size, and `def` is the defender army size. 

For example, 5 vs. 4:

```
-4      0.13115846
-3      0.1438941
-2      0.16578466
-1      8.263191e-2
0       0.0
1       0.0
2       5.902279e-2
3       0.11472143
4       0.16465819
5       0.13812847
win     0.4765309
lose    0.5234691
```

* Negatives mean that defender wins with that many armies remaining, and attacker has 1 army left.
* Positives mean that the attacker wins with that many armies left, INCLUDING the army that stays behind. (This is why 0 and 1 will not occur.)

# Implementation details

Probabilities of individual battle outcomes are hardcoded in `probs`.

Uses dynamic programming, done "automatically" in the `Memo` monad.

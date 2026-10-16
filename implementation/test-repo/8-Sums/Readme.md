# 8. Sums

In stream `c` the specification sums up all values from input stream `i`.
In stream `b` the values are only summed up, if input stream `sep` was true at the moment they arrived.
In stream `c` the values are only summed up, if input stream `sep` was false at the moment they arrived.

Finally, the specification calculates `c - (a + b)` which theoretically is always 0.

While the range approach is not able to calculate the exact value 0 for `error`, the symbolic approach is until the first pruning after `sep` was ?.
However, the symbolic approach is still able to produce a better interval for `error` than the range approach.

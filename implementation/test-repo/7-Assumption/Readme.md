# 7. Assumptions

The specification counts the number of occurrences of 42 on the input stream `i`.
Furthermore, there is a second input stream `time` which carries the current wall-time.

We add a stream `ASSUMPTION`, encoding our assumption about the system that 42 on `i` can only appear at least 5 time units apart from another.
An assumption is always known to be true.

The range approach is not able to handle assumptions.
The resulting final event of stream count contains uncertainty: `[5.00,7.00]`

The symbolic approach is able to deal with the assumption:

When the input `?,42` arrives, the symbolic approach can deduce that the timestamp of this 42 event must be in `[40.00,?]` because the last 42 appeared at time 35.
As a consequence, when the next input `43,?` appears the system can deduce that the `?` cannot be a 42.
Finally, the symbolic approach returns the correct, certain value for the number of 42s: 5.0

# 9. Simple Future

In stream `a` the spec adds the input to the following value of `a` with 0 being its value at the trace end.
Stream `b` does the same but the input is doubled beforehand.
Stream `c` then calculates the difference between the double of `a` and `b`.
Furthermore, the assumptions forces every input to be non-negative.

Without calculating the future, every output would always be `?` since the future values of `a` and `b` are unknown then.

By calculating future symbolically, however, the tool can deduce that `b` is always double the value of `a` and therefore outputs `0.00` for `c` at every time step.
Additionally, since all inputs are known to be non-negative, the tool can also deduce that for `a` and `b` the future inputs can only increase their values, leading to a known lower bound for these streams.

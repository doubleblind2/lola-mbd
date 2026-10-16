# 10. Enums and Sets

The specification defines two new enum types `Ex1` and `Ex2`, each with its set of possible values.
There are two input streams, one for each enum and an output stream `a` of type `Ex1` that outputs the value of `i1` if `i2` was `X` and `C` otherwise.

The tool is able to handle uncertainty also for enums.
When the input is `A,?`, it is able to deduce that the output can be either `A` (for `i2=X`) or `C` (otherwise).
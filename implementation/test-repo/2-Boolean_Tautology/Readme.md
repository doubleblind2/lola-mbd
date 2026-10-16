# 2. Boolean tautology exampe

Simple example with just booleans.
One stream checks if there once was a true on the input stream, and the other one if there always is a false on the input stream.
A further stream `c` is the conjunction of the two aforementioned streams and hence always false.

The range approach is not able to detect that `c` is always false; however, the symbolic approach is.

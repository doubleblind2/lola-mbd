# 14. Internals and Sampling

The specification has two input streams `i1` and `i2`.
The goal is to output every integer from -10 to 10 between the two numbers.
This is realized by defining an internal stream implicitly defined via the assumption to be between the two inputs and sampling the output from -10 to 10 in increments of 1.
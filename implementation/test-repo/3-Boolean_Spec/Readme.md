# 3. Boolean specification example

Simple example with just booleans.
It checks (in stream `c`) if input stream `x` was true at least once in the last two timestamps or if the current value of input stream `y` is equal to the last value of input stream `x`.


At the 9th output event, the symbolic approach is able to detect that c is true

```
?,?,true
```

while the ranges approach is not

```
?,?,?
```

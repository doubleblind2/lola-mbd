# 5. Sliding window

The last 5 values of input stream `input` are summed up in stream `last5Values`.
The current value of `last5Values` is then compared to the current value of input stream `threshold`.

The symbolic approach is able to maintain more precise intervals and is, hence, able to give certain outputs for the threshold check, where the range approach is not able to.

E.g. the 14th output:

```
[18.00,23.00],false
```

vs.

```
[13.00,28.00],?
```

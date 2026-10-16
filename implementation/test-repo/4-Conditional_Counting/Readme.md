# 4. Conditional Counting

The value of the stream `input` is aggregated either on stream `sumA` if input stream `isA` is true, and otherwise on stream `sumB`.
Stream `equal` determines whether `sumA` and `sumB` are currently equal.

At the 4th output event, the symbolic approach is able to detect that `equal` is false.

```
[5.00,10.00],[5.00,10.00],false
```

The range approach fails in this situation:

```
[5.00,10.00],[5.00,10.00],?
```

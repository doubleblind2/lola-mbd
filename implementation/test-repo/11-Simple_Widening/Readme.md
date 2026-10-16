# 11. Simple Widening

The specification defines an input stream `x` that is constrained by the assumption to be between 1 and 10.
The output stream `a` sums all `x` from the back and `nextA` contains the value of `a` at the subsequent instant.

Without widening the monitor would not be able to compute the initial semantics of the specification because the value of `a` grows by a value between 1 and 10 each time step, resulting in a unique state for each time step.

The run script of this example however enables the tool to apply widening (--force-widening flag) if after 20 steps of computation no initial fixed point is found, i.e. no repeating state was found.
Therefore, every state after the 21st (counted from the end) is described by the same state, where the value of a is approximated by the interval [22,?].

In this particular example, this means that during forward calculation for +1-offsets `a` is always known to be between `[22,?]`. The minimum of 22 is known due to `x` being at least 1 but the maximum can become arbitrarily large.

With widening the sequence of possible values for `a` beginning from the end (`n` is the trace length) looks like:

```
ts: n-1     n-2         n-22      n-23    n-24         0
a:  [1,10], [2,20], ...,[22,220], [22,?], [22,?], ..., [22,?]
```

Without widening the sequence would continue in the same manner as it started and no stabilizing state would be found.
In case of unknown trace length, this would be impossible.

During the actual monitoring, the inputs `x` are then simply added to the next `a`.

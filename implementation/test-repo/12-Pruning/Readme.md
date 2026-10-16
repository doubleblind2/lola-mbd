# 12. Simple Pruning Demonstration

The specification simply adds all inputs from stream `x` to stream `a`, additionally assuming that `x` is never negative.
The example input consists of 250 inputs all consisting of the uncertain interval `[-5,5]`.
When monitored symbolically, a new symbol gets introduced for every input and since `a` depends on all previous inputs, they can not be eliminated through simple cleanup, that is, quantifier elimination.
This leads to increasing runtime and space as the monitoring continues (see `outputs/runtime_no_pruning.log`).
However, using a pruning strategy allows to rewrite the expression, such that instead of keeping track of every `x^t` being between 0 and 5 (0 because of the assumption), a new symbol gets generated that is restricted by the sum all inputs up to that point.
Since this information is sufficient to determine the current output, the input symbols become irrelevant.
This leads to much faster (and, if run long enough, actually constant) runtimes; see `outputs/runtime_pruning.log` (the spikes are the instants where pruning occurs).

Note that the range evaluator cannot be applied because of the existence of an assumption.

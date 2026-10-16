# 13. Boolean Future

This example very briefly demonstrates the BooleanFutureEvaluator using an automaton approach to anticipatorily monitor boolean specifications.
The specification defines two input streams `p` and `q` and outputs whether Finally(q), written `fq`, holds.
An assumption provides the background knowledge that `q` must hold five instants after `p` held.

In the second instant it can be concluded that `fq` is `true` because `p` is `true` now and therefore `q` must be `true` five instants later.
# 6. Peak Detection

This example uses the macro syntax to define three streams

`s1`: Holds the value of the last event from input stream `i`
`s2`: Holds the value from `s1` plus the event from `i` two timestamps ago
`s3`: Holds the value from `s2` plus the event from `i` three timestamps ago

The specification then checks if `s2` has the greatest value of these three.

For the given input the symbolic approach is always able to maintain a certain result, while the ranges approach often outputs uncertainty.

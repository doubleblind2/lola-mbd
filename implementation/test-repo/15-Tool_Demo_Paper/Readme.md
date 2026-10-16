# 15. Example from the Tool Demo Paper

The specification defines an input stream `x` that is summed up on stream `y` with a sliding window of size 3.
This is achieved by adding the current value of `x` and subtracting the one three steps before.
Stream `ok` then checks whether the current sum is at most 30.
Using anticipation, stream `stays_ok` checks whether this is the case and in the future, essentially recurrently computing LTL's Globally(ok). 
The assumption restricts the current value of `x` to, firstly, differ from its successor by at most 5 and, secondly, always be between 0 and 20.

Additionally, a stream `nextX` provides the possible values that will not lead to `stays_ok` definitely becoming false.
This is realized using the `linked with` syntax leading to only output values for the subsequent `x` under the temporary assumption that `stays_ok` is `true` and, therefore, `ok` being `true` in every future instant as well.

Note that `stays_ok` cannot be `true` since it is always possible that the stream becomes `false` in the future (except near the trace end).

When reading the first input 10, while the assumption alone would allow the next input to be within 5 and 15, the monitor tells us that any value above 12.5 will definitely lead to `ok` becoming false in the future.
This is the case even though the next sum is definitely below 30 because any value we could input after that would result in a sum above 30.
If the next input is then exactly 12.5, the monitor tells us that only 7.5 is a valid value for the third input (anything below is restricted by the assumption, anything above leads to `ok` becoming false in the future).

At the 8th instant when 13 is read, the interval given before was exceeded and, as expected, `stays_ok` becomes `false` even though `ok` is still fine.
Now, there is no input reducing the sum back to 30 (the smallest possible input is 8) and, thus, `INVALID` is given for `nextX`.
After two more inputs the sum is below 30 again and there are again possible values for the future.
In the 11th instant, an uncertain input `[10,15]` is inputted which makes it uncertain whether the sum is above 30 or not.
However, no matter how the input actually looks like, the monitor is still able to give a valid range for the next input.
Three instants later, the uncertainty got filtered out again.

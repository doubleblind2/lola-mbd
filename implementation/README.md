# UnAA-LOLA - Implementation of LOLA for handling uncertainty, assumptions and anticipation

## General

This project is a Scala implementation of a LOLA engine dealing with uncertainty, assumptions and anticipation.

There are four main modes to run the engine

- Certain: Just deal with certain values
- Ranges: Use value range / interval abstraction for evaluation of uncertainty
- Boolean automaton: Use an automaton-based approach for dealing with uncertainty within boolean specification
- Symbolic: Fully symbolic implementation of the theory using z3 as solver backend 

There is also a legacy past-only symbolic implementation (z3_past_only) that is a bit faster than the full symbolic one but does not support anticipation and many of the advanced features.

## Build

The project is available as sbt project. To build it install `sbt` and run

```
sbt assembly
```

Find the executable binary in `target/scala-2.13/unaa-lola-0.3.jar`.

Tested with sbt version 1.9.7; versions up to 1.12.12 should work as well.

## Dependencies / Libraries

- ANTLR4
- Scopt
- Z3
- Breeze

## Usage

### General usage

```
Usage: java -jar unaa-lola.jar LolaSourcePath Evaluator [options]

  LolaSourcePath           Path to the LOLA specification file.
  Evaluator                Use one of {boolean_automaton, certain, range, z3, z3_past_only} to either allow only fully certain inputs, uncertainties handled by interval arithmetic abstraction (future and some other features unsupported), uncertainties handled by a symbolic engine (currently only z3) or uncertainties handled by an automaton (supports only boolean specs).
  --csv-in                 Consume events in CSV format.
  --csv-out                Output events in CSV format.
  -v, --verbose            Be more verbose.
  -d, --debug              Add debug information to the main log.
  --log-file               Create file techLog.log instead of logging on std.err.
  --no-logging             Do not log at all (aside from runtimes) and print only event outputs.
  --log-runtime            Create file runtime.log with runtimes per event, one per line.
  -i, --inputs <value>     Number of input lines (time stamps), unknown if unset.
  --estimation-steps <value>
                           Abandon future estimation if taking longer than this.
  --z3-opt-queries-off     Switch z3 optimizer off (may lead to worse results but faster runtime).
  --z3-runtime             Collect query calls to z3 separately from total runtime. Only works in conjunction with --log-runtime.
  --disable-future         Disable anticipation, i.e. use maximally abstracted values for positive offsets.
  -o, --opt-interval <value>
                           How often optimization (including pruning) takes place during forward calculation, defaults to no optimization.
  -w, --force-widening <value>
                           Forces widening after this many steps during backwards calculation. Defaults to no widening.

```

### Specification format

On a basic level specifications follow the following syntax:

```
in <streamname>: <type>;
[...]
def <streamname>: <type> = <LOLA-term>;
[...]
out <streamname>;
```
- `in` defines input streams, i.e., streams whose values are provided by the user
- `def` defines defined streams (in the theory called output streams), i.e., streams that are defined via an expression `LOLA-term`
- `out` declares which streams (input and defined) will be outputted back to the user
- Supported `<type>`s are `Bool`, `Real` and Enums (see below)
- `<LOLA-term>`s are the following

    - `<streamname>` for referencing the current event of a stream (must begin with a lowercase letter)
    - `<streamname>[offset, default]` for an offset operation, i.e., referencing a stream at a time offset by `offset`; if that time point does not exist, the constant `default` will be returned instead
    - `<expression><op><expression>` for `<op>`: ||, &&, ==, !=, <=, >=, >, <, <->, ->, +, -, * (see note below for *)
    - `!<bool-expression>`
    - `if <bool-expression> then <expression> else <expression>`
    - Real literals, `true`, `false` and enum constants.
    - `(<expression>)`

Stream definitions can also be split into two statements:

```
def <streamname>: <type>;
[...]
<streamname> = <LOLA-term>;
```
This can be necessary when two streams reference each other to ensure that a stream's type is known before being used in an expression.

The assumption stream is identified by a special bool stream `ASSUMPTION`. Assumptions and positive offsets are not supported by the certain and range evaluators (the legacy symbolic implementation does support assumptions but only with negative offsets).

See folder `test-repo/` for documented ready-to-use examples.

Note that for real-valued streams the LOLA-term must be a linear arithmetic expression (with the addition of if-then-else expressions), meaning that a subexpression involving a stream can only be multiplied (*) with a constant.
(The reason for this is that otherwise our pruning strategies are not applicable.)

#### Internal Streams (full symbolic only)

```
internal <streamname>: <type>
```

defines an internal stream. Values of internal streams are neither defined through inputs nor by expressions, but they can be constrained by the assumption. Internally, they are treated as input streams but without actually reading inputs.

Example:

```
in i1: Real;
in i2: Real;

internal a: Real;
def ASSUMPTION: Bool = i1 <= a && a <= i2;

out a;
```

Here, for `i1 = 5` and `i2 = 10` the output is `a = [5,10]`.

#### Enumerations

```
enum <enumname> {<enumconst1>, <enumconst2>, <...>};
```

defines a new enum type `<enumname>` with its constants. This type can then be used in the same manner as bools and reals.

Example:

```
enum Ex {A, B, C, D};

in x: Ex;

def a: Bool = x == B;
out a;
```

(For the time being, enum types must begin with an uppercase letter followed by a lowercase letter and enum constants must not contain lowercase letters.)

#### Macro syntax

```
@<from>:<to>@
<code>
@
```

adds `<code>` `to-from` times to the specification.

If `code` contains `{<offset>}` this expression is replaced by `i+offset` where `i` is the counter from `from` to `to` that indicates the number of the current unrolling.

Example:

```
@2:4@
def s{0} : Real;
s{0} = s{-1} + i[-{0},0];
@
```

is equivalent to the following specification

```
def s2 : Real;
s2 = s1 + i[-2,0];
def s3 : Real;
s3 = s2 + i[-3,0];
def s4 : Real;
s4 = s3 + i[-4,0];
```

See `test-repo/6-Peak_Detection`

#### Sampling Reals (full symbolic only)

```
out a from <from> to <to> by <by>.
```

specifies that a stream `a` of type `Real` shall be sampled upon output. A set of reals instead of an interval will be outpup containing all possible values between `<from>` and `<to>` spaced by `<by>`.
In the following example

```
in x: Real;
def a: Real = x + 1;
out a from 0 to 10 by 1.5;
```

if `x` is input as an interval `[3,30]` (i.e., an uncertain value between 3 and 30), instead of outputting `[4,31]` this will output `{4.5, 6.0, 7.5, 9.0}`.

#### Linking real and bool streams (full symbolic only)

```
<streamname> linked with <streamname>;
```
links a real (lhs stream) and a boolean (rhs stream) stream. 
This affects the output of the corresponding real stream. The tool outputs all possible values for the real stream under the premises that the boolean rhs stream is true.

E.g. for the specification

```
in x: Real;

def a: Bool = x - 5 >= 10;
x linked with a;

out x;
```

if the monitor receives the input `x=[3,30]` (i.e., an uncertain value between 3 and 30) the output for stream `x` would be `x = [15,30]`.

If there is no possible value for the real stream that is valid under the assumption that the boolean stream holds, `INVALID` will be outputted.

Additionally, if widening is performed during the tool execution, the widening of the real stream is performed separately for the case that the boolean stream is true and that the boolean stream is false.

#### Model-based Diagnosis

With the corresponding flags `singleDiag`, `multiDiag`, `temporalDiag` diagnosis is enabled.
The `keep` option determines how far into the past component states are considered.
In the specification a component stream can be declared via `comp <c>;` where `c` is the name of the component.
Outside of diagnosis these are treated as internal Boolean streams.

### Input format

The input/output follows the following syntax (the variant can be chosen by a parameter passed to the program):

#### Variant A: Equational (default)

```
<streamname>=<value>
```

If all input stream events are passed to the program the computation is performed for this instant.

#### Variant B: CSV (when `--csv-in` enabled)

At the beginning a single line defining the order:
```
<streamname1>,<streamname2>,...
```

Then per timestamp one line:
```
<value1>,<value2>,...
```

If all input stream events are passed to the program the computation is performed for this instant.

#### Input values

Valid inputs are constants (`true`, `false`, reals and defined enum constants) as well as uncertain values.
Uncertain values vary between types:

- Bool: only `?`
- Real: `?` for full uncertainty, closed intervals `[<from>, <to>]` (use `?` if one side is unbounded, e.g. `[?,5]`) and finite sets of reals `{<r1>, <r2>, <...>}`.
- Enums: `?` for full uncertainty or sets of enum constants `{<e1>, <e2>, <...>}`

### Notes

If `--opt-interval` is not set, no pruning will be applied, i.e., in general the time and space complexity will not be constant per instant.
Setting this option to a number `x` does not necessarily mean that every `x` time steps pruning is applied because the tool may skip a pruning step if the constraint state is still very small.

When positive offsets are involved you should set `--inputs` as the backwards future calculation requires a known trace end as starting point.
The number can be arbitrarily large as long as the backwards calculation terminates beforehand.
To ensure this, either your specification is of a shape that allows finding a repeating state naturally (see `test-repo/9-Simple_Future`) or you have to set `--force-widening` to force the backwards calculation to overapproximate using widening after a certain number of steps without repetition.

By default, no runtimes are logged.
If `--log-runtimes` is set evaluation time (applying spec) and simplification time (cleanup/pruning) per instant are measured separately.
Additionally, set `z3-runtime` to also log the portion of the total runtime that can be attributed to z3.

## DSL / API

See `test-repo/API_example` to see how the DSL and API work.
Note that the sbt build there assumes that you have the tool published locally.
You can do so by running `sbt publishLocal` from the root directory.
This generates a slim jar (i.e., without dependencies) in your `~/.ivy`
The example sbt project can then can be run via `sbt run` from the `test-repo/API_Example` directory.
Alternatively, you can load the fat jar that was precompiled as explained above.
To do so, comment out the resolvers and libraryDepencecies in `test-repo/API_Example/sbt.build` and uncomment the unmanagedJars.
Afterwards simply run the example with `sbt run` from within the example directory.

## Code structure

The class `Frontend` parses the code via ANTLR4 to an `AST` and `Flattener` transforms the spec to a normal form (only -1 or +1 offsets).

A common interface for all evaluators is `EvaluatorInterface`.
It is currently implemented by `CertainEvaluator` (everything is certain and past-only), `ValueRangeEvaluator` (uncertain reasoning only via interval arithmetic and past-only), `BooleanFutureEvaluator` (boolean-only specification uncertain reasoning via automaton) and `SymbolicEngine` (symbolic reasoning). The legacy `PastOnlyZ3Evaluator` also applies symbolic reasoning but to a lesser degree and is implemented less abstractly and with fewer features. The common interface handles the bulk of the input and output logic.

`SymbolicEngine` uses an implementation of `ConstraintState` (currently only `Z3CombiConstraintState`) to handle symbolic expressions. The z3 implementation constructs and manages z3 expressions and additionally contains a `LinAlgCore` that supports the handling of reals by enabling usage of pruning (`Z3LinAlgOptimization`) on linear algebraic formulas (so called `RealAddition`s). Via `Z3CombiQuery` both of these are used to obtain the possible values of streams.

`ConstraintState` handles the general routine for each time step via `ConstraintStateTransformer` and can easily be extended by another implementation (other abstract domain) besides the z3 one.

Future references are handled by `FutureEstimation` which computes the fixed point of a specification and provides values of future time steps.

For further details see the documentation in the source code.


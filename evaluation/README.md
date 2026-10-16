# Lola MBD Evaluation

## Implementation

The implementation of the symbolic LOLA monitor with the diagnosis extension is available in the neighboring folder `../implementation/`.

A precompiled JAR can be found in `bin/`.

## Evaluation Data

The raw data for both circuits is contained in `c17/` and `s27/`.
Each folder consists of the following:

- `specs/` contains the diagnosis specification, the original circuit translated into lola and various mutations of the latter where one component was changed to be stuck-at-false,
- `traces/` contains the input traces, the correct observations (i.e., inputs and outputs under normal system behavior) as well as observations for each mutation,
- `tools/` contains various tools that can be used to extract and aggregate information, such as runtimes or correctness, from the results,
- `results/` contains the results,
- a script `run.sh` to run the experiment, and
- scripts for producing new inputs and their corresponding observations.

For c17, the results are first divided by mutated component and then by `k`.
For s27, the results are first divided by mutated component, then by `k`, and then by trace number.

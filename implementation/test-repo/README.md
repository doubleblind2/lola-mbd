# Artificial examples

This folder contains (artificial) examples to demonstrate the strengths (and also weaknesses) of the symbolic calculation approach.

Every folder contains a specification and a short input trace with uncertainties. Furthermore, there is a short description of every example in the `Readme.md` in the folder.
Finally, there is a `run.sh` in every folder to execute the LOLA monitor for the corresponding specification. 
For the first 8 examples, the shell script always executes both, the symbolic and the ranges approach.
That way, one can compare the difference in precision between simple interval abstraction and symbolic evaluation.
The other examples are to demonstrate some of the advanced (symbolic-only) features.
Outputs, logs and runtimes per event are stored in a subdirectory `output` which is created by the shell script.

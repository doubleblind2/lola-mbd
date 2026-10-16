#!/bin/bash

mkdir -p outputs
java -jar ../../bin/unaa-lola.jar spec.lola z3 --inputs 1000 --debug --csv-in --csv-out --log-file --log-runtime --z3-runtime --opt-interval 10 < sample_input.csv > outputs/output_pruning.csv
mv techLog.log outputs/log_pruning.log
mv runtime.log outputs/runtime_pruning.log
java -jar ../../bin/unaa-lola.jar spec.lola z3 --inputs 1000 --debug --csv-in --csv-out --log-file --log-runtime --z3-runtime < sample_input.csv > outputs/output_no_pruning.csv
mv techLog.log outputs/log_no_pruning.log
mv runtime.log outputs/runtime_no_pruning.log


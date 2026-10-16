#!/bin/bash

mkdir -p outputs
java -jar ../../bin/unaa-lola.jar spec.lola z3 --debug --csv-in --csv-out --log-file --log-runtime --z3-runtime < sample_input.csv > outputs/output_symbolic.csv
mv techLog.log outputs/log_symbolic.log
mv runtime.log outputs/runtime_symbolic.log
java -jar ../../bin/unaa-lola.jar spec.lola range --debug --csv-in --csv-out --log-file --log-runtime < sample_input.csv > outputs/output_ranges.csv
mv techLog.log outputs/log_ranges.log
mv runtime.log outputs/runtime_ranges.log


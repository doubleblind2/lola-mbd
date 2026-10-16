#!/bin/bash

mkdir -p outputs
java -jar ../../bin/unaa-lola.jar spec.lola boolean_automaton --inputs 100 --debug --csv-in --csv-out --log-file --log-runtime < sample_input.csv > outputs/output.csv
mv techLog.log outputs/log.log
mv runtime.log outputs/runtime.log



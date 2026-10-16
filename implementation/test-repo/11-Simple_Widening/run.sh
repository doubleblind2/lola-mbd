#!/bin/bash

mkdir -p outputs
java -jar ../../bin/unaa-lola.jar spec.lola z3 --inputs 30 --debug --csv-in --csv-out --log-file --log-runtime --z3-runtime --force-widening 20 < sample_input.csv > outputs/output.csv
mv techLog.log outputs/log.log
mv runtime.log outputs/runtime.log



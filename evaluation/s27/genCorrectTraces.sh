#!/bin/bash

trap "exit" INT

BASE_DIR="traces"

for t in {1..10}
do
    java -jar ../bin/unaa-lola.jar specs/s27.lola z3 --disable-future --csv-in --csv-out < $BASE_DIR/inputs$t.csv > $BASE_DIR/correct$t.csv
done



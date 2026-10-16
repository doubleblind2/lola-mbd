#!/bin/bash

trap "exit" INT

for c in {1..6}
do
    java -jar ../bin/unaa-lola.jar specs/c17m$c.lola z3 --disable-future --csv-in --csv-out < traces/inputs$t.csv > traces/m$c.csv
done


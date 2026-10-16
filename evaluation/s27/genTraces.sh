#!/bin/bash

trap "exit" INT

COMPS=(5 6 7 9 10 12 15)

BASE_DIR="traces"

for c in "${COMPS[@]}"
do
    echo "c = $c"
    TARGET_DIR="$BASE_DIR/g$c"
    mkdir -p "$TARGET_DIR"

    for t in {1..10}
    do
        java -jar ../bin/unaa-lola.jar specs/s27g$c.lola z3 --disable-future --csv-in --csv-out < $BASE_DIR/inputs$t.csv > $TARGET_DIR/t$t.csv
    done
done


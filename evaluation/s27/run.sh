#!/bin/bash

trap "exit" INT

COMPS=(5 6 7 9 10 12 15)

BASE_DIR="results"

mkdir -p "$BASE_DIR"

for c in "${COMPS[@]}"
do
    echo "c = $c"
    for k in {0..9}
    do
        echo "k = $k"
        for t in {1..10}
        do
            echo "t = $t"

            TARGET_DIR="$BASE_DIR/g$c/$k/$t"
            mkdir -p "$TARGET_DIR"

            java -jar ../bin/unaa-lola.jar specs/s27d.lola z3 --multiDiag --disable-future -k $k --log-runtime --csv-in --checkAtempDiag c$c < traces/g$c/t$t.csv

            mv "diagnoses.log" "runtime.log" "diagnosis_runtime.log" "$TARGET_DIR/"
        done
    done
done

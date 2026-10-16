#!/bin/bash

trap "exit" INT

BASE_DIR="results"

mkdir -p "$BASE_DIR"

for c in {1..6}
do
    echo "c = $c"
    for k in {0..31}
    do
        echo "k = $k"

        TARGET_DIR="$BASE_DIR/$c/$k"
        mkdir -p "$TARGET_DIR"

        java -jar ../bin/unaa-lola.jar specs/c17d.lola z3 --multiDiag --disable-future -k $k --log-runtime --csv-in --checkAtempDiag c$c < traces/m$c.csv

        mv "diagnoses.log" "runtime.log" "diagnosis_runtime.log" "$TARGET_DIR/"
    done
done

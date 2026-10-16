#!/bin/bash

java -jar ../bin/unaa-lola.jar specs/c17.lola z3 --disable-future --csv-in --csv-out < traces/inputs.csv > traces/correct.csv




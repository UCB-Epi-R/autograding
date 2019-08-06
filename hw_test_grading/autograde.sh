#!/bin/bash

sed '1,6d' $1 > hw.R;
Rscript ok.R hw.R;
python3 parse_output.py hw_score.JSON;

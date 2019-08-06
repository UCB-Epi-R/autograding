#!/bin/bash

Rscript install_packages.R;
sed '1,2d' $1 > hw.R;
Rscript ok.R hw.R;
python3 parse_output.py hw_score.JSON;

#!/bin/bash

#Rscript install_packages.R;
pip3 install -r requirements.txt;
mv $1 hw.R;
Rscript -e "library(knitr); stitch('hw.R');"
Rscript ok.R hw.R;
python3 prep_pdf.py;
python3 upload_submission.py;
python3 parse_output.py hw_score.JSON;

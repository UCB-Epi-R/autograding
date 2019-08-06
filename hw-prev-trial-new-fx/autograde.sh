#!/bin/bash

Rscript install_packages.R;
pip3 install -r requirements.txt;
cp $1 hw.R;
Rscript -e "library(knitr); stitch('hw.R');"
Rscript grade_asmt.R hw.R;
python3 prep_pdf.py hw.pdf hw_upload.pdf 15;
python3 upload_submission.py 51507 224157 zah6y_fc4_l_uaW_s3yU1A hw_upload.pdf ;
python3 parse_output.py hw_score.JSON;

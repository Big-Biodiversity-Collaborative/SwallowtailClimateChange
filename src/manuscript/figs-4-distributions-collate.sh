# Paste all the supplemental distribution figures together
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-04-03

# Run this script from the top-level directory of the project. That is, from 
# within the SwallowtailClimageChange folder, in a terminal window, run
# src/manuscript/figs-4-distributions-collate.sh

gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=output/manuscript/Supplemental-Figure-Distributions.pdf \
  output/manuscript/distribution-pages/current-p-*.pdf \
  output/manuscript/distribution-pages/forecast-papilio*.pdf
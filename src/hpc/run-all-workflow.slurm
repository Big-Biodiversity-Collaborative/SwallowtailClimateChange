#!/bin/bash
### All three steps of workflow. Use with caution!!!
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=Full-workflow
#SBATCH --ntasks=8
#SBATCH --nodes=1
### Asking for 20 hours
#SBATCH --time=20:00:00
#SBATCH --partition=standard
#SBATCH --account=jcoliver

### GDAL and PROJ modules necessary for some packages (e.g. rgdal)
module load gdal
module load proj
module load geos
module load R/4.2.2
### Run the script builders and data extraction before submitting job
### src/bash/build-scripts-1-CV.sh
### src/bash/build-scripts-2-CV.sh
### src/bash/build-scripts-3-CV.sh
Rscript src/hpc/prep-extract-data.R
### Rscript src/run-indiv/run-all-1-CV-scripts.R -a -f
### Rscript src/run-indiv/run-all-2-SDMs-full-scripts.R -a -f
### Rscript src/run-indiv/run-all-3-predict-scripts.R -a -f
### New approach goes right to wrappers
Rscript src/run-indiv/run-all-1-CV.R -a -f
Rscript src/run-indiv/run-all-2-SDMs-full.R -a -f
Rscript src/run-indiv/run-all-3-predict.R -a -f

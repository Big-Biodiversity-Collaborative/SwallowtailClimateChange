#!/bin/bash
### Extract data files and make sure dependencies are installed
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=R-library-test
#SBATCH --ntasks=1
#SBATCH --nodes=1
### Asking for 10 minutes
#SBATCH --time=00:10:00
#SBATCH --partition=standard
#SBATCH --account=jcoliver

### GDAL will load PROJ and GEOS modules, too
module load gdal/3.3.2
module load R/4.3
### extraction now handled by prep-extract-data.R
### unzip -uo data/gbif-pa.zip
### unzip -uo data/gbif-shapefiles.zip
Rscript src/hpc/prep-check-libraries.R
Rscript src/hpc/prep-extract-data.R
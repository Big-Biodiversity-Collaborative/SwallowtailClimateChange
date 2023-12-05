# SwallowtailClimateChange
Data and code for North American Swallowtail and larval host plant 
distributions in relation to climate change

**Currently under development**

## General approach:

Retrieves data from online sources (in this case, the Global Biodiversity 
Information Facility, [GBIF](https://gbif.org)) and perform quality control 
processes to ensure observations are only from Canada, Mexico, and the United 
States of America. The data are analyzed to create species distribution models 
based on presence and pseudo-absence data using a variety of models (e.g. 
Maximum Entropy, generalized linear model). The models are then used to predict 
presence or absence under a variety of conditions, including current climate 
and forecast climate models. These predictions are used to estimate change in 
the range sizes of individual butterfly species and the relative size of range 
overlap of their known host plant species.

## Dependencies

The project uses the following additional R packages:

+ dismo
+ dplyr
+ ecospat
+ ENMeval
+ flexsdm
+ gbm
+ ggplot2
+ glmnet
+ kernlab
+ ks
+ Matrix (for lasso models)
+ mgcv
+ parallel (usually part of R distribution, but needs explicit loading)
+ randomForest
+ raster
+ rnaturalearth
+ rnaturalearthdata
+ Rtools (needed to use the zip() function on Windows machines)
+ sp
+ spocc
+ stats (included in base R)
+ stringr
+ terra
+ tidyr
+ tidyterra
+ TNRS

## Workflow

The workflow has the general structure of:

1. Data retrieval and cleaning
2. Preparing R scripts for analyses of individual species
3. Bulk processing of single-species analyses; for each species distribution 
method (boosted regression trees (BRT), generalized additive model (GAM), 
Lasso, maximum entropy (MaxEnt), and random forest (RF)):
    1. Evaluate models using spatial cross validation (CV)
    2. Using best models from CV step, above, re-estimate model parameters 
    using all observational data (no training/testing split)
    3. Predict suitability values for each species, for each of five climate 
    models (one contemporary, four forecast); combine these for an ensemble 
    suitability raster and presence/absence prediction 
    4. Combine predicted consensus distributions of each _insect_ species with 
    the consensus predictions for all of its respective host plants to create a 
    single raster with distributional information (see documentation in 
    functions/overlap_raster.R for interpretations of values in those rasters)
4. Synthesizing results of single-species analyses

### Scripts, in order of use

In descriptions below, \<method\> refers to a character string indicating the 
method used for species distribution modeling, e.g. "glm" for generalized 
linear model and "maxent-notune" for untuned MaxEnt model.

1. Data retrieval and cleaning (in src/data)
   1. **src/data/gbif-1-download.R**: Download observational data from GBIF to 
   the data folder; note by default the data files that are downloaded by this 
   script are _not_ under version control
   2. **src/data/gbif-2-filter.R**: Run quality assurance on downloaded data, 
   and retain only those records that:
       1. are not observations based on barcodes only,
       2. are observations from 2000-2023,
       3. are in locations with climate data (which effectively restricts 
       observations to North America), and
       4. are thinned to a max of X observations per grid cell (of climate 
       raster),
       5. are inside the 98% contour of observations
   3. **src/data/gbif-3-presence-absence.R**: Generate a presence/absence 
   dataset for each species, to be used in any species distribution model; 
   also create a shapefile defining geographical limits of predictions
   4. **src/data/prep-climate-data.R**: Download monthly climate data for time 
   span of interest (2000-2018) and calculate the average values for the 19 
   standard bioclimatic variables (should not need to be run locally; data are 
   available in data/wc2-1 directory); resulting rasters are in 2.5 minute 
   resolution
   5. **src/data/prep-forecast-data.R**: Download monthly climate data for 
   ensemble of forecast climate models and calculate the average values for the 
   19 standard bioclimatic variables (should not need to be run locally; data 
   are available in data/ensemble sub-directories); resulting rasters are in 
   2.5 minute resolution
   6. **src/data/prep-aridity-data.R**: Download measure(s) of aridity and 
   calculate mean and median values for each insect species; data stored as a 
   csv in data/aridity-statistics.csv.
2. Preparing R scripts for analyses of individual species
   1. **src/bash/build-scripts-1-CV.sh**: bash shell script to build an R 
   script for each species to estimate and evaluate each SDM method with 
   spatial folds cross-validation; one script is built for each row (species) 
   in data/gbif-reconcile.csv
   2. **src/bash/build-scripts-2-SDMs-full.sh**: bash shell script to build an 
   R script for each species to estimate model parameters based on _all_ 
   observation data for four of five SDM methods (MaxEnt models based on all 
   observation data are already estimated in CV step, above); one script is 
   built for each row (species) in data/gbif-reconcile.csv
   3. **src/bash/build-scripts-3-predict.sh**: bash shell script to build an R 
   script for each species to calculate predicted probabilities for each 
   climate model for each SDM method; one script is built for each row 
   (species) in data/gbif-reconcile.csv
   4. **src/bash/build-scripts-4-overlap.sh**: bash shell script to build an R 
   scripts for each species of insect to create overlap rasters of insect with 
   its respective host plant(s)
3. Bulk processing of single-species analyses (see below for example graphic)
   1. **src/run-indiv/run-all-1-CV-scripts.R**: Run individual species' 
   cross-validation scripts; can toggle on/off to run all species or just a 
   subset.
   2. **src/run-indiv/run-all-2-SDMs-full-scripts.R**: Run individual species' 
   scripts for SDM estimation based on all data (for BRT, GAM, Lasso, and RF
   methods); can toggle on/off to run all species or just a subset.
   3. **src/run-indiv/run-all-3-predict-scripts.R**:  Run individual species' 
   scripts for suitability prediction and combine for ensemble suitability and 
   presence/absence rasters (for BRT, GAM, Lasso, MaxEnt, and RF methods); can 
   toggle on/off to run all species or just a subset.
4. Running analyses on HPC (if the the run-indiv scripts of step 3 are to be 
run on a high-performance computing cluster)
   1. **src/hpc/run-all-1-CV.slurm**: Run the R script 
   src/run-indiv/run-all-1-CV-scripts.R via slurm
   2. **src/hpc/run-all-2-SDMs-full.slurm**: Run the R script 
   src/run-indiv/run-all-2-SDMs-full-scripts.R via slurm
   3. **src/hpc/run-all-3-predict.slurm**: Run the R script 
   src/run-indiv/run-all-3-predict-scripts.R via slurm
5. Synthesizing results of single-species analyses (**UNDER CONSTRUCTION**)
   1. **src/summary/summary-1-create-overlap-rasters.R**: Create predicted 
   overlap rasters for each species of insect; see details of raster cell 
   values in the script. Will also create maps (ggplot-produced png files) if 
   indicated.
   2. **src/summary/summary-2-compare-ranges.R**: Compare the ranges of current 
   to forecast distributions, both considering insect ranges alone, and 
   considering only the areas where insects are predicted to overlap with one 
   or more host plant species; several metrics calculated, including area and 
   median latitude.
   3. **src/summary/summary-3-create-delta-rasters.R**: Create raster of 
   predicted differences in range between contemporary climate and forecast 
   climate models. Will also create maps (currently png files) if indicated.
   4. **src/summary/summary-4-draw-species-richness-maps.R**: Draw maps of 
   _Papilio_ species richness for current and forecast climate conditions and a 
   map showing the change between current and forecast estimates.
   5. **src/summary/create-observations-maps.R**: Create graphics files 
   (currently png) of filtered observations on a map.

### Analysis workflow example
Analysis workflow with _Papilio rumiko_ and one of its host plants, 
_Casimiroa greggii_, from bulk processing of single-species analyses (step 3, 
above).
![Example of analysis workflow with _Papilio rumiko_ and one of its host plants, _Ptelea trifoliata_](docs/analysis-workflow.svg)

## Directory structure

+ data
    + ensemble: forecast climate data
    + gbif: observation data; most data are under version control in zip files
    starting with the string "gbif-". Individual csv files are not under 
    version control.
        + downloaded: data downloaded from GBIF; no QA/QC or filtering
        + filtered: filtered observation data (see src/data/gbif-2-filter.R)
        + presence-absence: observation data with presence and pseudo-absences
        + shapefiles: minimum convex polygons based on filtered observations
    + lakes: North American large bodies of water shapefiles; used to exclude 
    climate data from such areas (see src/data/prep-climate-data.R)
    + wc2-1: current climate data
+ development: directory for script development
    + data: data for developmental purposes
    + functions: R functions under development
    + output: destination folders for output from developmental scripts/
    functions; generally files are not under version control, although 
    directory structure is.
        + SDMs: model objects from species distribution modeling
        + distributions: predicted distributions based on species distribution 
        models (from SDMs folder) and climate data (either current or forecast)
    + src: R scripts under development
+ docs: additional detailed documentation
+ functions: R functions used by the project
+ logs: Log files from parallel processing of modeling and forecasting steps 
(files with .log extension are not under version control)
+ output: most files are not under version control, but directory structure is
    + areas: estimated areas and changes from contemporary distributions
    + aridity: DEPRECATED, early measurements of aridity
    + distributions: raster files of predicted distributions for individual 
    species; ensemble predictions based on five SDM methods
    + maps: distribution maps (image files) for insect species and hosts
    + overlaps: composite rasters of insect and host species
    + plots: miscellaneous data visualizations
    + SDMs: best-fit species distribution models for five SDM methods
    + suitabilities: predicted suitability values
    + summary-stats: summary statistics from model performances
    + variable-contributions: DEPRECATED summary of SDMs' relative contribution 
    of each bioclimatic variable in predicting presence / absence
+ src: 
    + bash: bash scripts to generate individual species R scripts; also 
    includes a fairly brute force approach for identifying R package 
    dependencies (find-dependencies.sh)
    + data: R scripts for data download, assessment, and quality control
    + hpc: scripts (primarily slurm) for running analyses on HPC
    + indiv: R scripts for individual species modeling and forecasting; 
    automatically created by shell scripts in src/bash
    + run-indiv: R scripts to run individual species scripts in src/indiv in 
    parallel
    + summary: R scripts to analyze and visualize results of individual species
    modeling and predictions
+ templates: template R scripts used by shell scripts to generate modeling 
and prediction scripts for individual species
+ tests: woefully depauperate location for tests

## Miscellany

+ load_functions.R is a helper script that loads all the functions in the 
functions directory with the `source()` command

## Additional resources

+ Excellent set of examples and underlying rationale of species distribution 
modeling in R: [https://rspatial.org/raster/sdm/index.html](https://rspatial.org/raster/sdm/index.html)
+ Performance comparisons among several species distribution modeling methods: 
Valavi et al. 2021, [https://doi.org/10.1002/ecm.1486](https://doi.org/10.1002/ecm.1486).
+ Another comparison among different SDM approaches at
[https://doi.org/10.1002/ecm.1370](https://doi.org/10.1002/ecm.1370)
+ Good best practices that we should probably adopt at  [https://cran.r-project.org/web/packages/rangeModelMetadata/vignettes/rmm_workflowWithExampleRangeModel.html](https://cran.r-project.org/web/packages/rangeModelMetadata/vignettes/rmm_workflowWithExampleRangeModel.html)
+ Heh heh, just look: [https://doi.org/10.1111/2041-210X.13591](https://doi.org/10.1111/2041-210X.13591)
+ Worked example of species distribution models used to inform decision-making
[https://doi.org/10.1093/biosci/biz045](https://doi.org/10.1093/biosci/biz045)

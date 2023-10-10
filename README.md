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
method (e.g. maximum entropy, boosted regression trees, random forest)
    1. Estimate species distribution model for each species (referred to in 
    scripts as the "SDM" step)
    2. Predict distributions for each species based on the estimated species 
    distribution model under a variety of global climate models, including 
    contemporary climate ("current") and ensemble forecast climate models (this
    is referred to in scripts as the "distribution" step)
    3. Combine predicted distributions for each species into a consensus 
    distribution for each global climate model (this is referred to as the 
    "consensus" step)
    3. Combine predicted consensus distributions of each _insect_ species with 
    the consensus predictions for all of its respective host plants to create a 
    single raster with distributional information (see documentation in 
    functions/overlap_raster.R for interpretations of values in those rasters) 
    (referred to as the "overlap" step)
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
       1. are observations from 2000-2022,
       2. are in locations with climate data (which effectively restricts 
       observations to North America), and
       3. are inside the 95% contour of observations
   3. **src/data/gbif-3-presence-absence.R**: Generate a presence/absence 
   dataset for each species, to be used in any species distribution model
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
   1. **src/bash/build-scripts-SDM.sh**: bash shell scripts to build species 
   distribution models for individual species; one script is built for each 
   row (species) in data/gbif-reconcile.csv
   2. **src/bash/build-scripts-distribution.sh**: bash shell scripts to build R 
   scripts that predict presence / absence of species based on species 
   distribution models and predictor data (e.g. current bioclimatic data and
   forecast data)
   3. **src/bash/build-scripts-overlap.sh**: bash shell scripts to 
   build R scripts that create overlap rasters for each species of insect with 
   its respective host plant(s)
3. Bulk processing of single-species analyses (see below for example graphic)
   1. **src/run-indiv/run-all-SDM-\<method\>-scripts.R**: Run each species 
   distribution model creation script created in 2.i (operates in parallel 
   using `parallel::mclapply`)
   2. **src/run-indiv/run-all-distribution-\<method\>-scripts.R**: Run each 
   prediction script for individual species for current and forecast climate 
   conditions; generate presence / absence rasters for all species
   3. **src/run-indiv/run-all-overlap-\<method\>-scripts.R**: Assemble 
   predicted presence / absence rasters for each insect and associated host 
   plants into a single raster per insect species; see documentation in 
   functions/overlap_raster.R for raster values
4. Synthesizing results of single-species analyses
   1. **src/summary/create-overlap-maps-\<method\>.R**: Use predicted overlap 
   rasters to generate maps (image files), one for each species of insect in 
   data/insect-host.csv
   2. **src/summary/calculate-range-sizes-\<method\>.R**: Calculate range sizes 
   (in square kilometers) for each insect species, as well as area 
   (km<sup>2</sup>) of the insect's range that overlaps with at least one host 
   plant species' range and the area of the insect's range that overlaps with 
   zero host plant species' ranges
   3. **src/summary/calculate-variable-contributions-\<method\>.R**: Extract 
   results of species distribution model to assess contributions of each 
   predictor to model output
   3. **src/summary/compare-range-changes-\<method\>.R**: **DEPRECATED** 
   Compare the range sizes of current and forecast distributions, both 
   considering insect ranges alone, and considering only the areas where 
   insects are predicted to overlap with one or more host plant species
   4. **src/summary/draw-species-richness-maps-\<method\>.R**: Draw maps of 
   _Papilio_ species richness for current and forecast climate conditions and a 
   map showing the change between current and forecast estimates

### Analysis workflow example
Analysis workflow with _Papilio rumiko_ and one of its host plants, 
_Ptelea trifoliata_, from bulk processing of single-species analyses (step 3, 
above).
![Example of analysis workflow with _Papilio rumiko_ and one of its host plants, _Ptelea trifoliata_](docs/analysis-workflow.svg)

## Directory structure

+ data
    + ensemble: forecast climate data
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
    + distributions: raster files of predicted distributions for individual 
    species
    + maps: distribution maps (image files) for insect species and hosts
    + plots: miscellaneous data visualizations
    + overlaps: composite rasters of insect and host species
    + SDMs: species distribution models
    + variable-contributions: summary of SDMs' relative contribution of each 
    bioclimatic variable in predicting presence / absence
+ src: 
    + bash: bash scripts to generate individual species R scripts
    + data: R scripts for data download and assessment
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
+ The script count-gbif-names.R provides a way to count the number of records 
that GBIF would return. Useful for identifying taxonomic incongruence (e.g. 
cases where the accepted name does not match the name used by GBIF, _Papilio 
polyxenes_ vs. _P. polibetes_)
+ compare-estimates-aridity.R looks at coefficient estimates for the logistic 
regression model to see if there is a difference between arid and non-arid 
insect species
+ Examples (i.e. developmental scripts) can be found with the prefix 
"papilio_multicaudata"
    + papilio_multicaudata-ggplot.R: Using ggplot to overlay insect and host 
    distributions. Mostly proof of concept replaced by combination of functions 
    `overlap_raster` and `overlap_map`
    + papilio_multicaudata-range-comparison.R: Comparing proportion of range 
    overlap of insect and hosts for current and forecast conditions. Replaced 
    by plot-change-proportion-overlap.R
    + papilio_multicaudata-svm-sdm.R: An example of running support vector 
    machine species distribution model and using the model to predict the 
    current and forecast distribution of _P. multicaudata_ and a subset of its
    host plants
+ single-species-download.R can be used to test (and develop) automated 
downloads of a single species from GBIF.

## Additional resources

+ Excellent set of examples and underlying rationale of species distribution 
modeling in R: [https://rspatial.org/raster/sdm/index.html](https://rspatial.org/raster/sdm/index.html)
+ Good best practices that we should probably adopt at  [https://cran.r-project.org/web/packages/rangeModelMetadata/vignettes/rmm_workflowWithExampleRangeModel.html](https://cran.r-project.org/web/packages/rangeModelMetadata/vignettes/rmm_workflowWithExampleRangeModel.html)
+ Heh heh, just look: [https://doi.org/10.1111/2041-210X.13591](https://doi.org/10.1111/2041-210X.13591)
+ Comparison among different SDM approaches at
[https://doi.org/10.1002/ecm.1370](https://doi.org/10.1002/ecm.1370)
+ Worked example of species distribution models used to inform decision-making
[https://doi.org/10.1093/biosci/biz045](https://doi.org/10.1093/biosci/biz045)

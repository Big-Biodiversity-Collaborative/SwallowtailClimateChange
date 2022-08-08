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
generalized linear model, support vector machine). The models are then used to 
predict presence or absence under a variety of conditions, including current 
climate and forecast climate models. These predictions are used to estimate 
change in the range sizes of individual butterfly species and the relative size 
of range overlap of their known host plant species.

## Dependencies

The project uses the following additional R packages:

+ dismo
+ dplyr
+ ggplot2
+ kernlab
+ maptools
+ parallel (usually part of R distribution, but needs explicit loading)
+ randomForest
+ raster
+ RColorBrewer
+ rgdal
+ Rtools (needed to use the zip() function on Windows machines)
+ sf
+ sp
+ spocc
+ stringr
+ terra
+ tidyr

To run models implementing the MaxEnt approach, you will also need the MaxEnt 
program, which can be downloaded from 
[https://biodiversityinformatics.amnh.org/open_source/maxent/](https://biodiversityinformatics.amnh.org/open_source/maxent/).
The file maxent.jar then needs to be moved (or copied) to the java folder of 
the dismo package. You can find where that folder exists on your machine by 
typing, in an R console:

```{r}
system.file("java", package = "dismo")
```

## Workflow

The workflow has the general structure of:

1. Data retrieval and cleaning
2. Preparing R scripts for analyses of individual species
3. Bulk processing of single-species analyses
4. Synthesizing results of single-species analyses

### Scripts, in order of use

In descriptions below, \<model\> refers to a character string indicating the 
model used for species distribution modeling, e.g. "glm" for generalized 
linear model and "svm" for support vector machine.

1. Data retrieval and cleaning (in src/data)
   1. **src/data/download-data.R**: Download observational data from GBIF to 
   the data folder; note by default the data files that are downloaded by this 
   script are _not_ under version control
   2. **src/data/data-gbif-qa.R**: Run quality assurance on downloaded data, 
   to ensure observations fall within geographic area of interest, in this 
   case, North America
   3. **src/data/prep-climate-data.R**: Download monthly climate data for time 
   span of interest (2000-2018) and calculate the average values for the 19 
   standard bioclimatic variables (should not need to be run locally; data are 
   available in data/wc2-1 directory)
   4. **src/data/prep-forecast-data.R**: Download monthly climate data for 
   ensemble of forecast climate models and calculate the average values for the 
   19 standard bioclimatic variables (should not need to be run locally; data 
   are available in data/ensemble sub-directories)
2. Preparing R scripts for analyses of individual species
   1. **src/bash/build-scripts-model.sh**: bash shell scripts to build species 
   distribution models for individual species; one script is built for each 
   row (species) in data/gbif-reconcile.csv
   2. **src/bash/build-scripts-prediction.sh**: bash shell scripts to build R 
   scripts that predict presence / absence of species based on species 
   distribution models and predictor data (e.g. current bioclimatic data and
   forecast data)
   3. **src/bash/build-scripts-overlap-raster.sh**: bash shell scripts to 
   build R scripts that create overlap rasters for each species of insect with 
   its respective host plant(s)
3. Bulk processing of single-species analyses (see below for example graphic)
   1. **src/run-indiv/run-all-model-\<model\>-scripts.R**: Run each species 
   distribution model creation script created in 2.i (operates in parallel 
   using `parallel::mclapply`)
   2. **src/run-indiv/run-all-prediction-\<model\>-scripts.R**: Run each 
   prediction script for individual species for current and forecast climate 
   conditions; generate presence / absence rasters for all species
   3. **src/run-indiv/run-all-overlap-raster-\<model\>-scripts.R**: Assemble 
   predicted presence / absence rasters for each insect and associated host 
   plants into a single raster per insect species
4. Synthesizing results of single-species analyses
   1. **src/summary/create-overlap-maps-\<model\>.R**: Use predicted overlap 
   rasters to generate maps (image files), one for each species of insect in 
   data/insect-host.csv
   2. **src/summary/calculate-range-sizes.R**: Calculate range sizes (in 
   square kilometers) for each insect species, as well as area 
   (km<sup>2</sup>) of the insect's range that overlaps with at least one host 
   plant species' range and the area of the insect's range that overlaps with 
   zero host plant species' ranges
   3. **src/summary/compare-range-changes.R**: **DEPRECATED** Compare the range 
   sizes of current and forecast distributions, both considering insect ranges 
   alone, and considering only the areas where insects are predicted to overlap 
   with one or more host plant species
   4. **src/summary/draw-species-richness-maps-glm.R**: Draw maps of _Papilio_ 
   species richness for current and forecast climate conditions and a map 
   showing the change between current and forecast estimates

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
    + areas: estimated ranges of insects and the overlap with hostplants
    + distributions: raster files of predicted distributions for individual 
    species
    + maps: distribution maps (image files) for insect species and hosts
    + plots: miscellaneous data visualizations
    + ranges: composite rasters of insect and host species and estimates of 
    range areas
+ src: 
    + bash: bash scripts to generate individual species R scripts
    + data: R scripts for data download and assessment
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

# SwallowtailClimateChange
Data and code for North American Swallowtail and larval host plant distributions in relation to climate change

**Currently under development**

## General approach:

Retrieve data from online sources (in this case, the Global Biodiversity 
Information Facility, [GBIF](https://gbif.org)) and perform quality control 
processes to ensure observations are only from Canada, Mexico, and the United 
States of America. The data are analyzed to create species distribution models 
based on presence and pseudo-absence data using a variety of models (e.g. 
generalized linear model, support vector machine). The models are then used to 
predict presence or absence under a variety of conditions, including current 
climate and forecast climate models. These predictions are used to estimate 
change in the range sizes of individual butterfly species and the relative size 
of range overlap of their known host plant species.

## Scripts, in order of use

In descriptions below, <model> refers to a character string indicating the 
model used for species distribution modeling, e.g. "glm" for generalized 
linear model and "svm" for support vector machine.

1. **download-data.R**: Download observational data from GBIF to the data folder; 
note by default these data files are _not_ under version control.
1. **data-gbif-qa.R**: Run quality assurance on downloaded data, to ensure 
observations fall within geographic area of interest, in this case, North 
America
1. **build-model-<model>-files.sh**: bash shell scripts to build species 
distribution models for individual species. One script is built for each row 
(species) in data/gbif-reconcile.csv.
1. **run-all-model-<model>-scripts.R**: Run each script that was generated in 
previous step (operates in parallel using `parallel::mclapply`)
1. **run-all-prediction-<model>-scripts.R**: Use species distribution model to 
predict presence / absence for current and forecast climate conditions
1. **build-overlap-rasters-<model>.R**: Assemble predicted presence / absence
rasters for each insect and associated host plants into a single raster per 
insect species
1. **build-distribution-maps-<model>.R**: Use predicted overlap rasters to 
generate maps, one for each species of insect in data/insect-host.csv
1. **calculate-range-sizes.R**: Calculate range sizes (in square kilometers) 
for each insect species
1. **compare-range-sizes.R**: Compare the range sizes of current and forecast distributions, both considering insect ranges alone, and considering only the 
areas where insects are predicted to overlap with one or more host plant 
species

## Directory structure

+ data
    + cmip5/2_5m: forecast climate data (not under version control)
    + wc2-5: current climate data (not under version control)
+ functions: R functions used by the project
+ logs: Log files from parallel processing of modeling and forecasting steps 
(not under version control)
+ output
    + distributions: raster files of predicted distributions for individual 
    species
    + maps: distribution maps for insect species and hosts
    + models: species distribution model objects
    + plots: miscellaneous data visualizations
    + ranges: estimates of range areas
+ scripts: scripts for individual species modeling and forecasting; 
automatically created by shell script
+ templates: template R scripts used by shell scripts to generate modeling 
and forecasting scripts for individual species
+ tests: woefully depauperate location for tests

## Miscellany

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
    distributions. Mostly proof of concept replaced by create_overlaps.R in 
    the functions directory
    + papilio_multicaudata-range-comparison.R: Comparing proportion of range 
    overlap of insect and hosts for current and forecast conditions. Replaced 
    by plot-change-proportion-overlap.R
    + papilio_multicaudata-svm-sdm.R: An example of running support vector 
    machine species distribution model and using the model to predict the 
    current and forecast distribution of _P. multicaudata_ and a subset of its
    host plants
+ single-species-download.R can be used to test (and develop) automated 
downloads of a single species from GBIF.
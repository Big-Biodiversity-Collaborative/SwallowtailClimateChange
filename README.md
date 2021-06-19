# SwallowtailClimateChange
Data and code for North American Swallowtail and larval host plant distributions in relation to climate change

**Currently under development**

## General approach:

In descriptions below, <model> refers to a character string indicating the 
model used for species distribution modeling, e.g. "glm" for generalized 
linear model and "svm" for support vector machine.

1. Download observational data from the [Global Biodiversity Information 
Facility](https://gbif.org) to the data folder; note by default these data 
files are _not_ under version control. (download-data.R)
2. Run quality assurance on downloaded data, to ensure observations fall within
geographic area of interest, in this case, North America (data-gbif-qa.R)
3. Run a species distribution model for individual species (in the scripts 
directory, files ending in "model-<model>.R")
4. Use the resultant model to predict distributions, for forecast climate 
conditions (in the scripts directory, files ending in "-forecast-<model>.R")
5. Create maps and calculate proportion overlap between insect and hosts 
(build-distribution-maps-<model>.R); maps are saved to output/maps and overlap 
calculations are saved to output/<model>-overlaps.csv
6. Create biodiversity maps of insects for current and forecast models
(build-biodiversity-map.R)
7. Interrogate changes in overlap between current and forecast models 
(plot-change-proportion-overlap.R)
8. Calculate range sizes (in square kilometers) for insect species 
(calculate-range-sizes.R)
9. Compare the range sizes of current and forecast distributions, both 
considering insect ranges alone, and considering only the areas where insects 
are predicted to overlap with one or more host plant species 
(compare-range-sizes.R)

Wrapper scripts to accomplish 3 and 4 above for all species are 
run-all-model-<model>-scripts.R and run-all-forecast-<model>-scripts.R, 
respectively. 

Scripts to run species distribution modeling and to generate forecast 
distributions for each species individually are generated from shell script 
build-model-<model>-files.sh and build-forecast-<model>-files.sh, respectively.

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
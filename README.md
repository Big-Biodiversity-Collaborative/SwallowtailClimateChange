# SwallowtailClimateChange
Data and code for North American Swallowtail and larval host plant distributions in relation to climate change

**Currently under development**

## General approach:

1. Download observational data from the [Global Biodiversity Information 
Facility](https://gbif.org) (download-data.R)
2. Run quality assurance on downloaded data, to ensure observations fall within
geographic area of interest, in this case, North America (data-gbif-qa.R)
3. Run a species distribution model for individual species (in the scripts 
directory, files ending in "-sdm.R")
4. Use the resultant model to predict distributions, for forecast climate 
conditions (in the scripts directory, files ending in "-forecast.R")
5. Create maps and calculate proportion overlap between insect and hosts 
(build-all-distribution-maps.R)
6. Create biodiversity maps of insects for current and forecast models
(build-biodiversity-map.R)
7. Interrogate changes in overlap between current and forecast models 
(plot-change-proportion-overlap.R)

Wrapper scripts to accomplish 3 and 4 above for all species are 
run-all-sdm-scripts.R and run-all-forecast-scripts.R, respectively.

## Miscellany

+ The script count-gbif-names.R provides a way to count the number of records 
that GBIF would return. Useful for identifying taxonomic incongruence (e.g. 
cases where the accepted name does not match the name used by GBIF, _Papilio 
polyxenes_ vs. _P. polibetes_)
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

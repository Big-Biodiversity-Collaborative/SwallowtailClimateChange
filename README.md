# SwallowtailClimateChange
Data and code for North American Swallowtail and larval host plant distributions in relation to climate change

**Currently under development**

General approach:

1. Run a species distribution model for individual species (in the scripts 
directory, files ending in "-sdm.R")
2. Use the resultant model to predict distributions, for forecast climate 
conditions (in the scripts directory, files ending in "-forecast.R")
3. Create maps and calculate proportion overlap between insect and hosts 
(script build-all-distribution-maps.R)

Wrapper scripts to accomplish 1 and 2 above for all species are 
run-all-sdm-scripts.R and run-all-forecast-scripts.R, respectively.

Examples (i.e. developmental scripts) can be found with the prefix 
"papilio_multicaudata"
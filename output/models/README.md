# Species distribution models

## Summary

Destination director for the output of a variety of species distribution 
models. Likely not under version control.

### File naming convention

<genus>_<species>-model-<model type>-<predictors>.rds

+ genus: Genus name
+ species: Specific epithet
+ model type: Type of species distribution model; e.g. "bioclim", "svm"
+ predictors: Bioclimatic predictors; either "current" or the name of the 
forcast model such as "GFDL-ESM4_RCP45", where the model name is left of the 
underscore ("GFDL-ESM4") and the representative concentration pathway (RCP, 
here, 4.5% CO2) is right of the underscore. Future work should use CMIP6 data 
set, where RCP has been replaced by the shared socioeconomic pathway (SSP).


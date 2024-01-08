# Species distributions

## Summary

Destination directory for the estimated species' distributions based on species
distribution models and either current or future climate predictors. Likely not 
under version control.

### File naming convention

[genus]_[species]-distribution-[climate scenario].rds

+ genus: Genus name
+ species: Specific epithet
+ climate scenario: Climate model used for predictions; either "current" (for 
contemporary predictions) or the name of the SSP and time period of the model. 
For forecast models, the format is ensemble-ssp-[SSP]_YYYY, where SSP is 245 or 
370 and YYYY is the four-digit year (either 2041 or 2071).

# Papilio rutulus & P. glaucus workflow
2022-01-24

Rough steps for a proof of concept with two species, investigating relationship 
of aridity to predicted change

+ grab the data
+ do any data qa/qc
    + consider restricting bug and plant data temporally...
+ build sdm models for bug (all bugs)
+ build sdm models for plants (all plants)
+ do current distributions for bug (all bugs)
+ do current distributions for plants (all plants)
+ do forecast distributions for bug (all bugs)
+ do forecast distributions for plants (all plants)
+ create restricted current range of bug to areas with >= 1 plant
+ create restricted forecast range of bug to areas with >= 1 plant
+ calculate change between current and forecast (bugs only)
+ calculate change between current and forecast (bug range with plants in mind)
+ get aridity data & calculate measurement for a species
+ do regression between aridity & percent change
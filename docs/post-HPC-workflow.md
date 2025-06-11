# Post-HPC workflow

Following analyses on the HPC (evaluating models, estimating final model 
parameters, and making predictions based on those models), additional scripts 
are run to summarize the results and produce manuscript assets (i.e. figures 
and tables). These are run in the following order:

## The various analyses

- [ ] src/summary/summary-1-create-overlap-rasters.R
- [ ] src/summary/summary-2-compare-ranges.R
- [ ] src/summary/summary-3-create-richness-rasters.R
- [ ] src/protected-areas-1-calc-species.R
- [ ] src/protected-areas-2-calc-hotspots.R
- [ ] src/protected-areas-3-plot-species.R
- [ ] src/protected-areas-4-plot-hotspots.R

## Figures and tables for manuscript

- [ ] src/manuscript/figs-2-shifts.R
- [ ] src/manuscript/figs-3-richness.R
- [ ] src/manuscript/figs-4-distributions.R
- [ ] src/manuscript/figs-5-protected.R
- [ ] src/manuscript/tables-1-species.R
- [ ] src/manuscript/tables-2-overlap.R
- [ ] src/manuscript/render-cheatsheet.R
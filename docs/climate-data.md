# Climate data

Climate data for these project are the [19 standard bioclimatic variables](https://www.worldclim.org/data/bioclim.html) 
available from the WorldClim project ([Fick & Hijmans 2017](https://doi.org/10.1002/joc.5086)). 
All data are stored at 2.5 minutes resolution in WGS84 projection.

## Contemporary data

For contemporary climate data, we used the values averaged for the years 
2000-2018 at the 2.5 minute resolution. The time range (2000-2018) differs from 
the averages available at WorldClim (1970-2000), thus we had to do the annual 
calculations for each of the variables based on the monthly t<sub>min</sub>, 
t<sub>max</sub>, and precipitation data available at 
[https://worldclim.org/data/monthlywth.html](https://worldclim.org/data/monthlywth.html). 
The `dismo::biovars()` function was used to calculate annual values for each of 
the variables, and those annual values where then averages over 2000-2018.

## Forecast data

For forecast climate data, we used ensemble projections from the CMIP6 database, 
sourced from [https://adaptwest.databasin.org/pages/adaptwest-climatena/](https://adaptwest.databasin.org/pages/adaptwest-climatena/). 
Similar to the contemporary data, we used monthly values of t<sub>min</sub>, 
t<sub>max</sub>, and precipitation and the `dismo::biovars()` function to 
calculate values for the 19 bioclimatic variables. The forecast data are 
available in 30 second resolution; before calculating bioclimate variables, the
rasters for t<sub>min</sub>, t<sub>max</sub>, and precipitation were 
re-projected to match contemporary climate data with `raster::projectRaster()`
to ensure consistent CRS (WGS84), resolution (2.5 minutes), and geographic 
extent (roughly North America).
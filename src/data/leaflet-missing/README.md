# Missing-area polygons map

Interactive Leaflet map of polygons from the `missing-areas` shapefile. Each
polygon is drawn on the map, and a map pin marks a point inside the polygon.
Hovering a pin shows `OBJECTID` and `PA_NAME` from the shapefile.

## Provenance

The R code was written with Cursor AI; it does appear to violate terms of 
service of the tile provider (OpenStreetMap?), so use with caution.

## Data

Polygons come from [`missing-areas/missing-areas.shp`](missing-areas/missing-areas.shp)
(with `.shx`, `.dbf`, `.prj`, and `.cpg`). Coordinates are WGS 84.

## Requirements

R packages:

- `leaflet`
- `sf`
- `dplyr`

Install missing packages from CRAN, for example:

```r
install.packages(c("leaflet", "sf", "dplyr"))
```

## How to run

From the project root in R:

```r
source("missing-areas-map.R")
```

The script writes `missing-areas-map.html` and displays the map in the RStudio
Viewer (or the default HTML widget viewer). You can also open
`missing-areas-map.html` in a web browser.

## Map behavior

- Polygons are outlined and filled on the basemap.
- Pins use Leaflet’s default marker icon and are placed with
  `sf::st_point_on_surface()` so they stay inside their polygons.
- Hovering a pin shows `OBJECTID` and `PA_NAME`.

# Interactive Leaflet map of missing-area polygons
# Jeff Oliver
# jcoliver@arizona.edu
# 2026-10-05

require(dplyr)
require(leaflet)
require(sf)

################################################################################
# Data
################################################################################

# Protected-area polygons identified as missing from related analyses
areas <- sf::st_read("missing-areas/missing-areas.shp", quiet = TRUE)

# Pins should fall inside polygons (true centroids can lie outside concave
# shapes). Compute on a projected CRS, then return to WGS 84.
pins <- areas
sf::st_geometry(pins) <- areas %>%
  sf::st_transform(crs = 3857) %>%
  sf::st_geometry() %>%
  sf::st_point_on_surface() %>%
  sf::st_transform(crs = 4326)

################################################################################
# Map
################################################################################

# Hover text for pin symbols
pin_labels <- paste0(
  "OBJECTID: ", pins$OBJECTID,
  "<br>PA_NAME: ", pins$PA_NAME
)
pin_labels <- lapply(pin_labels, htmltools::HTML)

missing_map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = areas,
              color = "#1f4e79",
              weight = 1,
              fillColor = "#5b9bd5",
              fillOpacity = 0.4,
              highlightOptions = highlightOptions(weight = 2,
                                                  color = "#0d2b45",
                                                  fillOpacity = 0.6,
                                                  bringToFront = FALSE)) %>%
  addMarkers(data = pins,
             label = pin_labels,
             labelOptions = labelOptions(direction = "auto",
                                         textsize = "12px",
                                         opacity = 0.9))

htmlwidgets::saveWidget(missing_map,
                        file = "missing-areas-map.html",
                        selfcontained = TRUE)

missing_map

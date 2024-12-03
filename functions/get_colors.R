#' Return character vector of hexadecimal colors
#' 
#' @param palette character name of palette to return; see Details
#' 
#' @details
#' Color values returned are based on the value passed to \code{palette} 
#' argument. Valid palette names are:
#' #' \describe{
#'   \item{"distdelta"}{Four color vector for types of predicted changes in 
#'   range (absent, loss, gain, stable)}
#'   \item{"eastwest"}{Two color palette used to indicate Eastern and Western 
#'   species}
#'   \item{"hotspot"}{Single element vector for coloring areas designated as 
#'   richness hotspots}
#'   \item{"overlap"}{Four color palette for indicating predicted presence / 
#'   absence estimates (absent, only host present, only insect present, host 
#'   and insect both present)"}
#'   \item{"protected"}{Four colors for the different types of land management 
#'   of protected areas (national, state, local, private)}
#'   \item{"richdelta}{Three color palette to serve as endpoints and midpoint 
#'   for maps illustrating change in species richness estimates}
#'   \item{"richness"}{Palette of eight colors based on the magma palette for 
#'   richness maps. Similar to the "weekly" palette returned by the 
#'   \code{ebirdst::ebirdst_palettes()} function}
#' } 
#' 
#' @return Character vector of hexadecimal color values (e.g. "#FF0000" for 
#' red). Returns named vectors for all palettes except hotspot, richdelta, and 
#' richness.
#' 
#' @examples 
#' rich_cols <- get_colors(palette = "richness")
get_colors <- function(palette = c("distdelta", "eastwest", "hotspot", 
                                   "overlap", "protected", "richdelta", 
                                   "richness" )) {
  graycol <- "#f2f2f2"
  pal <- switch(palette,
                distdelta = c(absent = "#ededed", loss = "#fc8d59",
                              gain = "#2c7bb6", stable = "#ffffbf"),
                eastwest = c(east = "#3eafa3", west = "#ce932a"),
                hotspot = "#D24E71",
                overlap = c(absent = "#ededed", host_only = "#b2df8a",
                            insect_only = "#a6cee3", both = "#1f78b4"),
                protected = c(national = "#1B9E77", state = "#D95F02",
                              local = "#7570B3", private = "#E7298A"),
                richdelta = c("#D10000", graycol, "#104e8b"),
                richness = {
                  # Start with seven-color palette based on plasma
                  rich_cols <- rev(hcl.colors(n = 7, palette = "plasma"))
                  # Drop the first value in the vector, which is ugly yellow
                  rich_cols <- rich_cols[-1]
                  # Create color function between our zero (light gray) and 
                  # first yellow to span gray to first color; otherwise the
                  # contrast between 0 and 1 is too high
                  gry_ramp <- colorRampPalette(c(graycol, rich_cols[1]))
                  # Create final vector with gray (1), bridge to our palette 
                  # (2), and remainder of palette (3-8)
                  rich_cols <- c(graycol, gry_ramp(6)[3], rich_cols)
                  rich_cols
                })
  return(pal)
}
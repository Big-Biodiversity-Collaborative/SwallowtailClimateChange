# Do species accumulation curves for four types of protected areas
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-12-01

require(vegan)
require(dplyr)
require(terra)

# Would like to build species accumulation curves (SAC), to compare (estimated) 
# richness between current climate and forecast conditions. Essentially, would 
# like a plot for each forecast climate model and each of the four protection 
# levels, with a curve for current richness and a curve for the forecast 
# richness. 

# There is some question about what the sample unit is for the plot - it could 
# be grid cells, which is probably the "easiest" as our data are currently in 
# that form. I wonder though if square kilometers (area) would be a little 
# easier to understand.

# In order to take advantage of the vegan package for this, we will want a 
# data frame that looks like:
#         Sp. 1  Sp. 2. ... Sp. T
# Site 1      0      1  ...     0
# Site 2      1      1  ...     0
#   ...     ...    ...  ...   ...
# Site F      0      0  ...     1

# A little wary, as F is going to be not small...
# Let's just try a sample dataset
test <- FALSE # toggle for the sample dataset example
if (test) {
  species <- paste0("Sp", c(1:15))
  counts <- lapply(X = 1:15, FUN = function(x) {
    sample(x = c(0,1), size = 60000, replace = TRUE, prob = c(0.9, 0.1))
  })
  names(counts) <- species
  # This gets us to that desired data shape:
  df <- dplyr::bind_cols(counts)
  
  # Do the species accumulation curve calculations
  sac <- vegan::specaccum(df)
  # Plot the curve (really a square)
  plot(sac)
  # Zoom in to part of the curve that is interesting
  plot(sac, xlim = c(0, 200))
  # OK, seems pretty good. The sac function might have taken a couple seconds, 
  # but did not crash.
}

# Read in protected areas info (may take 3 minutes or so)
shpfile_path <- "data/protected-areas/protected-areas-categorized.shp"
message("Reading protected areas shapefile (may take a few minutes)...")
protected_areas <- terra::vect(shpfile_path)
message("Protected areas shapefile read.")

# Read in insect information
ih <- read.csv(file = "data/insect-host.csv")

# Read in climate models data frame
climate_models <- read.csv(file = "data/climate-models.csv")

# Need to create a SpatRaster of all current suitability rasters (0, 1)
# May be able to avoid species-level iteration, if we can create a vector of 
# filenames to pass to terra::rast()
# nice_names <- tolower(gsub(pattern = " ",
#                            replacement = "_",
#                            x = unique(ih$insect)))
# overlap_files <- paste0("output/overlaps/",
#                           nice_names, "-overlap-current.rds")
overlap_files <- list.files(path = "output/overlaps",
                            pattern = "*-overlap-current.rds",
                            full.names = TRUE)

# TODO: May need to do some filtering out of species with no future foreasts?
overlap_rasters <- lapply(X = overlap_files,
                          FUN = readRDS)

# TODO: Use file names to pull out species name and name those list elements

# Need to reclassify, so:
# [0, 1, 2, 3] = 0
# [4, 5] = 1
# Using the two-column matrix approach appropriate for integers
rcl_mat <- matrix(data = c(0, 1, 2, 3, 4, 5,
                           0, 0, 0, 0, 1, 1),
                  byrow = FALSE,
                  ncol = 2)
colnames(rcl_mat) <- c("is", "becomes")
overlap_rasters <- lapply(X = overlap_rasters,
                          FUN = terra::classify,
                          rcl = rcl_mat)

# How the hell do I do the next step of making the data frame? Maybe...
# 1. Expand all rasters to same extent
# 2. Raster-by-raster, extract 0,1 for each cell (a vector for each species?)
# 3. Combine these vectors into a data frame?

# Start by finding extent of all rasters (the min/max of lon. & lat.)
overlaps_extent <- terra::ext(terra::sprc(overlap_rasters))

# Extend all the rasters so they have the same geographic extent
overlap_rasters <- lapply(X = overlap_rasters,
                          FUN = terra::extend,
                          y = overlaps_extent,
                          fill = NA)

# Extract raster values as data frames. Creates a list with one element (data 
# frame) for each species
overlap_dfs <- lapply(X = overlap_rasters,
                      FUN = terra::as.data.frame,
                      xy = TRUE, # TODO: should only need xy or cells
                      cells = TRUE,
                      na.rm = FALSE,
                      wide = FALSE)

# Do a reality check, to ensure all grid cells are same (same lon and lat) 
# across species. But we will cheat and only look at a sample of 1000 rows 
# (there are over 4M). 
rowsamp <- sample(x = 1:nrow(overlap_dfs[[1]]), size = 1000)
# Because numerics absolutely HATE 0, we add in a tolerance parameter to set a 
# cutoff for how high the variance has to be in order for us to actually 
# consider lon/lat to not be equal
tol <- 1e-16

# Pull out longitudes & latitudes for the sampled rows, calculate variance, and 
# see if any have variances that are considered too high
# Longitude (x)
xs <- lapply(X = overlap_dfs,
             FUN = function(df, whichrows) {
               df %>%
                 select(x) %>%
                 slice(rowsamp)
             },
             whichrows = rowsamp)
x_df <- bind_cols(xs, .name_repair = "unique_quiet")
x_var <- x_df %>% 
  rowwise() %>% 
  mutate(variance = var(c_across(where(is.numeric)))) %>% 
  select(variance)
# Latitude (y)
ys <- lapply(X = overlap_dfs,
             FUN = function(df, whichrows) {
               df %>%
                 select(y) %>%
                 slice(rowsamp)
             },
             whichrows = rowsamp)
y_df <- bind_cols(ys, .name_repair = "unique_quiet")
y_var <- y_df %>% 
  rowwise() %>% 
  mutate(variance = var(c_across(where(is.numeric)))) %>% 
  select(variance)
if (any(x_var$variance > tol)) {
  warning("Not all longitudes equal in raster cell samples")
}
if (any(y_var$variance > tol)) {
  warning("Not all latitudes equal in raster cell samples")
}

# Whew. It's not paranoia if they are out to get you. We now feel confident 
# that the data frames are the same across all species. Fastidiously cleaning.
rm(xs, ys, x_df, y_df, x_var, y_var, rowsamp, tol)

# We can create a new data frame that just has the "value" column as presence/
# absence for each species
values <- bind_cols(lapply(X = overlap_dfs,
                           FUN = dplyr::pull,
                           var = values),
                    .name_repair = "unique_quiet")
# And we do actually want cell id; we can add in lon/lat later if we need to
values <- cbind(overlap_dfs[[1]][, c("cell", "x", "y")], values)

# Drop the rows that are NA for all species
# TODO: will need to update this when we appropriately name columns with the 
# actual species name
sp_cols <- paste0("...", 1:15)
# TODO: Not this way. Something takes way too long.
# start <- Sys.time()
# non_missing <- values %>%
#   rowwise() %>%
#   mutate(all_missing = all(is.na(c_across(sp_cols))))
# end <- Sys.time()

sm_iris <- iris[c(1:2, 51:52, 101:102), ]
lapply(X = 1:nrow(sm_iris),
       FUN = function(x, df) {
         print(df[x, ])
       },
       df = sm_iris)

test_rows <- c(1:4, 1953364:1953366)
all_missting <- unlist(lapply(X = 1:nrow(values[test_rows, ]), 
       FUN = function(x, df) {
         one_row <- df[x, ]
         just_sp_cols <- one_row[, sp_cols]
         all(is.na(just_sp_cols))
        },
       df = values[test_rows, ]))

# Still way too long. Gotta be a better way. Maybe through a matrix...
all_missing <- unlist(lapply(X = 1:nrow(values),
                             FUN = function(x, df) {
                               one_row <- df[x, ]
                               just_sp_cols <- one_row[, sp_cols]
                               all(is.na(just_sp_cols))
                             },
                             df = values))

# This takes NO time. Is a for loop really the way to go?!?
for(i in 1:nrow(values)) {
  if (i %% 100000 == 0) {
    cat("Row i = ", i, "\n")
  }
}
# NOPE. When I throw   all(is.na(values[i, sp_cols])) in the loop, it crawls

# Maybe just doing some math on a matrix is quicker
values_mat <- as.matrix(values[, sp_cols])
# Update values so NAs become 0, 0 -> 1, and 1 -> 2
values_mat[values_mat > 0] <- 2
values_mat[values_mat < 2] <- 1
values_mat[is.na(values_mat)] <- 0
row_sums <- rowSums(x = values_mat)
row_missing <- row_sums < 1
row_missing[test_rows]

# And now just pull out those rows that have at least one non-NA value from the 
# original values data frame and update it so NAs -> 0
no_missing <- values[!row_missing, sp_cols]
no_missing[is.na(no_missing)] <- 0

# Run vegan::specaccum on that data frame. Consider extracting stuff in a tidy 
# way...
# Takes 23 seconds with 100 permutations
start <- Sys.time()
sac <- vegan::specaccum(comm = no_missing, permutations = 100)
end <- Sys.time()

# Do much of the above, for each of the forecast climate models

# Now plot curves
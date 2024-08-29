# Consolidation of agency information for protected areas data
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-08-09

require(dplyr)
require(terra)

################################################################################
# README
# This script does a lot. Probably too much. Currently it:
# 1. Processes the IUCN protected areas shapefile to categorize each of the 
#    62K polygons into one of four bins (see below)
# 2. Merges all polygons within each category (technically, the union of all 
#    polygons within a category) to create a SpatVector of four polygons and 
#    writes that to disk as a shapefile
# 3. Converts the SpatVector of four polygons into a SpatRaster of four layers
# 4. Uses bioclimatic variable rasters to assess how much information is lost 
#    when using the SpatRaster, rather than the SpatVector (polygon) data to 
#    measure area
################################################################################

################################################################################
# Categorization of polygons
################################################################################

# Data from 
# http://www.cec.org/north-american-environmental-atlas/north-american-protected-areas-2021/
# Includes information about agency (in the MGMT_AGNCY field). Would like to 
# consolidate into general types (national, state, regional). There is a field 
# called GOV_TYPE in the data, but the majority of records (>53K are listed as 
# "Not Reported"). Start by looking at PA_Type field, which has good "National",
# "State", and "Local" starts of strings.

# Data wrangling starts with downloading zip from Google Drive, unzipping all 
# the files, and reading the shapefile into memory.

# Update path as appropriate on local machine.
shpfile_orig <- "~/Desktop/iucn/CEC_NA_2021_terrestrial_IUCN_categories.shp"

# Read in protected areas file
pa <- terra::vect(shpfile_orig)

# Make a quick data.frame copy to interrogate
pa_df <- data.frame(pa)

# This subset will be the one we actually work with, updating AGNCY_SHORT with 
# values of "National", "State", "Local", and "Private"
agencies <- data.frame(pa) %>%
  select(COUNTRY, STATE_PROV, MGMT_AGNCY, PA_NAME, TYPE_PA, GOV_TYPE) %>%
  mutate(AGNCY_SHORT = NA_character_)

# This reality check provides a measure of progress. The sum will (ideally) 
# eventually be 0 (no missing categorizations)
sum(is.na(agencies$AGNCY_SHORT))
# 62272

####################
# MGMT_AGNCY field #
####################

####################
# Exact matches
# Several agencies are named and easily categorized in the MGMT_AGNCY field. We
# start here with those

# Two values in MGMT_AGNCY are very close string-wise, but different agency
# agencies %>%
#   filter(substr(MGMT_AGNCY, 1, 46) == "National Commission of Natural Protected Areas")
# National: "National Commission of Natural Protected Areas"
# Private: "National Commission of Natural Protected Areas - Individual landowners"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "National Commission of Natural Protected Areas",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "National Commission of Natural Protected Areas - Individual landowners",
                               true = "Private",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 61741

# National level:
# "National Capital Commission (NCC)"
# "Tennessee Valley Authority"
# State level:
# "Manitoba Agriculture and Resource Development"
# "Manitoba Conservation and Climate"
# Additional NGOs (Private)
# "Nature Conservancy of Canada"
# "Nature Trust of New Brunswick"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "National Capital Commission (NCC)",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Tennessee Valley Authority",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "American Indian Lands",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Manitoba Agriculture and Resource Development",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Manitoba Conservation and Climate",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Nature Conservancy of Canada",
                               true = "Private",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Nature Trust of New Brunswick",
                               true = "Private",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 61269

# A bunch of "Joint" things that each need individual attention; when 
# partnership includes entities at different levels (e.g., State and Local), 
# categorized under the higher-level entity (e.g., State and Local categorized
# as State). For public / private partnerships, categorized based on the public 
# entity (e.g., State and Private categorized as State)
# State & Local (categorized as State)
# "Joint - State of TN & City of Murfreesboro"
# Local
# "Joint - City of Richmond Parks & Richmond Dept. of Public Works"
# "Joint - Suffolk County / Town of East Hampton"
# "Joint - City of Milford and Little Miami Inc."
# "Joint - City of Louisville & Jefferson County"
# "Joint - Town of Killingly and Killingly High School Fore"
# "Joint - City and County of Broomfield"
# "Joint - City & County of Denver"
# "Joint - Board of Water Commissioners City and County of Denver"
# "Joint - City of Aspen/Pitkin County"
# "Joint - City of Lafayette & County of Boulder"
# "Joint - City of Louisville & County of Boulder"
# "Joint - San Francisco - Public Utilities Commission, City and County of"
# Local & Private (categorized as Local)
# "Joint - Private & City of Golden Valley"
# Private
# "Joint - Block Island Land Trust / TNC / BIC"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - State of TN & City of Murfreesboro",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - City of Richmond Parks & Richmond Dept. of Public Works",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - Suffolk County / Town of East Hampton",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - City of Milford and Little Miami Inc.",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - City of Louisville & Jefferson County",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - Town of Killingly and Killingly High School Fore",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - City and County of Broomfield",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - City & County of Denver",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - Board of Water Commissioners City and County of Denver",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - City of Aspen/Pitkin County",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - City of Lafayette & County of Boulder",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - City of Louisville & County of Boulder",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - San Francisco - Public Utilities Commission, City and County of",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - Private & City of Golden Valley",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Joint - Block Island Land Trust / TNC / BIC",
                               true = "Private",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 61242

# Three additional Canada entries that are national
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Indigenous and Northern Affairs Canada",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Indigenous Government",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Indigenous Government, Government of the Northwest Territories, Federal Government (Crown-Indigenous Relations and Northern Affairs Canada)",
                               true = "National",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 61238

# The last few that can be categorized based exact matches to MGMT_AGNCY
# State
# "Mariana Islands Government"
# Local
# "Unknown - Brushy Creek MUD"
# Private
# "Meduxnekeag River Association"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Mariana Islands Government",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Unknown - Brushy Creek MUD",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Meduxnekeag River Association",
                               true = "Private",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 61222

####################
# Partial matches
# Still looking at the MGMT_AGNCY field, the beginning values can be used for 
# some categorizations, e.g., "City of", "County of"
# City of
# County of
# City Land
# County Land
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 7) == "City of",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 9) == "City Land",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 9) == "County of",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 11) == "County Land",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 20) == "Regional Agency Land",
                               true = "Local",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 52098

# Update the national-level US agencies; we use partial match because there is 
# often additional, site-specific information in the field as well
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 29) == "Agricultural Research Service",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 23) == "Army Corps of Engineers",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 25) == "Bureau of Land Management",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 21) == "Bureau of Reclamation",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 14) == "Forest Service",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 38) == "Natural Resources Conservation Service",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 28) == "U.S. Fish & Wildlife Service",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 21) == "National Park Service",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 21) == "Department of Defense",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 29) == "Other or Unknown Federal Land",
                               true = "National",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 36084

# Some Canadian national agencies
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 25) == "Canadian Wildlife Service",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 19) == "Parks Canada Agency",
                               true = "National",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 35873

# A bunch of Canadian state agencies
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 54) == "Department of Natural Resources and Energy Development",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 17) == "Environment Yukon",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 30) == "Government of British Columbia",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 22) == "Government of Manitoba",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 26) == "Government of Newfoundland",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 26) == "Government of Saskatchewan",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 39) == "Government of the Northwest Territories",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 28) == "Ministère de l'Environnement",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 20) == "Ministère des Forêts",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 42) == "Ministry of Natural Resources and Forestry",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 13) == "Ontario Parks",
                               true = "State",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 29542

# More state-level agencies (US & CA)
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 23) == "Nova Scotia Environment",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 27) == "Other or Unknown State Land",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 25) == "Parks Operations Division",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 32) == "State Department of Conservation",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 24) == "State Department of Land",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 37) == "State Department of Natural Resources",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 23) == "State Fish and Wildlife",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 16) == "State Land Board",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 25) == "State Park and Recreation",
                               true = "State",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 17496

# More local updates
# "Regional Water Districts"
# "Other or Unknown Local Government"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 24) == "Regional Water Districts",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 33) == "Other or Unknown Local Government",
                               true = "Local",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 17231

# Fairly easy to categorize these two as "Private"
# "Private"
# "Non-Governmental Organization"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 7) == "Private",
                               true = "Private",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 29) == "Non-Governmental Organization",
                               true = "Local",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 2203


####################
# TYPE_PA field    #
####################
# Partial matches
# TYPE_PA strings often end with site-specific information, so we us partial 
# matches with substrings

# State
# "Provincial"
# Nice try, Quebec.
# "Quebec's National Park Reserve"
# "Quebec's National Park"
# "State"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 10) == "Provincial",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 30) == "Quebec's National Park Reserve",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 22) == "Quebec's National Park",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 5) == "State",
                               true = "State",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 923

# National
# "Federal Other"
# "First Nation"
# "National"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 13) == "Federal Other",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 12) == "First Nation",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 8) == "National",
                               true = "National",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 917

# TYPE_PA starting with "Territorial" are State-level (Canada)
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 11) == "Territorial",
                               true = "State",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 907

# TYPE_PA starting with "Private" (includes "Privately") are "Private"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 7) == "Private",
                               true = "Private",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 54

# The majority of remaining sites required searching the internet to find what 
# type of agency manages the area. Information is stored in CSV file
category_updates <- read.csv(file = "data/protected-areas-updates.csv")

# Add those to agencies via join (after removing duplicate rows)
category_updates <- category_updates %>%
  distinct()
agencies <- agencies %>%
  left_join(category_updates, by = join_by(STATE_PROV, PA_NAME))

# Now we have two AGNCY_SHORT columns, need to make a single one
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(is.na(AGNCY_SHORT.x),
                               true = AGNCY_SHORT.y,
                               false = AGNCY_SHORT.x)) %>%
  select(-c(AGNCY_SHORT.x, AGNCY_SHORT.y))
sum(is.na(agencies$AGNCY_SHORT))
# 10

# For the remaining 10, need to see these on a map (not enough information is 
# provided in the metadata). Start by subsetting from the original shapefile
pa_missing_category <- terra::subset(x = pa,
                                     subset = is.na(agencies$AGNCY_SHORT))
# plot(terra::subset(pa_missing_category, subset = c(TRUE, rep(FALSE, 9))))

# Can we reproject into something a little more useful?
pa_missing_wgs84 <- terra::project(pa_missing_category, "EPSG:4326")
# Oh thank god
# plot(terra::subset(pa_missing_wgs84, subset = c(TRUE, rep(FALSE, 9))))

# Let's use centroids to check on Google Maps
pa_missing_centroids <- terra::centroids(pa_missing_wgs84)
# Can get these as df, but need to pass XY to geom argument
pa_missing_cent_df <- terra::as.data.frame(pa_missing_centroids, geom = "XY")
# Print these to console for easier copy/pasting into browser; note reversal of 
# coordinates to latitude, longitude
# pa_missing_cent_df %>%
#   select(STATE_PROV, PA_NAME, TYPE_PA, y, x)

# After looking at the map, all these are local (9/10 are also labeled as 
# "Local Conservation Area" in the TYPE_PA field), so we can go ahead and 
# toggle those remaining NA values to "Local"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(is.na(AGNCY_SHORT),
                               true = "Local",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 0
# BAM

# Next step is to write this information to file to save the record
write.csv(file = "data/protected-areas-management.csv",
          x = agencies,
          row.names = FALSE)

################################################################################
# Unionization of polygons within each category
################################################################################
# We will go ahead and create a Spat with only four geometries, one each of the 
# unions for the four categories.
agencies <- read.csv(file = "data/protected-areas-management.csv")

# First, add in the AGNCY_SHORT information to the pa object so we can use it 
# with the by argument of terra::aggregate
pa[["AGNCY_SHORT"]] <- agencies$AGNCY_SHORT

# Now we can drop any areas outside the North America (mostly we just drop 
# U.S. Territories and the state of Hawaii)
state_prov_excl <- c("US-AS", "US-FM", "US-GU", "US-HI", "US-MH", "US-N/A",
                     "US-PR", "US-PW", "US-UM", "US-VI")
pa <- terra::subset(pa, subset = !(pa$STATE_PROV %in% state_prov_excl))

# Now union them (or "dissolve" in terra-speak) with aggregate. Can take 20-40 
# minutes, so be patient
# Sys.time()
pa_categorized <- terra::aggregate(pa, by = "AGNCY_SHORT")
# Sys.time()

# Reality check, also takes a few minutes to render. All polygons should now be
# restricted to continental North America: Canada, Mexico, and U.S (including 
# AK, but excluding HI and U.S. territories)
# plot(pa_categorized,
#     col = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99"))

# Re-project to EPSG:4326 before saving
pa_categorized <- terra::project(x = pa_categorized, 
                                 y = "EPSG:4326")

# Write takes a while (1:35, that's 95 minutes!)
Sys.time()
terra::writeVector(x = pa_categorized,
                   filename = "data/protected-areas/protected-areas-categorized.shp",
                   overwrite = TRUE)
Sys.time()

################################################################################
# Crop SpatVector to climate data extent
################################################################################
Sys.time()
pa_categorized <- terra::vect(x = "data/protected-areas/protected-areas-categorized.shp")

# Load in a bioclimate raster for cropping purposes
bio1 <- terra::rast(x = "data/wc2-1/bio1.tif")

# Crop the SpatVector to the same extent as the climate data
pa_categorized <- terra::crop(x = pa_categorized,
                              y = bio1)

# Write takes a while (1:35, that's 95 minutes!)
Sys.time()
terra::writeVector(x = pa_categorized,
                   filename = "data/protected-areas/protected-areas-categorized.shp",
                   overwrite = TRUE)
Sys.time()


################################################################################
# Convert SpatVector to SpatRaster
################################################################################

# Takes about 3 minutes to load (a lot longer than original shapefile that is 
# about 100MB *larger*)
Sys.time()
pa_categorized <- terra::vect(x = "data/protected-areas/protected-areas-categorized.shp")

# Load in a bioclimate raster for resolution purposes
bio1 <- terra::rast(x = "data/wc2-1/bio1.tif")

# Proof of concept on a much smaller extent (around Tucson and northward)
small_ext <- terra::ext(c(-112.14, -109.60, 32.07, 34.54))

bio_small <- terra::crop(bio1, small_ext)
# plot(bio_small)
# 30 seconds for this crop
pa_small <- terra::crop(pa_categorized, small_ext)
# plot(pa_small, col = rainbow(10))

# These polygons are much higher resolution than bioclimatic cells. Let's try 
# a simple rasterize
# DO NOT TRY TO USE by = "AGNCY_SHOR" WILL CRASH
# pa_raster <- terra::rasterize(x = pa_small,
#                               y = bio_small)
# plot(pa_small, col = rainbow(10))
# plot(pa_raster, add = TRUE)
# Fine, but we want a different raster layer for each category. Will need to do 
# this manually, as trying to do this via rasterize with by = "AGNCY_SHOR" 
# causes a crash

# Adding each as a layer to a single SpatRaster layer
# TODO: Not sure that indexing by number aligns with category correctly
# National
pa_raster <- terra::rasterize(x = pa_small[1],
                                       y = bio_small)
# State
add(pa_raster) <- terra::rasterize(x = pa_small[2],
                                       y = bio_small)
# Local
add(pa_raster) <- terra::rasterize(x = pa_small[3],
                                       y = bio_small)
# Private
add(pa_raster) <- terra::rasterize(x = pa_small[4],
                                       y = bio_small)
names(pa_raster) <- c("National", "State", "Local", "Private")
# plot(pa_raster)

# Now create a binary map of the bioclim variable, 0 below mean, 1 above mean
bio_binary <- bio_small
# Gotta get real funky to calculate a mean for a raster layer
bio_binary[bio_binary < global(bio_small, "mean", na.rm = TRUE)[[1]]] <- 0
bio_binary[bio_binary >= global(bio_small, "mean", na.rm = TRUE)[[1]]] <- 1
# plot(bio_binary)

# Take that bio_binary SpatRaster and add it to each of the layers in pa_raster
# use par = TRUE to add bio_binary to each of the pa_raster layers separately
pa_raster_sum <- sum(pa_raster, bio_binary, par = TRUE)
# plot(pa_raster_sum[[1]])
# plot(pa_raster_sum[[2]])
# plot(pa_raster_sum)

# Now, finally, find the area of cells with a 2 in each of the pa_raster_sum 
# layers
pa_raster_sum[pa_raster_sum < 2] <- NA
pa_raster_sizes <- terra::cellSize(x = pa_raster_sum,
                                   mask = TRUE,
                                   lyrs = TRUE,
                                   unit = "km")
names(pa_raster_sizes) <- c("National", "State", "Local", "Private")

# Sum values for each layer. That is area protected.
pa_areas <- terra::global(pa_raster_sizes, "sum", na.rm = TRUE)
# pa_areas
#                 sum
# National   90.12276
# State    3051.78588
# Local      90.41891
# Private   343.64057
# BAM!

# Now to do area calculation based on the polygons in pa_small
# Very cludgy because I'm tired
# make the bio_binary an area calculation
bio_binary_area <- bio_binary
bio_binary_area[bio_binary_area < 1] <- NA
bio_binary_area <- terra::cellSize(bio_binary_area, 
                                   mask = TRUE,
                                   unit = "km")
# National
in_pa_national <- exactextractr::exact_extract(x = bio_binary_area,
                                               y = sf::st_as_sf(pa_small[1]),
                                               progress = TRUE)
in_pa_national <- bind_rows(in_pa_national) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(area_in = value * coverage_fraction)
national_prot <- sum(in_pa_national$area_in)

# State
in_pa_state <- exactextractr::exact_extract(x = bio_binary_area,
                                               y = sf::st_as_sf(pa_small[2]),
                                               progress = TRUE)
in_pa_state <- bind_rows(in_pa_state) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(area_in = value * coverage_fraction)
state_prot <- sum(in_pa_state$area_in)

# Local
in_pa_local <- exactextractr::exact_extract(x = bio_binary_area,
                                               y = sf::st_as_sf(pa_small[3]),
                                               progress = TRUE)
in_pa_local <- bind_rows(in_pa_local) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(area_in = value * coverage_fraction)
local_prot <- sum(in_pa_local$area_in)

# Private
in_pa_private <- exactextractr::exact_extract(x = bio_binary_area,
                                               y = sf::st_as_sf(pa_small[4]),
                                               progress = TRUE)
in_pa_private <- bind_rows(in_pa_private) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(area_in = value * coverage_fraction)
private_prot <- sum(in_pa_private$area_in)

# Compare raster-based calculation
# pa_areas
# National   90.12276
# State    3051.78588
# Local      90.41891
# Private   343.64057

# to polygon-based calculation
# national_prot # 132.16
# state_prot    # 3001.63
# local_prot    # 65.01
# private_prot  # 312.25

# Numbers are different enough to cause concern. But still need to think about 
# the fact that our predicted area rasters are at the resolution of the cell 
# (the bioclim variable)...

# 0 0 0 0 0
# 0 0 0 0 0
# 0 0 0 0 0
# 0 0 0 0 0
# 1 0 0 0 0

# 1 1 1 1 0
# 1 1 1 1 1
# 1 1 1 1 1
# 1 1 1 1 1
# 1 1 1 1 1


# Crop the SpatVector (more time here...)
Sys.time()
pa_categorized <- terra::crop(pa_categorized, bio1)
Sys.time()

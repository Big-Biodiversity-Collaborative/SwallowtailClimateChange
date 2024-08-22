# Consolidation of agency information for protected areas data
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-08-09

require(dplyr)
require(terra)

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
pa <- vect(shpfile_orig)

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

################################################################################
# MGMT_AGNCY field
################################################################################
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

################################################################################
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

################################################################################
# TYPE_PA field
################################################################################
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
# UNIONIZE
################################################################################
# We will go ahead and create a Spat with only four geometries, one each of the 
# unions for the four categories.
agencies <- read.csv(file = "data/protected-areas-management.csv")

# TODO: Reproject entire SpatVector to EPSG:4326 here?

# Doing this the coarse way
# pa_national <- terra::subset(pa, subset = agencies$AGNCY_SHORT == "National")
# pa_national
# 16487 areas
# Hmm...this ran for > 60 minutes without finishing. See if it does what we 
# really think it is doing
# pa_national <- terra::union(pa_national)

# Start by grabbing a couple of national parks, and testing terra::union
parks <- substr(pa_df$MGMT_AGNCY, 1, 21) == "National Park Service"
parks_df <- pa_df %>% filter(parks)
sort(table(parks_df$COMMENTS))

# Four rows (different polygons) for Yellowstone National Park. Pull those out
yellowstone_rows <- pa_df$COMMENTS == "Yellowstone National Park"
yellowstone_pa <- terra::subset(pa, subset = yellowstone_rows)
plot(yellowstone_pa) # Three adjacent polygons
yellowstone_union <- terra::union(yellowstone_pa)
plot(yellowstone_union) # Now there are *FOUR* rows. Not really what we want
# It is possible that the fourth is the actual union
plot(terra::subset(yellowstone_union, subset = c(F, F, F, T)))
# No. Definitely not
terra::polys(yellowstone_pa, col = c("blue", "green", "orange", "red"))

# Try terra::aggregate?
yellowstone_agg <- terra::aggregate(yellowstone_pa)
plot(yellowstone_agg)
# Yah! There it is
terra::polys(yellowstone_pa, col = c("blue", "green", "orange", "red"))

# Try it for reals?
pa_order <- order(data.frame(pa)$GIS_HA)
pa_big3 <- pa_order %in% c(1:3)
pa_subset <- terra::subset(pa, subset = pa_big3)
pa_subset <- terra::project(x = pa_subset, y = "EPSG:4326")
plot(pa_subset, col = "#FF0000")

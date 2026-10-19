# Consolidation of agency information for protected areas data
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-08-09

require(dplyr)
require(tidyr)
require(terra)
require(exactextractr)

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
# https://www.cec.org/north-american-environmental-atlas/north-american-protected-areas-2025/
# Includes information about agency (in the MGMT_AGNCY field). Would like to 
# consolidate into general types (national, state, regional). There is a field 
# called GOV_TYPE in the data, that we start with, with a lot of areas that we 
# can easily categorize. After that, look at PA_Type field, which has good 
# "National", "State", and "Local" starts of strings.

# Data wrangling starts with downloading zip from Google Drive, unzipping all 
# the files, and reading the shapefile into memory.

# Update path as appropriate on local machine.
# shpfile_orig <- "~/Desktop/iucn/CEC_NA_2021_terrestrial_IUCN_categories.shp"
shpfile_orig <- "~/Desktop/iucn/CEC_NA_2025_terrestrial_IUCN_categories.shp"

# Read in protected areas file, may take a few seconds
pa <- terra::vect(shpfile_orig)

# Make a quick data.frame copy to interrogate
pa_df <- data.frame(pa)

# This subset will be the one we actually work with, in a moment adding the 
# column AGNCY_SHORT, which will take values of "National", "State", "Local", 
# and "Private"
agencies <- data.frame(pa) %>%
  select(OBJECTID, COUNTRY, STATE_PROV, MGMT_AGNCY, PA_NAME, TYPE_PA, GOV_TYPE)

####################
# GOV_TYPE field   #
####################

# What values show up in the GOV_TYPE field?
agencies %>%
  select(GOV_TYPE) %>%
  table()

# Plenty of easy associations here. Use the gov_type.csv to do initial 
# assignment (file was created manually). Note the 2021 version of the data had 
# much less data in the GOV_TYPE field and was not used.
gov_type <- read.csv(file = "data/protected-areas/gov_type.csv")
agencies <- agencies %>%
  left_join(gov_type, by = "GOV_TYPE")

# This reality check provides a measure of progress. The sum will (ideally) 
# eventually be 0 (no missing categorizations)
sum(is.na(agencies$AGNCY_SHORT))
# 29704

# Similarly, the PA_TYPE field has some that we can fairly easily assign
type_pa <- read.csv(file = "data/protected-areas/type_pa.csv") %>%
  select(TYPE_PA, AGNCY_SHORT)
agencies <- agencies %>%
  left_join(type_pa, by = "TYPE_PA") %>%
  mutate(AGNCY_SHORT = coalesce(AGNCY_SHORT.x, AGNCY_SHORT.y),
         .keep = "unused")
sum(is.na(agencies$AGNCY_SHORT))
# 9697

####################
# MGMT_AGNCY field #
####################

# Are there some big ones still left that we can use MGMT_AGNCY for?
# agencies %>%
#   filter(is.na(agencies$AGNCY_SHORT)) %>%
#   group_by(MGMT_AGNCY) %>%
#   summarize(count = n()) %>%
#   ungroup() %>%
#   arrange(desc(count))

# write.csv(file = "~/Desktop/still_missing.csv",
#           row.names = FALSE,
#           x = agencies %>%
#             filter(is.na(agencies$AGNCY_SHORT)) %>%
#             group_by(MGMT_AGNCY) %>%
#             summarize(count = n()) %>%
#             ungroup() %>%
#             arrange(MGMT_AGNCY))

####################
# Partial matches
# Looking at the MGMT_AGNCY field, the beginning values can be used for some 
# categorizations, e.g., "City of", "County of"
# City of
# County of
# City Land
# County Land
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 9) == "City Land",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 7) == "City of",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 11) == "County Land",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 20) == "Regional Agency Land",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 24) == "Regional Water Districts",
                               true = "Local",
                               false = AGNCY_SHORT))

sum(is.na(agencies$AGNCY_SHORT))
# 7290

# Some First Nations manage lands with MGMT_AGNCY
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 17) == "Environment Yukon",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 21) == "Indigenous Government",
                               true = "National",
                               false = AGNCY_SHORT))

sum(is.na(agencies$AGNCY_SHORT))
# 7269

####################
# Several agencies are named and easily categorized in the MGMT_AGNCY field.

# write.csv(file = "~/Desktop/still_missing.csv",
#           row.names = FALSE,
#           x = agencies %>%
#             filter(is.na(agencies$AGNCY_SHORT)) %>%
#             group_by(MGMT_AGNCY) %>%
#             summarize(count = n()) %>%
#             ungroup() %>%
#             arrange(MGMT_AGNCY))


# A bunch of Canadian state agencies
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 30) == "Government of British Columbia",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 26) == "Government of Newfoundland",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 47) == "Government of Newfounland & Labrador/Commission",
                               true = "State", # [sic] Newfoundland is misspelled
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 21) == "Government of Nunavut",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 26) == "Government of Saskatchewan",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  # They may sound national, but Quebecois!
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 28) == "Ministère de l'Environnement",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 35) == "Ministère des Ressources naturelles",
                               true = "State",               
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Ministry of Natural Resources and Forestry",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Nova Scotia Environment and Climate Change",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Ontario Parks",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Parks Division, Alberta Forestry and Parks",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 29) == "Manitoba Economic Development",
                               true = "State",               
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(MGMT_AGNCY == "Manitoba Environment and Climate Change",
                               true = "State",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))
# 1483

# write.csv(file = "~/Desktop/still_missing.csv",
#           row.names = FALSE,
#           x = agencies %>%
#             filter(is.na(agencies$AGNCY_SHORT)) %>%
#             group_by(MGMT_AGNCY) %>%
#             summarize(count = n()) %>%
#             ungroup() %>%
#             filter(count > 1) %>%
#             arrange(desc(count)))

# A fair number of locally-managed areas start with 
# "Other or Unknown Local Government"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 33) == "Other or Unknown Local Government",
                               true = "Local",               
                               false = AGNCY_SHORT))

# At this point, all areas that have been categorized so far are managed at a 
# single level, i.e. National OR State OR Local OR Private. There are some 
# areas that are jointly managed, so we will need to accommodate this 
# polymorphism. For now, this is being: the AGNCY_SHORT column
# will have semicolon-separated fields if an area is managed by entities at two 
# different levels, e.g. "National; Local".

# write.csv(file = "~/Desktop/still_missing.csv",
#           row.names = FALSE,
#           x = agencies %>%
#             filter(is.na(agencies$AGNCY_SHORT)) %>%
#             group_by(MGMT_AGNCY) %>%
#             summarize(count = n()) %>%
#             ungroup() %>%
#             filter(count > 1) %>%
#             arrange(desc(count)))

# Another join for a bunch of joint-operated areas. Relies on the 
# joint_el_al.csv another manually-created CSV file.
joint_mgmt <- read.csv(file = "data/protected-areas/joint_et_al.csv") %>%
  select(MGMT_AGNCY, AGNCY_SHORT)
agencies <- agencies %>%
  left_join(joint_mgmt, by = "MGMT_AGNCY") %>%
  mutate(AGNCY_SHORT = coalesce(AGNCY_SHORT.x, AGNCY_SHORT.y),
         .keep = "unused")
sum(is.na(agencies$AGNCY_SHORT))
# 114

# The remaining required manual updates.
category_updates <- read.csv(file = "data/protected-areas/protected-areas-updates.csv")
category_updates <- category_updates %>%
  filter(!is.na(AGNCY_SHORT)) %>%
  select(c(OBJECTID, AGNCY_SHORT))

# Merge those and see what is left
agencies <- agencies %>%
  left_join(category_updates, by = c("OBJECTID")) %>%
  mutate(AGNCY_SHORT = coalesce(AGNCY_SHORT.x, AGNCY_SHORT.y),
         .keep = "unused")

sum(is.na(agencies$AGNCY_SHORT))
# 0
# BAMF

# Next step is to write this information to file to save the record
write.csv(file = "data/protected-areas/protected-areas-management.csv",
          x = agencies,
          row.names = FALSE)

################################################################################
# Unionization of polygons within each category
################################################################################
# We will go ahead and create a Spat with only four geometries, one each of the 
# unions for the four categories.
agencies <- read.csv(file = "data/protected-areas/protected-areas-management.csv")

# Before we create the shapefile, we will actually need to extract information 
# from the AGNCY_SHORT column, which may include semi-colon-separated values in 
# AGNCY_SHORT, where the area is jointly managed by units at two or more levels 
# (e.g. "National; Local"). We add this info to the pa object. We will use the 
# info to create vectors for each of the four management types

pa[["AGNCY_Local"]] <- (grepl(pattern = "Local", 
                               x = agencies$AGNCY_SHORT))
pa[["AGNCY_State"]] <- (grepl(pattern = "State", 
                               x = agencies$AGNCY_SHORT))
pa[["AGNCY_National"]] <- (grepl(pattern = "National", 
                                  x = agencies$AGNCY_SHORT))
pa[["AGNCY_Private"]] <- (grepl(pattern = "Private", 
                                 x = agencies$AGNCY_SHORT))

# AFTER adding back the management-level information to the pa object, we can 
# drop any areas outside the North America (mostly we just drop U.S. 
# Territories and the state of Hawaii)
state_prov_excl <- c("US-AS", "US-FM", "US-GU", "US-HI", "US-MH", "US-N/A",
                     "US-PR", "US-PW", "US-UM", "US-VI")
pa <- terra::subset(pa, subset = !(pa$STATE_PROV %in% state_prov_excl))

# In turn, extract a SpatVector for each management level (can take a few 
# seconds for each)
pa_local <- pa[pa$AGNCY_Local, ]
pa_state <- pa[pa$AGNCY_State, ]
pa_national <- pa[pa$AGNCY_National, ]
pa_private <- pa[pa$AGNCY_Private, ]

# We will be adding a column "AGNCY_SHORT" to the pa objects; hopefully this 
# will help mitigate downstream problems this update may create. :D
pa_local$AGNCY_SHORT <- "Local"
pa_state$AGNCY_SHORT <- "State"
pa_national$AGNCY_SHORT <- "National"
pa_private$AGNCY_SHORT <- "Private"

# We will drop effectively all of the area-specific attributes, as they 
# will not be relevant for the single geometry (i.e. a national park name is 
# no longer needed for a geometry that includes all national-level protected 
# areas)
pa_local <- pa_local[, c("AGNCY_SHORT")]
pa_state <- pa_state[, c("AGNCY_SHORT")]
pa_national <- pa_national[, c("AGNCY_SHORT")]
pa_private <- pa_private[, c("AGNCY_SHORT")]

# Create a single (vector) geometry for each of the management levels 
# (terra::aggregate); some patience here. Takes some time, especially for those 
# geometries that require makeValid (lazily calling makeValid on all)
pa_local <- terra::aggregate(x = terra::makeValid(pa_local))
pa_state <- terra::aggregate(x = terra::makeValid(pa_state))
pa_national <- terra::aggregate(x = terra::makeValid(pa_national))
pa_private <- terra::aggregate(x = terra::makeValid(pa_private))

# Put all these back into a single SpatVector
pa_categorized <- rbind(pa_local, pa_state, pa_national, pa_private)
pa_categorized$AGNCY_SHORT <-c("Local", "State", "National", "Private")

# Create a layer that includes all UNprotected areas and add that to the larger 
# SpatVector. Start by aggregating all the protected areas into a single 
# geometry. Def takes a while. 50 minutes.
Sys.time()
all_prot_areas <- terra::aggregate(pa_categorized)
Sys.time()

# Create a rectangle that covers the extent of the protected areas polygon
area_rect <- terra::vect(terra::ext(all_prot_areas))
# plot(area_rect, col = "#d95f02", alpha = 0.4)
# plot(all_prot_areas, col = "#1b9e77", alpha = 0.6, add = TRUE)

# A little over 20 minutes for this symdif
Sys.time()
unprotected_areas <- terra::symdif(area_rect, all_prot_areas)
Sys.time()
if(!is.valid(unprotected_areas)){
  warning("Unprotected areas vector is invalid. Attempting to fix.")
  unprotected_areas <- terra::makeValid(unprotected_areas)
}
Sys.time()
# plot(unprotected_areas, col = "#BB0202", alpha = 0.4)
# Look to make sure there are "holes" where protected areas are before running 
# the next line
# plot(all_prot_areas, col = "#1b9eC7", alpha = 0.6, add = TRUE)

rm(area_rect, all_prot_areas)
gc()

# Add a pair of attributes to align with those of protected areas
unprotected_areas$AGNCY_SHORT <- "None"
crs(unprotected_areas) <- "epsg:4326"

# Write stuff to disk; about 8 minutes.
Sys.time()
saveRDS(object = pa_categorized,
        file = "data/protected-areas/protected-areas-categorized.rds")
saveRDS(object = unprotected_areas,
        file = "data/protected-areas/unprotected-areas.rds")
Sys.time()

# TODO: Consider making shapefile version of these two...
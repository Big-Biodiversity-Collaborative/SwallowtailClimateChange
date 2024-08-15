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

# table(agencies$TYPE_PA)
# Heh, there are six of these:
# Cliff inhabited by a colony of Birds

# There are a bunch of types that start with "Local", but include NGOs:
# Local Conservation Area (the bulk of them 8436)
# Local Park 
# Local Recreation Area 
# Local Resource Management Area 

# The reality check to measure progress (eventually sum = 0)
sum(is.na(agencies$AGNCY_SHORT))

# Some substr starts to deal with:
# Local
# "City Park"
# "Local Historic or Cultural Area"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 9) == "City Park",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 31) == "Local Historic or Cultural Area",
                               true = "Local",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))

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

# We can also update to Local when MGMT_AGENCY starts with 
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

# table(agencies$TYPE_PA[is.na(agencies$AGNCY_SHORT)])

# TYPE_PA starting with "Territorial" are State-level (Canada)
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 11) == "Territorial",
                               true = "State",
                               false = AGNCY_SHORT))

# TYPE_PA starting with "Private" (includes "Privately") are "Private"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(TYPE_PA, 1, 7) == "Private",
                               true = "Private",
                               false = AGNCY_SHORT))

sum(is.na(agencies$AGNCY_SHORT))

# OK, now we start looking at MGMT_AGNCY
# table(agencies$MGMT_AGNCY[is.na(agencies$AGNCY_SHORT)])

# Update the national-level US agencies
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

# Some Canadian national agencies
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 25) == "Canadian Wildlife Service",
                               true = "National",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 19) == "Parks Canada Agency",
                               true = "National",
                               false = AGNCY_SHORT))
sum(is.na(agencies$AGNCY_SHORT))

# table(agencies$MGMT_AGNCY[is.na(agencies$AGNCY_SHORT)])

# A bunch of Canadian state updates
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

# Update more state-level agencies
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

# Private
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

# table(agencies$MGMT_AGNCY[is.na(agencies$AGNCY_SHORT)])

# Two types in Mexico, very close string-wise, but different agency
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

# table(agencies$MGMT_AGNCY[is.na(agencies$AGNCY_SHORT)])  

# Some exact matches to do:
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

# table(agencies$MGMT_AGNCY[is.na(agencies$AGNCY_SHORT)])  
# agencies %>% filter(is.na(AGNCY_SHORT))

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

# Three holdouts that are national
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
# table(agencies$MGMT_AGNCY[is.na(agencies$AGNCY_SHORT)])  
# agencies %>% filter(is.na(AGNCY_SHORT))

# Last few that can be categorized based on MGMT_AGNCY
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

# agencies %>% filter(is.na(AGNCY_SHORT))

# For those remaining, pull in columns from pa_df to see if there is more 
# information
# agencies_missing <- agencies %>%
#   filter(is.na(AGNCY_SHORT)) %>%
#   select(-AGNCY_SHORT) %>%
#   left_join(pa_df, multiple = "all")
# Not much there. Need to just find all based on PA_NAME
# deep_dive <- agencies %>% 
#   filter(is.na(AGNCY_SHORT)) %>%
#   select(STATE_PROV, PA_NAME)
# write.csv(x = deep_dive,
#           row.names = FALSE,
#           file = "~/Desktop/areas-to-check.csv")

# After checking the internet, several can be updated
category_updates <- read.csv(file = "~/Desktop/pa-investigation - Sheet1.csv")
# Add those to agencies via join (after removing duplicate rows)
category_updates <- category_updates %>%
  distinct()
agencies_join <- agencies %>%
  left_join(category_updates, by = join_by(STATE_PROV, PA_NAME))

# Now we have two AGNCY_SHORT columns, need to make a single one
agencies_write <- agencies_join %>%
  mutate(AGNCY_SHORT = if_else(is.na(AGNCY_SHORT.x),
                               true = AGNCY_SHORT.y,
                               false = AGNCY_SHORT.x)) %>%
  select(-c(AGNCY_SHORT.x, AGNCY_SHORT.y))

# agencies_write %>% filter(is.na(AGNCY_SHORT))

# Note that the agencies_write data frame has several non-distinct rows, see: 
# nrow(agencies_write)
# 62272
# nrow(agencies_write %>% distinct())
# 48961
# so we will have to rely on numeric indexing to get these data back into the
# protected areas shapefile
write.csv(file = "data/protected-areas-management.csv",
          x = agencies_write,
          row.names = FALSE)

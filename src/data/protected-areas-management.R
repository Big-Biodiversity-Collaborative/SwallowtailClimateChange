# Consolidation of agency information for protected areas data
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-08-09

require(dplyr)
require(terra)

# Data from 
# http://www.cec.org/north-american-environmental-atlas/north-american-protected-areas-2021/
# Includes information about agency (in the MGMT_AGNCY field). Would like to 
# consolidate into general types (federal, state, regional). There is a field 
# called GOV_TYPE in the data, but the majority of records (>53K are listed as 
# "Not Reported"). Start by looking at PA_Type field, which has good "National",
# "State", and "Local" starts of strings.

# TODO: Start with PA_Type field instead

# Data wrangling starts with downloading zip from Google Drive, unzipping all 
# the files, and reading the shapefile into memory.

# Update path as appropriate on local machine.
shpfile_orig <- "~/Desktop/iucn/CEC_NA_2021_terrestrial_IUCN_categories.shp"

# Read in protected areas file
pa <- vect(shpfile_orig)

pa_df <- data.frame(pa)
agencies <- data.frame(pa) %>%
  select(COUNTRY, STATE_PROV, MGMT_AGNCY, PA_NAME, TYPE_PA) %>%
  mutate(AGNCY_SHORT = NA_character_)

# What does text look like at the *start* of the values?
agency_start <- substr(x = agencies$MGMT_AGNCY, 
                       start = 1,
                       stop = 7)

# unique(agency_start)
# [1] "Nationa" "City La" "Forest " "State D" "U.S. Fi" "Non-Gov" "Bureau "
# [8] "Other o" "Private" "State F" "Natural" "County " "State L" "State P"
# [15] "Tenness" "Regiona" "Joint -" "Unknown" "Joint"   "Army Co" "Federat"
# [22] "Guam Go" "America" "Other"   "Departm" "Mariana" "Agricul" "Palau G"
# [29] "Governm" "PEI Dep" "Island " "Nature " "PEI Wil" "Sir And" "Co-Mana"
# [36] "Nova Sc" "Meduxne" "Ministè" "Ontario" "Ministr" "Manitob" "City of"
# [43] "Ducks U" "Parks O" "Contact" "Environ" "Vuntut " "Indigen" "Parks C"
# [50] "Canadia"

# Start by setting AGNCY_SHORT for all the city land
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 7) == "City of",
                               true = "Local",
                               false = AGNCY_SHORT))

# See what is left, letting the substring get a little longer
agency_start <- substr(x = agencies[is.na(agencies$AGNCY_SHORT), "MGMT_AGNCY"], 
                       start = 1,
                       stop = 10)
# unique(agency_start)
# [1] "National C" "City Land " "Forest Ser" "State Depa" "National P" "U.S. Fish "
# [7] "Non-Govern" "Bureau of " "Other or U" "Private - " "State Fish" "Natural Re"
# [13] "County Lan" "State Land" "Private"    "State Park" "City Land"  "Tennessee "
# [19] "Regional A" "Regional W" "Joint - Sa" "Joint - Ci" "Unknown - " "Joint - Bo"
# [25] "Joint"      "Army Corps" "Unknown"    "Joint - To" "Joint - Na" "Federated "
# [31] "Guam Gover" "American I" "Other"      "Joint - Li" "Joint - Ke" "Joint - Fl"
# [37] "Joint - Pi" "Joint - Fr" "Joint - KY" "Joint - Mu" "Joint - We" "Joint - Pr"
# [43] "Department" "Mariana Is" "Joint - Fg" "Joint - Su" "Joint - TN" "Agricultur"
# [49] "Palau Gove" "Joint - Bl" "Joint - Te" "Joint - St" "Government" "PEI Depart"
# [55] "Island Nat" "Nature Con" "PEI Wildli" "Sir Andrew" "Co-Managed" "Private an"
# [61] "Nova Scoti" "Nature Tru" "Meduxnekea" "Ministère " "Ontario Pa" "Ministry o"
# [67] "Manitoba C" "Manitoba A" "Ducks Unli" "Nature Man" "Manitoba H" "Parks Oper"
# [73] "Contact 'B" "Environmen" "Vuntut Gwi" "Indigenous" "Parks Cana" "Canadian W"

# Go ahead and mark "City Land" as "Local", too and "State Park" & "State Land" 
# as "State"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 9) == "City Land",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 10) == "State Park",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 10) == "State Land",
                               true = "State",
                               false = AGNCY_SHORT))

# See what is left, letting the substring get a little longer
agency_start <- substr(x = agencies[is.na(agencies$AGNCY_SHORT), "MGMT_AGNCY"], 
                       start = 1,
                       stop = 12)
# unique(agency_start)
# OK, lots there, including several that start with "Private - " look at those
private <- agencies %>%
  filter(substr(MGMT_AGNCY, 1, 7) == "Private")
# unique(sort(private$MGMT_AGNCY))
# With one exception, we can mark these all as "Private" in AGNCY_SHORT; 
# Not sure about: "Private and PEI Department of Environment, Water & Climate Change" 
# private %>% filter(MGMT_AGNCY == "Private and PEI Department of Environment, Water & Climate Change")
# Leave it for now

# Update all but the Private / PEI joint as "Private"
agencies <- agencies %>%
  # Start by marking anything starting with "Private" as such
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 7) == "Private",
                               true = "Private",
                               false = AGNCY_SHORT)) %>%
  # Now flip the PEI one back
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 15) == "Private and PEI",
                               true = NA_character_,
                               false = AGNCY_SHORT))

# Look again now that the private ones are updated  
agency_start <- substr(x = agencies[is.na(agencies$AGNCY_SHORT), "MGMT_AGNCY"], 
                       start = 1,
                       stop = 12)

# unique(sort(agency_start))

# Set County Land to "Local"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 11) == "County Land",
                               true = "Local",
                               false = AGNCY_SHORT))

# Check some common U.S. federal entities to see if anything else is in there
us_fed <- c("Forest Service", "State Department", "National Parks Service",
            "U.S. Fish", "Bureau of Reclamation", "Army Corps of Engineers",
            "Bureau of Land Management", "Department of Agriculture")
us_fed_str <- paste0(us_fed, collapse = "|")
agencies_us_fed <- agencies[grepl(pattern = us_fed_str, x = agencies$MGMT_AGNCY),]

# unique(sort(agencies_us_fed$MGMT_AGNCY))
# Variety of updates here; often list the district or other sub-set along with 
# the agency name. All of these string starts should be marked as "Federal":
# + "Agricultural Research Service"
# + "Army Corps of Engineers"
# + "Bureau of Land Management"
# + "Forest Service"
# + "U.S. Fish & Wildlife Service"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 29) == "Agricultural Research Service",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 23) == "Army Corps of Engineers",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 25) == "Bureau of Land Management",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 21) == "Bureau of Reclamation",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 14) == "Forest Service",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 28) == "U.S. Fish & Wildlife Service",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 21) == "Department of Defense",
                               true = "Federal",
                               false = AGNCY_SHORT))

# These, however, are "State" level agencies (starts of strings):
# + "Joint - Tennessee Wildlife Resources Agency/State Department of Agriculture"
# + "Other or Unknown State Land"
# + "State Department of Conservation"
# + "State Department of Land"
# + "State Department of Natural Resources"
# + "State Fish and Wildlife"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 75) == "Joint - Tennessee Wildlife Resources Agency/State Department of Agriculture",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 27) == "Other or Unknown State Land",
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
                               false = AGNCY_SHORT))

# What's left?
agency_start <- substr(x = agencies[is.na(agencies$AGNCY_SHORT), "MGMT_AGNCY"], 
                       start = 1,
                       stop = 19)

# unique(sort(agency_start))

# Look at "Government of..." entries
govt_of <- agencies[grepl(pattern = "Government of ",x = agencies$MGMT_AGNCY), ]

# unique(sort(govt_of$MGMT_AGNCY))
# Will want to grepl these:
# Environment Yukon
# Government of New Brunswick
# Government of British Columbia
# Government of Manitoba
# Government of Newfoundland and Labrador
# Government of Nunavut
# Government of Saskatchewan
# Government of the Northwest Territories
# Government of Quebec

can_state <- c("New Brunswick", "British Columbia", "Manitoba", 
               "Newfoundland and Labrador", "Nunavut", "Saskatchewan",
               "the Northwest Territories", "Quebec")
can_state <- paste0("Government of ", can_state)
can_state <- c("Environment Yukon", can_state)
can_state_str <- paste0(can_state, collapse = "|")

# Base R fun!
agencies$AGNCY_SHORT[grepl(pattern = can_state_str,
                           x = agencies$MGMT_AGNCY)] <- "State"
# Quick reality check
# agencies %>% 
#   filter(AGNCY_SHORT == "State") %>% 
#   filter(COUNTRY == "CAN") %>% 
#   head(n = 40)

agency_start <- substr(x = agencies[is.na(agencies$AGNCY_SHORT), "MGMT_AGNCY"], 
                       start = 1,
                       stop = 19)
# unique(sort(agency_start))

# Let's look at those "Joint..." entries
joint <- agencies$MGMT_AGNCY[substr(x = agencies$MGMT_AGNCY,
                                    start = 1,
                                    stop = 7) == "Joint -"]
# unique(sort(joint))

# These are Local (city + county and the ilk) (substr)
# "Joint - City"
# "Joint - Town of Killingly"
# "Board of Water Commissioners City and County of Denver"
# "Joint - San Francisco"
# "Joint - Suffolk County"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 12) == "Joint - City",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 25) == "Joint - Town of Killingly",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 54) == "Board of Water Commissioners City and County of Denver",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 21) == "Joint - San Francisco",
                               true = "Local",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 22) == "Joint - Suffolk County",
                               true = "Local",
                               false = AGNCY_SHORT))

# Local grepl
# "City of Golden Valley"
agencies$AGNCY_SHORT[grepl(pattern = "City of Golden Valley",
                           x = agencies$MGMT_AGNCY)] <- "Local"

# These are State (grepl)
# "Kentucky State Nature Preserves Commission"
# "KY State Nature Preserves Commission"
# "State Department of Agriculture, Division of Forestry"
agencies$AGNCY_SHORT[grepl(pattern = "Kentucky State Nature Preserves Commission",
                           x = agencies$MGMT_AGNCY)] <- "State"
agencies$AGNCY_SHORT[grepl(pattern = "KY State Nature Preserves Commission",
                           x = agencies$MGMT_AGNCY)] <- "State"
agencies$AGNCY_SHORT[grepl(pattern = "State Department of Agriculture, Division of Forestry",
                           x = agencies$MGMT_AGNCY)] <- "State"

# State substr
# "Joint - State of TN"
# "Joint - Western Kentucky University"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 19) == "Joint - State of TN",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 35) == "Joint - Western Kentucky University",
                               true = "State",
                               false = AGNCY_SHORT))

# Federal (grepl)
# "US Forest Service"
# "National Park Service"
agencies$AGNCY_SHORT[grepl(pattern = "US Forest Service",
                           x = agencies$MGMT_AGNCY)] <- "Federal"
agencies$AGNCY_SHORT[grepl(pattern = "National Park Service",
                           x = agencies$MGMT_AGNCY)] <- "Federal"


# Starts with "Manitoba" = State
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 8) == "Manitoba",
                               true = "State",
                               false = AGNCY_SHORT))

agency_start <- substr(x = agencies[is.na(agencies$AGNCY_SHORT), "MGMT_AGNCY"], 
                       start = 1,
                       stop = 19)
# unique(sort(agency_start))

# Diving into uncategorized Canadian areas
can_uncat <- agencies %>%
  filter(COUNTRY == "CAN") %>%
  filter(is.na(AGNCY_SHORT)) %>%
  select(MGMT_AGNCY) %>%
  distinct() %>%
  arrange()

# can_uncat

# Federal Canadas (substr)
# "Vuntut Gwitchin First Nation"
# "Indigenous Government"
# "Parks Canada Agency"
# "Canadian Wildlife Service"
# "National Capital Commission (NCC)"
# "Indigenous and Northern Affairs Canada"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 28) == "Vuntut Gwitchin First Nation",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 21) == "Indigenous Government",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 19) == "Parks Canada Agency",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 25) == "Canadian Wildlife Service",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 33) == "National Capital Commission",
                               true = "Federal",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 38) == "Indigenous and Northern Affairs Canada",
                               true = "Federal",
                               false = AGNCY_SHORT))

# State Canadas (substr)
# "Ministère de l'Environnement"  
# "Ministry of Natural Resources and Forestry"
# "Nova Scotia Department of Lands and Forestry"
# "Nova Scotia Environment"
# "Ontario Parks"
# "Parks Operations Division, Alberta Environment and Parks"
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 28) == "Ministère de l'Environnement",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 42) == "Ministry of Natural Resources and Forestry",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 44) == "Nova Scotia Department of Lands and Forestry",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 23) == "Nova Scotia Environment",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 13) == "Ontario Parks",
                               true = "State",
                               false = AGNCY_SHORT)) %>%
  mutate(AGNCY_SHORT = if_else(substr(MGMT_AGNCY, 1, 56) == "Parks Operations Division, Alberta Environment and Parks",
                               true = "State",
                               false = AGNCY_SHORT))

# Status check
agency_start <- substr(x = agencies[is.na(agencies$AGNCY_SHORT), "MGMT_AGNCY"], 
                       start = 1,
                       stop = 19)
# unique(sort(agency_start))

# What are these "Unknown"?

unknown <- agencies$MGMT_AGNCY[substr(agencies$MGMT_AGNCY, 1, 7) == "Unknown"]
# unique(sort(unknown))
# [1] "Unknown"                                          "Unknown - Archbold Expeditions, Inc."            
# [3] "Unknown - Aspen Center for Environmental Studies" "Unknown - B. K. I., Inc., Consulting Ecologists" 
# [5] "Unknown - Brushy Creek MUD"                       "Unknown - Cardno ENTRIX"                         
# [7] "Unknown - EarthBalance"                           "Unknown - Florida Mitigation Providers, LLC"     
# [9] "Unknown - Foothills"                              "Unknown - FPL Everglades Mitigation Bank"        
# [11] "Unknown - Quest Ecology Inc."                     "Unknown - The Conservation Fund"                 
# [13] "Unknown - Trout Lake Nature Center, Inc."         "Unknown - Unspecified"                           
# [15] "Unknown - Various"                                "Unknown - Westervelt Ecological Services"   


# NGO:
# "Nature Manitoba"


################################################################################
# Some cludgey ones
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(grepl(pattern = "National Commission of Natural Protected Areas", 
                                     x = MGMT_AGNCY),
                               true = "Federal",
                               false = AGNCY_SHORT))
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(grepl(pattern = "National Capital Commission", 
                                     x = MGMT_AGNCY),
                               true = "Federal",
                               false = AGNCY_SHORT))
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(grepl(pattern = "National Capital Commission", 
                                     x = MGMT_AGNCY),
                               true = "Federal",
                               false = AGNCY_SHORT))
agencies <- agencies %>%
  mutate(AGNCY_SHORT = if_else(grepl(pattern = "Indigenous and Northern Affairs Canada", 
                                     x = MGMT_AGNCY),
                               true = "Federal",
                               false = AGNCY_SHORT))

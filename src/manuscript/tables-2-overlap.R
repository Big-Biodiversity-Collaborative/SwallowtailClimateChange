# Create table of predicted suitability areas and overlap with >= host
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-10-21

library(dplyr)

# Table with four columns:
# | species | Area (km2) | Area (km2) >= 1 host | % >= 1 host |
# Area and % info is in output/summary-stats/overlap-summary-allspp.csv
# Want rows that are:
#   distribution = "total insect"
#   climate = "current"
# Relevant columns are:
#   area: for km^2 area of suitable area for insect
#   pinsect_withhost: for percent of range suitable for at least one host

areas <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")
# Just pull out the stuff from the table of interest
areas <- areas %>%
  filter(distribution == "total insect") %>%
  filter(climate == "current") %>%
  select(insect, area, pinsect_withhost)

# Add the column of suitable area with >= host and rearrange columns
areas <- areas %>%
  mutate(area_withhost = area * (pinsect_withhost/100)) %>%
  select(insect, area, area_withhost, pinsect_withhost)

# Some number formatting, reducing decimal points
areas <- areas %>%
  mutate(area_withhost = round(area_withhost, 0),
         pinsect_withhost = round(pinsect_withhost, 2))

# Replace Papilio with P.
areas <- areas %>%
  mutate(insect = gsub(pattern = "Papilio",
                       replacement = "P.",
                       x = insect))

# Rename columns to be more manuscript-friendly
areas <- areas %>%
  rename(Species = insect,
         `Area (km2)` = area,
         `Area with at least one host (km2)` = area_withhost,
         `% Overlap with at least one host` = pinsect_withhost)

write.csv(x = areas,
          row.names = FALSE,
          file = "output/manuscript/Table-Area-Overlap.csv")


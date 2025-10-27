# Investigation of changes underlying gain/loss of area in forecasts
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-10-27

# Want to know if gain/loss of suitable area for an insect is due to gain/loss
# of suitable area for the insect, gain/loss of suitable area for at least one 
# host plant, or gain/loss for both.

# Incoming rasters have the following values:
# 0 = (Insect and hosts absent) Insect and all host plants predicted absent
# 1 = (1 host only) Insect predicted absent, only 1 host predicted present
# 2 = (2 or more hosts) Insect predicted absent, >= 2 hosts predicted present
# 3 = (Insect, no hosts) Insect predicted present, all hosts predicted absent
# 4 = (Insect, only 1 host) Insect and only 1 host predicted present
# 5 = (Insect, 2 or more hosts) Insect and >= 2 hosts predicted present

current <- readRDS(paste0("output/overlaps/", nice_name,
                          "-overlap-current.rds"))
future <- readRDS(paste0("output/overlaps/", nice_name,
                         "-overlap-", climate_model, ".rds"))

# Start by reclassifying them, current:
# {0} -> 0 (suitable for neither)
# {1,2} -> 1 (suitable for host, but not insect)
# {3} -> 2 (suitable for insect, but not hosts)
# {4,5} -> 4 (suitable for both insect and host)

# Forecast:
# {0} -> 0 (suitable for neither)
# {1,2} -> 8 (suitable for host, but not insect)
# {3} -> 16 (suitable for insect, but not hosts)
# {4,5} -> 32 (suitable for both insect and host)


# Expand (both?) to get right extent

# Add current and forecast rasters together to get new raster with combined 
# informative values. We care about cells with only certain values. Those 
# dealing with loss of area marked with (LOSS), gain of area marked with (GAIN)
# Current | Forecast
#       0 |        0 = 0
#       1 |        0 = 1
#       2 |        0 = 2
#       4 |        0 = 4 LOSS - both
#       0 |        8 = 8
#       1 |        8 = 9
#       2 |        8 = 10
#       4 |        8 = 12 LOSS - insect only
#       0 |        16 = 16
#       1 |        16 = 17
#       2 |        16 = 18
#       4 |        16 = 20 LOSS - plant only
#       0 |        32 = 32 GAIN - both
#       1 |        32 = 33 GAIN - insect only
#       2 |        32 = 34 GAIN - plant only
#       4 |        32 = 36

# Calculate area for each of these LOSS/GAIN categories:
# area_lost_both
# area_lost_insect
# area_lost_plant
# area_gained_both
# area_gained_insect
# area_gained_plant

# Update data frame with above stats for the respective forecast climate model

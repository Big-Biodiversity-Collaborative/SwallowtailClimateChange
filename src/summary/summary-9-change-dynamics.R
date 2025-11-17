# Investigation of changes underlying gain/loss of area in forecasts
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-10-27

# Want to know if gain/loss of suitable area for an insect is due to gain/loss
# of suitable area for the insect, gain/loss of suitable area for at least one 
# host plant, or gain/loss for both.

require(dplyr)
require(tidyr)
require(terra)
require(ggplot2)
require(DirichletReg)
require(ggtern)

source(file = "functions/get_colors.R")

# Get list of insects
ih <- read.csv(file = "data/insect-host.csv")
insects <- unique(ih$insect)

# Get list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")
# For our purposes, we do not need the current climate model
climate_models <- climate_models %>%
  filter(name != "current")

# data frame to hold values
areas_df <- tidyr::crossing(insect = insects, 
                            climate = climate_models$name) %>%
  mutate(area_current = NA_real_,
         area_lost_both = NA_real_,
         area_lost_insect = NA_real_,
         area_lost_plant = NA_real_,
         area_gained_both = NA_real_,
         area_gained_insect = NA_real_,
         area_gained_plant = NA_real_)

for (insect in insects) {
  nice_name <- tolower(gsub(pattern = " ",
                            replacement = "_",
                            x = insect))
  
  # Incoming rasters have the following values:
  # 0 = (Insect and hosts absent) Insect and all host plants predicted absent
  # 1 = (1 host only) Insect predicted absent, only 1 host predicted present
  # 2 = (2 or more hosts) Insect predicted absent, >= 2 hosts predicted present
  # 3 = (Insect, no hosts) Insect predicted present, all hosts predicted absent
  # 4 = (Insect, only 1 host) Insect and only 1 host predicted present
  # 5 = (Insect, 2 or more hosts) Insect and >= 2 hosts predicted present
  current_file <- paste0("output/overlaps/", nice_name,
                         "-overlap-current.rds")
  if (file.exists(current_file)) {
    message("Calculating change dynamics for ", insect, "\n")
    current <- readRDS(current_file)
    
    # Start by reclassifying them, current:
    # {0} -> 0 (suitable for neither)
    # {1,2} -> 1 (suitable for host, but not insect)
    # {3} -> 2 (suitable for insect, but not hosts)
    # {4,5} -> 4 (suitable for both insect and host)
    rcl_mat_current <- matrix(data = c(0, 1, 2, 3, 4, 5,
                                       0, 1, 1, 2, 4, 4),
                              nrow = 6, byrow = FALSE)
    current <- terra::classify(x = current,
                               rcl = rcl_mat_current)
    
    # While we are here, we can calculate current area suitable with at least one 
    # host
    current_areas <- round(terra::expanse(current, unit = "km", byValue = TRUE))
    area_current <- 0
    if (4 %in% current_areas$value) {
      area_current <- current_areas$area[current_areas$value == 4]
    }
    # And update rows for this species (repeated values, but fine)
    areas_df$area_current[areas_df$insect == insect] <- area_current
    
    for (climate_model in climate_models$name) {
      
      row_index <- which(areas_df$insect == insect &
                           areas_df$climate == climate_model)
      
      future_file <- paste0("output/overlaps/", nice_name,
                            "-overlap-", climate_model, ".rds")
      if (file.exists(future_file)) {
      future <- readRDS(future_file)
      
      # Forecast:
      # {0} -> 0 (suitable for neither)
      # {1,2} -> 8 (suitable for host, but not insect)
      # {3} -> 16 (suitable for insect, but not hosts)
      # {4,5} -> 32 (suitable for both insect and host)
      rcl_mat_future <- matrix(data = c(0, 1, 2, 3, 4, 5,
                                        0, 8, 8, 16, 32, 32),
                               nrow = 6, byrow = FALSE)
      future <- terra::classify(x = future,
                                rcl = rcl_mat_future)
      
      # Expand current to get right extent
      current <- extend(current, future)
      
      # Add current and forecast rasters together to get new raster with combined 
      # informative values. We care about cells with only certain values. Those 
      # dealing with loss of area marked with (LOSS), gain of area marked with (GAIN)
      # Current | Forecast
      #       0 |        0 = 0
      #       1 |        0 = 1
      #       2 |        0 = 2
      #       4 |        0 = 4 LOSS: both -> area_lost_both
      #       0 |        8 = 8
      #       1 |        8 = 9
      #       2 |        8 = 10
      #       4 |        8 = 12 LOSS: insect only -> area_lost_insect
      #       0 |        16 = 16
      #       1 |        16 = 17
      #       2 |        16 = 18
      #       4 |        16 = 20 LOSS: plant only -> area_lost_plant
      #       0 |        32 = 32 GAIN: both -> area_gained_both
      #       1 |        32 = 33 GAIN: insect only -> area_gained_insect
      #       2 |        32 = 34 GAIN: plant only -> area_gained_plant
      #       4 |        32 = 36
      raster_sum <- sum(terra::rast(list(current, future)), 
                        na.rm = TRUE)
      
      # Make things simpler for us by only keeping those values of interest and give 
      # those things names to make it easier
      
      area_names <- data.frame(id = c(4, 12, 20, 32, 33, 34),
                               name = c("area_lost_both",
                                        "area_lost_insect",
                                        "area_lost_plant",
                                        "area_gained_both",
                                        "area_gained_insect",
                                        "area_gained_plant"))
      
      raster_sum[!(raster_sum %in% area_names$id)] <- NA
      
      # Calculate area for each of the above categories
      areas <- round(terra::expanse(raster_sum, unit = "km", byValue = TRUE))
      
      # Add area values with the names and transform to wide
      areas <- areas %>%
        full_join(area_names, by = join_by(value == id)) %>%
        select(name, area) %>%
        pivot_wider(names_from = name, values_from = area)
      
      # Add those values to the larger data frame. This is so ugly.
      areas_df$area_lost_both[row_index] <- areas$area_lost_both
      areas_df$area_lost_insect[row_index] <- areas$area_lost_insect
      areas_df$area_lost_plant[row_index] <- areas$area_lost_plant
      areas_df$area_gained_both[row_index] <- areas$area_gained_both
      areas_df$area_gained_insect[row_index] <- areas$area_gained_insect
      areas_df$area_gained_plant[row_index] <- areas$area_gained_plant

      } # End conditional for future climate mdoel raster exists for this sp.
      else {
        message("No ", climate_model, " raster on file for ", insect, "\n")
      }
    } # end iterating over climate models
    
  } # End conditional for current raster exists for this species
  else {
    message("No current climate raster on file for ", insect, "\n")
  }
} # end iterating over insects

# Before sending to file, let's remove "emsemble_" from all those climate names
areas_df <- areas_df %>%
  mutate(climate = gsub(pattern = "ensemble_",
                        replacement = "",
                        x = climate))

write.csv(file = "output/summary-stats/overlap-changes.csv",
          x = areas_df,
          row.names = FALSE)

################################################################################
# Now look at stats, in terms of number of host plants

areas_df <- read.csv(file = "output/summary-stats/overlap-changes.csv")

# Get insect host plant list
ih <- read.csv(file = "data/insect-host.csv")

# Want to drop those plant species that were not included (too few 
# observations, which we will use as proxy to say plant is not available as a 
# host in North America)
pa_summary <- read.csv(file = "data/gbif-pa-summary.csv")
# Drop species that we did not create a presence/absence file for
pa_summary <- pa_summary[pa_summary$pa_csv == "yes", ]
keep_plant <- ih$host_accepted %in% pa_summary$species

# Drop those records for the plants we just excluded 
ih <- ih[keep_plant, ]

# Do host plant species count
host_counts <- ih %>%
  group_by(insect) %>%
  summarize(num_hosts = n())

# Join the count data with the area change calculations
areas_df <- areas_df %>%
  left_join(host_counts, by = c("insect" = "insect"))

# Drop P. aristodemus
areas_df <- areas_df %>%
  filter(insect != "Papilio aristodemus")

# For area_lost/area_gained, NA should be 0
areas_df <- areas_df %>%
  mutate(across(area_lost_both:area_gained_plant,  
                ~ if_else(is.na(.x),
                        0,
                        .x)))
         
# Hypothesis: specialists will have greater proportion area lost to plant 
# (plant and insect) vs area lost only to bug

# Considering areas that become unsuitable for any host plant species 
# (regardless of whether area is or is not suitable for insect)
ggplot(data = areas_df %>% 
         filter(climate == "ssp370_2041") %>%
         filter(insect != "Papilio appalachiensis"),
       mapping = aes(x = num_hosts, y = (area_lost_both + area_lost_plant)/area_current)) + 
  geom_point()

ggplot(data = areas_df %>% 
         filter(climate == "ssp370_2041") %>%
         filter(insect != "Papilio appalachiensis") %>%
         filter(insect != "Papilio brevicauda"),
       mapping = aes(x = num_hosts, y = (area_gained_both + area_gained_plant)/area_current)) + 
  geom_point()

# Areas lost
# Longer data for bar chart
areas_lost_long <- areas_df %>%
  select(-c(area_current, num_hosts, area_gained_both, area_gained_insect, area_gained_plant)) %>%
  # Want to plot proportions
  mutate(total_lost = area_lost_both + area_lost_plant + area_lost_insect) %>%
  mutate(prop_lost_both = area_lost_both/total_lost,
         prop_lost_insect = area_lost_insect/total_lost,
         prop_lost_plant = area_lost_plant/total_lost) %>%
  select(-c(area_lost_both, area_lost_insect, area_lost_plant, total_lost)) %>%
  pivot_longer(cols = -c(insect, climate),
               names_to = "type",
               values_to = "area")

areas_lost_long <- areas_lost_long %>%
  mutate(type = factor(type, levels = c("prop_lost_plant",
                                        "prop_lost_both",
                                        "prop_lost_insect")))

ggplot(data = areas_lost_long %>%
         filter(insect != "Papilio appalachiensis") %>%
         filter(climate == "ssp370_2041"),
       mapping = aes(x = insect)) +
  geom_bar(stat = "identity",
           mapping = aes(y = area, fill = type)) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"))

# Areas gained
areas_gained_long <- areas_df %>%
  select(-c(area_current, num_hosts, area_lost_both, area_lost_insect, area_lost_plant)) %>%
  # Want to plot proportions
  mutate(total_gained = area_gained_both + area_gained_plant + area_gained_insect) %>%
  mutate(prop_gained_both = area_gained_both/total_gained,
         prop_gained_insect = area_gained_insect/total_gained,
         prop_gained_plant = area_gained_plant/total_gained) %>%
  select(-c(area_gained_both, area_gained_insect, area_gained_plant, total_gained)) %>%
  pivot_longer(cols = -c(insect, climate),
               names_to = "type",
               values_to = "area")

areas_gained_long <- areas_gained_long %>%
  mutate(type = factor(type, levels = c("prop_gained_plant",
                                        "prop_gained_both",
                                        "prop_gained_insect")))

ggplot(data = areas_gained_long %>%
         filter(insect != "Papilio appalachiensis") %>%
         filter(climate == "ssp370_2041"),
       mapping = aes(x = insect)) +
  geom_bar(stat = "identity",
           mapping = aes(y = area, fill = type)) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"))

# A little t-test comparing prop_lost_insect to prop_gained_insect
prop_lost <- areas_lost_long %>%
  filter(insect != "Papilio appalachiensis") %>%
  filter(climate == "ssp370_2041")
prop_gained <- areas_gained_long %>%
  filter(insect != "Papilio appalachiensis") %>%
  filter(climate == "ssp370_2041")

# t.test(x = prop_lost$area[prop_lost$type == "prop_lost_insect"],
#        y = prop_gained$area[prop_gained$type == "prop_gained_insect"])
# t = 2.5571, df = 25.881, p-value = 0.01677
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.05073895 0.46705754
# sample estimates:
#   mean of x mean of y 
# 0.7575052 0.4986070 

################################################################################
# A more appropriate approach is Dirichlet regression, a multivariate extension 
# of the beta regression approach for proportions

# Want data in the following format:
# insect | plant | both | type
# where the first three columns are proportions that sum to 1, and type 
# indicates gain or loss
# Start by calculating proportions
areas_prop_df <- areas_df %>%
  select(-c(num_hosts)) %>%
  mutate(area_lost = area_lost_both + area_lost_insect + area_lost_plant,
         area_gained = area_gained_both + area_gained_insect + area_gained_plant) %>%
  mutate(across(area_lost_both:area_lost_plant,
                ~ .x/area_lost)) %>%
  mutate(across(area_gained_both:area_gained_plant,
                ~ .x/area_gained))
  # mutate(across(area_lost_both:area_lost_plant),
  #        ~ .x/sum(area_lost_both:area_lost_plant)) %>%
  # mutate(across(area_gained_both:area_gained_plant),
  #        ~ .x/sum(area_gained_both:area_gained_plant))
  


head(areas_prop_df %>% filter(insect == "Papilio brevicauda"))

# We want to change to longer format now
areas_prop_long <- areas_prop_df %>%
  pivot_longer(cols = area_lost_both:area_gained_plant) %>%
  mutate(type = if_else(substr(x = name, start = 1, stop = 9) == "area_lost",
                        true = "loss",
                        false = "gain")) %>%
  mutate(name = gsub(pattern = "area_lost_",
                     replacement = "",
                     x = name)) %>%
  mutate(name = gsub(pattern = "area_gained_", # Lazy, regex-hating bum
                     replacement = "",
                     x = name))

# Now we go back to wider format (seems inefficient?)
areas_prop_wide <- areas_prop_long %>%
  select(-c(area_current, area_lost, area_gained)) %>%
  rename(species = insect) %>% # otherwise end up with two "insect" columns
  pivot_wider(id_cols = c(species, climate, type),
              names_from = name,
              values_from = value)

head(areas_prop_long)
head(areas_prop_wide)

# For now, just look at ssp370_2041, remove P. appalachiensis, as there are no 
# gains, and drop species and climate info from data frame
areas_prop_ssp370_2041 <- areas_prop_wide %>%
  filter(climate == "ssp370_2041") %>%
  filter(species != "Papilio appalachiensis") %>%
  select(-c(species, climate)) %>%
  as.data.frame()

# Reality check
prop_sums <- rowSums(areas_prop_ssp370_2041[, 2:4])
prop_sums

# Now (finally!) we do Dirichlet regression
# Prepare the data (should be ready)
# Warning in DR_data(areas_prop_ssp370_2041[, 2:4]) :
#   some entries are 0 or 1 => transformation forced
areas_prop_DR <- DR_data(areas_prop_ssp370_2041[, 2:4])

areas_DR <- DirichReg(areas_prop_DR ~ type, areas_prop_ssp370_2041)
summary(areas_DR)
plot(areas_prop_DR) # TODO: Maybe look to ggtern for a better plot


gl_colors <- get_colors(palette = "distdelta")
ggtern(data = areas_prop_ssp370_2041, 
       mapping = aes(x = insect, y = both, z = plant)) +
  geom_point(mapping = aes(color = type)) +
  tern_limit(T = 1.05, L = 1.05, R = 1.05) +
  scale_color_manual(values = c(gl_colors[c(3, 2)])) +
  theme_bw() + 
  theme_showarrows()

################################################################################
# What if we did a binary approach, and just looked at plant or insect change, 
# ditching the "both" consideration...
# But then props will sum to greater than 1...
# Seems like a non-parametric approach would be able to handle this just fine


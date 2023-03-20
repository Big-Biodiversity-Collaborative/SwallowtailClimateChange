# Create consensus rasters (identifying the number of SDMs that predict 
# presence) for a set of species and climate scenarios
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-03-20

require(tidyr)
require(stringr)
require(terra)
require(dplyr)

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Load information about data availability for each species
species_info <- read.csv("data/gbif-pa-summary.csv")

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")

# List of SDMs to include
sdms <- c("brt", "gam", "lasso", "maxent-tune", "rf")

# Logical indicating whether to create consensus rasters for all species 
# or just a subset of insects and their host plants
all_insects <- TRUE

# If not all insects, identify which insects (and their host plants) to include
insects <- c("Papilio appalachiensis")

# Extract all species names
if (all_insects) {
  species <- unique(c(ih$insect, ih$host_accepted))
} else {
  plants <- ih$host_accepted[ih$insect %in% insects]
  species <- unique(c(insects, plants))
}

# Remove species from list that have an insufficient number of filtered 
# occurrence records and therefore should not be included in analyses. 
# (Technically, they shouldn't have distribution rasters, but some obsolete 
# files may still remain in the output/distributions folder)
exclude <- species_info$species[species_info$pa_csv == "no"]
species <- species[!species %in% exclude]

nice_names <- species %>%
  str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# Identify files with predicted distributions
all_dist_files <- list.files("output/distributions/", 
                             pattern = paste(nice_names, collapse = "|"),
                             full.names = TRUE)
all_dist_files <- str_subset(all_dist_files, paste(sdms, collapse = "|"))

# Create table to hold summary stats for predicted distributions from each SDM
summary <- as.data.frame(expand_grid(species = species, 
                                     clim_model = climate_models$name,
                                     sdm = sdms)) %>%
  mutate(area = NA,
         perc_current = NA, # % of current distribution predicted suitable in future
         area_ratio = NA)   # ratio of areas predicted suitable (future/current)

# Create table that contains information about the consensus rasters
summary_consensus <- as.data.frame(expand_grid(species = species, 
                                               clim_model = climate_models$name)) %>%
  mutate(ncells = NA, # number of grid cells in prediction area
         sdm0 = NA,   # percent of cells that 0 SDMs predicted suitable
         sdm1 = NA,   # percent of cells that 1 SDM predicted suitable
         sdm2 = NA,   # percent of cells that 2 SDMs predicted suitable
         sdm3 = NA,   # percent of cells that 3 SDMs predicted suitable
         sdm4 = NA,   # percent of cells that 4 SDMs predicted suitable
         sdm5 = NA)   # percent of cells that 5 SDMs predicted suitable

for (i in 1:length(species)) {
  
  spp_dist_files <- str_subset(all_dist_files, nice_names[i])
  
  if (length(spp_dist_files) == 0) {
    message("*** No distribution files for ", species[i], 
            ". Did not create consensus rasters.")
  } else {
    
    cat(paste0("Creating consensus rasters for ", species[i], ".\n"))
    
    clim_model <- climate_models$name[1]
    dist_files_current <- str_subset(spp_dist_files, clim_model)
    
    if (length(dist_files_current) < length(sdms)) {
      message("*** Missing at least one SDM distribution file for ", 
              species[i], ", ", clim_model, 
              ". Did not create consensus raster.")
    } else {
      
      dist_list_current <- list()
      
      for (j in 1:length(dist_files_current)) {
        dist_list_current[[j]] <- readRDS(dist_files_current[j])
        row_index <- which(summary$species == species[i] & 
                             summary$clim_model == clim_model & 
                             summary$sdm == sdms[j])
        # Calculate land area (in sq km) and add to summary
        area <- terra::expanse(dist_list_current[[j]], 
                               unit = "km",
                               byValue = TRUE)
        summary$area[row_index] <- round(area[area[,"value"] == 1,"area"])
      }  
      
      consensus <- rast(dist_list_current)
      
      # Create a raster with cell values equal to the number of SDMs
      # predicting presence (or suitability)
      consensus <- sum(consensus)
      
      counts <- freq(consensus)
      counts$percent <- round(counts$count / sum(counts$count) * 100, 1)
      row_index <- which(summary_consensus$species == species[i] &
                           summary_consensus$clim_model == clim_model)
      summary_consensus$ncells[row_index] <- sum(counts$count)
      count_vec <- rep(0, 6)
      count_vec[counts$value + 1] <- counts$percent
      summary_consensus[row_index, paste0("sdm", 0:5)] <- count_vec
      
      consensus_file <- paste0("output/consensus-rasters/",
                               nice_names[i],
                               "-consensus-",
                               clim_model,
                               ".rds")
      saveRDS(object = consensus,
              file = consensus_file)
    }
    
    for (clim_model in climate_models$name[2:5]) {
      dist_files <- str_subset(spp_dist_files, clim_model)
      
      if (length(dist_files) < length(sdms)) {
        message("*** Missing at least one SDM distribution file for ", 
                species[i], ", ", clim_model, 
                ". Did not create consensus raster.")
      } else {
        
        dist_list <- list()
        
        for (j in 1:length(dist_files)) {
          dist_list[[j]] <- readRDS(dist_files[j])
          row_index <- which(summary$species == species[i] &
                               summary$clim_model == clim_model & 
                               summary$sdm == sdms[j])
          
          # Calculate land area (in sq km) and add to summary
          area <- terra::expanse(dist_list[[j]], 
                                 unit = "km",
                                 byValue = TRUE)
          if (max(area[, "value"]) == 0) {
            summary$area[row_index] <- 0
            summary$perc_current[row_index] <- 0
            summary$area_ratio[row_index] <- 0
          } else {
            summary$area[row_index] <- round(area[area[,"value"] == 1,"area"])
            area_current <- summary$area[summary$species == species[i] & 
                                           summary$clim_model == "current" & 
                                           summary$sdm == sdms[j]]
            summary$area_ratio[row_index] <- 
              round(summary$area[row_index] / area_current, 2)
            
            # Calculate percent of current distribution that's suitable in future
            future <- terra::crop(dist_list[[j]], dist_list_current[[j]])
            overlay <- terra::expanse(dist_list_current[[j]] + future, 
                                      unit = "km",
                                      byValue = TRUE)
            if (max(overlay[, "value"]) < 2) {
              summary$perc_current[row_index] <- 0
            } else {
              overlay <- round(overlay[overlay[,"value"] == 2,"area"])
              perc_current <- overlay/area_current * 100
              summary$perc_current[row_index] <- round(perc_current, 2)
            }
          }
        } #j
        
        consensus <- rast(dist_list)
        
        # Create a raster with cell values equal to the number of SDMs
        # predicting presence (or suitability)
        consensus <- sum(consensus)
        
        counts <- freq(consensus)
        counts$percent <- round(counts$count / sum(counts$count) * 100, 1)
        row_index <- which(summary_consensus$species == species[i] &
                             summary_consensus$clim_model == clim_model)
        summary_consensus$ncells[row_index] <- sum(counts$count)
        count_vec <- rep(0, 6)
        count_vec[counts$value + 1] <- counts$percent
        summary_consensus[row_index, paste0("sdm", 0:5)] <- count_vec
        
        consensus_file <- paste0("output/consensus-rasters/",
                                 nice_names[i],
                                 "-consensus-",
                                 clim_model,
                                 ".rds")
        saveRDS(object = consensus,
                file = consensus_file)
      }
    }
  }
}

# Write summary tables to file
datestamp <- Sys.Date()
datestamp <- str_remove_all(datestamp, "-")

if (all_insects) {
  spp <- "allspp"
} else {
  if (length(insects) == 1) {
    spp <- "1insect"
  } else {
    spp <- paste0(length(insects), "insects")
  }
}

filename1 <- paste0("output/summary-stats/distribution-summaries-", 
                    spp, "-", datestamp, ".csv")
write.csv(summary, 
          file = filename1,
          row.names = FALSE)

filename2 <- paste0("output/summary-stats/consensus-summaries-",
                    spp, "-", datestamp, ".csv")
write.csv(summary_consensus,
          file = filename2,
          row.names = FALSE)


# Script to create figures, tables, summary stats for manuscript
# Erin Zylstra
# ezylstra@arizona.edu
# 2024-02-15

require(dplyr)
require(stringr)
require(terra)
require(ggplot2)
require(tidyterra)
require(gridExtra)
require(cowplot)

# Logical indicating whether to recreate maps/tables if they already exist
replace <- TRUE

# Load insect-host information and gbif data summaries
ih <- read.csv("data/insect-host.csv")
gbif <- read.csv("data/gbif-pa-summary.csv")

# Load summary stats
stats <- read.csv("output/summary-stats/overlap-summary-allspp.csv")

# Base of output filenames
output_basename <- "output/manuscript/"

# Grab spatial files with political boundaries
countries <- vect("data/political-boundaries/countries.shp")
countries3 <- filter(countries, countries$adm0_a3 %in% c("USA", "CAN", "MEX"))
states <- vect("data/political-boundaries/states.shp")  

# Summary: % of species that have SDMs built with <10,000 background points ---#

  spp_sdms <- gbif %>%
    filter(pa_csv == "yes")
  sum(spp_sdms$n_background < 10000) / nrow(spp_sdms) * 100

# Table: Papilio species ------------------------------------------------------#

  # Get number of filtered records for each swallowtail spp
  insects <- gbif %>%
    filter(str_detect(species, "Papilio")) 
  filter(insects, n_filtered < 40)
  
  insects <- insects %>%
    filter(pa_csv == "yes") %>%
    select(species, n_filtered)
  
  # Get number of host plants (total or with 40+ filtered records)
  ih2 <- ih %>%
    select(insect, host_accepted) %>%
    left_join(select(gbif, species, pa_csv), 
              join_by("host_accepted" == "species")) %>%
    group_by(insect) %>%
    summarize(n_hosts = length(host_accepted),
              n_hosts_suff = sum(pa_csv == "yes")) %>%
    data.frame()
  
  insects <- left_join(insects, ih2, join_by("species" == "insect"))
  
  # Create east/west indicator
  east <- "appalachiensis|brevicauda|canadensis|cresphontes|glaucus|palamedes|polyxenes|troilus"
  west <- "euymedon|indra|machaon|multicaudata|rumiko|rutulus|zelicaon"
  insects <- insects %>%
    mutate(ew = ifelse(grepl(east, species), "East", "West")) %>%
    mutate(species = str_replace(species, "Papilio", "P."))
  
  # Get range and mean/median number of records for each species
  summary(insects)
  
  # Write table to file:
  papilio_table <- paste0(output_basename, "papilio-table.csv")
  if (!file.exists(papilio_table) | (file.exists(papilio_table) & replace)) {
    write.csv(x = insects, 
              file = papilio_table, 
              row.names = FALSE)
  }

# Summaries: Climatically suitable areas --------------------------------------#

  # Attach number of host plants (with SDMs) to stats dataframe
  insects_simple <- insects %>%
    select(species, n_hosts_suff) %>%
    mutate(species = str_replace(species, "P. ", "Papilio ")) %>%
    rename(insect = species,
           n_hosts = n_hosts_suff)
  stats <- left_join(stats, insects_simple, by = "insect")
  
  # Predictions for P. appalachiensis
  stats %>%
    filter(insect == "Papilio appalachiensis", distribution == "total insect") %>%
    select(climate, area)
  # Predictions for P. palamedes
  stats %>%
    filter(insect == "Papilio palamedes", distribution == "total insect") %>%
    select(climate, area)
  stats %>%
    filter(insect == "Papilio palamedes", distribution == "insect + host") %>%
    select(climate, area)
  
  # There are many occasions when we will want summary stats that exclude
  # P. appalachiensis or P. appalachiensis and P. palamedes, so creating
  # additional stats dataframes to simplify.
  stats14 <- filter(stats, insect != "Papilio appalachiensis")
  stats13 <- filter(stats, !insect %in% c("Papilio appalachiensis", "Papilio palamedes"))

  # Summarize amount of land area that's predicted to be climatically suitable
  # (ignoring information about host plants). Columns with "all" are summaries 
  # across all 15 species; columns with "14" are summaries with all species except 
  # P. appalachiensis; columns with "13" are summaries with all species except
  # P. applachiensis and P. palamedes
  csuit_all <- stats %>%
    filter(distribution == "total insect") %>%
    group_by(climate) %>%
    summarize(area_min_all = min(area),
              area_mn_all = mean(area),
              area_md_all = median(area),
              area_max_all = max(area)) %>%
    data.frame()
  csuit_14 <- stats14 %>%
    filter(distribution == "total insect") %>%
    group_by(climate) %>%
    summarize(area_min_14 = min(area),
              area_mn_14 = mean(area),
              area_md_14 = median(area),
              area_max_14 = max(area)) %>%
    data.frame()
  csuit_13 <- stats13 %>%
    filter(distribution == "total insect") %>%
    group_by(climate) %>%
    summarize(area_min_13 = min(area),
              area_mn_13 = mean(area),
              area_md_13 = median(area),
              area_max_13 = max(area)) %>%
    data.frame()
  csuit <- left_join(csuit_all, csuit_14, by = "climate") %>%
    left_join(csuit_13, by = "climate")

  # Write table to file:
  suit_area <- paste0(output_basename, "suitable-areas.csv")
  if (!file.exists(suit_area) | (file.exists(suit_area) & replace)) {
    write.csv(x = csuit, 
              file = suit_area, 
              row.names = FALSE)
  }

# Summaries: Percent overlap --------------------------------------------------#

  # Summarize percent of area that's climatically suitable for insect that is 
  # also climatically suitable for one or more host plants (referred to as 
  # % overlap or swallowtail distribution). This info is captured in the 
  # "pinsect_withhost" column for "total insect" distributions. Note that we can't 
  # summarize % overlap for all 15 species because value for P. appalachiensis is 
  # NA (no area for swallowtail)
    
  overlap_14 <- stats14 %>%
    filter(distribution == "total insect") %>%
    group_by(climate) %>%
    summarize(overlap_min_14 = min(pinsect_withhost),
              overlap_mn_14 = mean(pinsect_withhost),
              overlap_md_14 = median(pinsect_withhost),
              overlap_max_14 = max(pinsect_withhost)) %>%
    data.frame()
  overlap_13 <- stats13 %>%
    filter(distribution == "total insect") %>%
    group_by(climate) %>%
    summarize(overlap_min_13 = min(pinsect_withhost),
              overlap_mn_13 = mean(pinsect_withhost),
              overlap_md_13 = median(pinsect_withhost),
              overlap_max_13 = max(pinsect_withhost)) %>%
    data.frame()
    
  # Use linear models to evaluate relationship between % overlap and total area
  # climatically suitable for swallowtail, as well as relationship between 
  # % overlap and number of host plants
  clims <- sort(unique(stats$climate))
  for (i in 1:length(clims)) {
    lmod <- lm(pinsect_withhost ~ scale(area), 
               data = filter(stats14, climate == clims[i]))
    overlap_14$beta_area[i] <- summary(lmod)$coefficients[2, "Estimate"]
    overlap_14$sd_area[i] <- summary(lmod)$coefficients[2, "Std. Error"]
    overlap_14$t_area[i] <- summary(lmod)$coefficients[2, "t value"]
    overlap_14$p_area[i] <- summary(lmod)$coefficients[2, "Pr(>|t|)"]
    
    lmod <- lm(pinsect_withhost ~ n_hosts, 
                data = filter(stats14, climate == clims[i]))
    overlap_14$beta_nhosts[i] <- summary(lmod)$coefficients[2, "Estimate"]
    overlap_14$sd_nhosts[i] <- summary(lmod)$coefficients[2, "Std. Error"]
    overlap_14$t_nhosts[i] <- summary(lmod)$coefficients[2, "t value"]
    overlap_14$p_nhosts[i] <- summary(lmod)$coefficients[2, "Pr(>|t|)"]
    
    lmod13 <- lm(pinsect_withhost ~ scale(area), 
                 data = filter(stats13, climate == clims[i]))
    overlap_13$beta_area[i] <- summary(lmod13)$coefficients[2, "Estimate"]
    overlap_13$sd_area[i] <- summary(lmod13)$coefficients[2, "Std. Error"]
    overlap_13$t_area[i] <- summary(lmod13)$coefficients[2, "t value"]
    overlap_13$p_area[i] <- summary(lmod13)$coefficients[2, "Pr(>|t|)"]
    
    lmoda <- lm(pinsect_withhost ~ n_hosts, 
                data = filter(stats13, climate == clims[i]))
    overlap_13$beta_nhosts[i] <- summary(lmoda)$coefficients[2, "Estimate"]
    overlap_13$sd_nhosts[i] <- summary(lmoda)$coefficients[2, "Std. Error"]
    overlap_13$t_nhosts[i] <- summary(lmoda)$coefficients[2, "t value"]
    overlap_13$p_nhosts[i] <- summary(lmoda)$coefficients[2, "Pr(>|t|)"]  
  }  

  # Write tables to file:
  o14 <- paste0(output_basename, "percent-overlap-14spp.csv")
  if (!file.exists(o14) | (file.exists(o14) & replace)) {
    write.csv(x = overlap_14, 
              file = o14, 
              row.names = FALSE)
  }
  o13 <- paste0(output_basename, "percent-overlap-13spp.csv")
  if (!file.exists(o13) | (file.exists(o13) & replace)) {
    write.csv(x = overlap_13, 
              file = o13, 
              row.names = FALSE)
  }
  
# Summaries: Changes in distributions over time -------------------------------#

# Focus on insect + host distributions for 13 species, since two species are not
# predicted to have suitable area or overlap with host plants
dist <- stats %>%
  filter(!insect %in% c("Papilio appalachiensis", "Papilio palamedes")) %>%
  filter(distribution == "insect + host") %>%
  select(-c(distribution, lat_max, lat_min, lon_max, lon_min, lat_max_bands, 
            lat_min_bands, lon_max_bands, lon_min_bands, lon_max_shift, 
            lon_min_shift, pinsect_withhost))

# Calculate % of current distribution that's in future distribution 
# (perc_current) and % change in area (perc_area_change)
dist <- dist %>%
  mutate(perc_current = area_retained / (area_retained + area_lost) * 100) %>%
  group_by(insect) %>%
  mutate(current_area = area[climate == "current"]) %>%
  ungroup() %>%
  data.frame() %>%
  mutate(perc_area_change = if_else(climate == "current", NA, 
                                    area / current_area * 100)) %>%
  select(-current_area)

# Summarize by time period, across species
dist_change <- dist %>%
  filter(climate != "current") %>%
  group_by(climate) %>%
  summarize(n_spp = length(insect),
            perc_current_min = min(perc_current),
            perc_current_mn = mean(perc_current),
            perc_current_md = median(perc_current),
            perc_current_max = max(perc_current),
            perc_change_min = min(perc_area_change),
            perc_change_mn = mean(perc_area_change),
            perc_change_md = median(perc_area_change),
            perc_change_max = max(perc_area_change),
            N_shift_min = min(lat_max_shift),
            N_shift_mn = mean(lat_max_shift),
            N_shift_md = median(lat_max_shift),
            N_shift_max = max(lat_max_shift),
            S_shift_min = min(lat_min_shift),
            S_shift_mn = mean(lat_min_shift),
            S_shift_md = median(lat_min_shift),
            S_shift_max = max(lat_min_shift)) %>%
  data.frame()

change <- paste0(output_basename, "distributional-changes.csv")
if (!file.exists(change) | (file.exists(change) & replace)) {
  write.csv(x = dist_change, 
            file = change, 
            row.names = FALSE)
}

# Figures: workflow (using P. rumiko as example) ------------------------------#

  workflow_spp <- "rumiko"
  ih_workflow <- ih %>% 
    filter(str_detect(insect, workflow_spp)) %>%
    left_join(select(gbif, c(species, pa_csv)), 
              by = c("host_accepted" = "species")) %>%
    filter(pa_csv == "yes")
  
  # P. rumiko suitability rasters for each SDM
  sdms <- c("brt", "gam", "lasso", "maxent", "rf")
  sdm_names <- paste0("output/suitabilities/papilio_", workflow_spp, "-",
                      sdms, "-current.rds")

  for (i in 1:length(sdms)) {
    suitc <- readRDS(sdm_names[i])
    if (i == 1) {
      xlim <- c(ext(suitc)[1], ext(suitc)[2])
      ylim <- c(ext(suitc)[3], ext(suitc)[4])
    }
    suitc <- mask(suitc, countries3)
    sdm_plot_base <- ggplot() +
      geom_spatvector(data = countries, color = NA, fill = "white") +
      geom_spatraster(data = suitc, maxcell = Inf) +
      scale_fill_gradientn(colors = terrain.colors(10), trans = "reverse",
                           na.value = "transparent", 
                           guide = guide_colorbar(reverse = TRUE)) +
      geom_spatvector(data = states, color = "gray65", fill = NA) +
      geom_spatvector(data = countries, color = "black", fill = NA) +
      coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim) +
      theme_bw()
    sdm_plot <- sdm_plot_base +
      theme(axis.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    sdm_suitc_out <- paste0(output_basename, workflow_spp, "-", 
                            sdms[i], "-suitability.png")
    if (!file.exists(sdm_suitc_out) | (file.exists(sdm_suitc_out) & replace)) {
      ggsave(filename = sdm_suitc_out,
             plot = sdm_plot,
             width = 5,
             height = 4,
             units = "in")
    }  
  }
  # Save one more of these maps, with a legend
  sdm_plot <- sdm_plot_base +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.5, "cm"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 8))
  sdm_suitcl_name <- paste0(output_basename, workflow_spp, 
                           "-sdm-suitability-legend.png")
  if (!file.exists(sdm_suitcl_name) | (file.exists(sdm_suitcl_name) & replace)) {
    ggsave(filename = sdm_suitcl_name,
           plot = sdm_plot,
           width = 5,
           height = 5,
           units = "in")
  }
  
  # Map with average suitabilities
  suitc <- readRDS(paste0("output/suitabilities/papilio_", 
                          workflow_spp, "-current.rds"))
  suitc <- mask(suitc, countries3)
  sdm_plot_base <- ggplot() +
    geom_spatvector(data = countries, color = NA, fill = "white") +
    geom_spatraster(data = suitc, maxcell = Inf) +
    scale_fill_gradientn(colors = terrain.colors(10), trans = "reverse",
                         na.value = "transparent", 
                         guide = guide_colorbar(reverse = TRUE)) +
    geom_spatvector(data = states, color = "gray65", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim) +
    theme_bw()
  sdm_plot <- sdm_plot_base +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          legend.key.width = unit(0.3, "cm"),
          legend.key.height = unit(1.5, "cm"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 8))
  suitcm_name <- paste0(output_basename, workflow_spp, "-suitability.png")
  if (!file.exists(suitcm_name) | (file.exists(suitcm_name) & replace)) {
    ggsave(filename = suitcm_name,
           plot = sdm_plot,
           width = 5.5,
           height = 4,
           units = "in")
  }
  
  # Area climatically suitable for P. rumiko
  suitb <- readRDS(paste0("output/distributions/papilio_", workflow_spp, 
                   "-distribution-current.rds"))
  suitb <- mask(suitb, countries3)
  labels_s <- c("Unsuitable", "Suitable") 
  suitb <- as.factor(as.numeric(suitb))
  levels(suitb) <- data.frame(value = c(0, 1), desc = labels_s)
  color_vec_s <- c("#f2f2f2", # Unsuitable
                   "#90c3de") # Suitable
  names(color_vec_s) <- levels(suitb)[[1]][,"desc"]
  
  suit_plot_base <- ggplot() +
    geom_spatvector(data = countries, color = NA, fill = "white") +
    geom_spatraster(data = suitb, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_s, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray65", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim) +
    theme_bw()
  suit_plot <- suit_plot_base +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          legend.key.width = unit(0.3, "cm"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 8))
  suitb_name <- paste0(output_basename, workflow_spp, "-suitable.png")
  if (!file.exists(suitb_name) | (file.exists(suitb_name) & replace)) {
    ggsave(filename = suitb_name,
           plot = suit_plot,
           width = 5.5,
           height = 4,
           units = "in")
  }
    
  # Areas climatically suitable for P. rumiko's host plants
  plants <- ih_workflow$host_accepted %>%
    tolower() %>%
    str_replace(pattern = " ", replacement = "_")
  suit_names <- paste0("output/distributions/", plants, 
                       "-distribution-current.rds")

  for (i in 1:length(plants)) {
    suitp <- readRDS(suit_names[i])
    labels_p <- c("Unsuitable", "Suitable") 
    suitp <- as.factor(as.numeric(suitp))
    levels(suitp) <- data.frame(value = c(0, 1), desc = labels_p)
    color_vec_p <- c("#f2f2f2", # Unsuitable 
                     "#89cf72") # Suitable
    names(color_vec_p) <- levels(suitp)[[1]][,"desc"]
    suit_plot_base <- ggplot() +
      geom_spatvector(data = countries, color = NA, fill = "white") +
      geom_spatraster(data = suitp, maxcell = Inf) +
      scale_fill_manual(name = "desc", values = color_vec_p, na.translate = FALSE) +
      geom_spatvector(data = states, color = "gray65", fill = NA) +
      geom_spatvector(data = countries, color = "black", fill = NA) +
      coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim) +
      theme_bw()
    suit_plot <- suit_plot_base +
      theme(axis.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    suitp_name <- paste0(output_basename, plants[i], "-suitable.png")
    if (!file.exists(suitp_name) | (file.exists(suitp_name) & replace)) {
      ggsave(filename = suitp_name,
             plot = suit_plot,
             width = 3.9,
             height = 2.7,
             units = "in")
    }
  }
  
  # Overlap
  overlap <- readRDS(paste0("output/overlaps/papilio_", workflow_spp, 
                            "-overlap-current.rds"))
  overlap <- mask(overlap, countries3)
  labels_o <- c("Absent", 
                "Hosts only",
                paste0("P. ", workflow_spp, " only"),
                paste0("P. ", workflow_spp, " and hosts"))     
  overlap2 <- overlap
  overlap2[overlap2 %in% 1:2] <- 1
  overlap2[overlap2 == 3] <- 2
  overlap2[overlap2 %in% 4:5] <- 3
  overlap2 <- as.factor(overlap2)
  levels(overlap2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  
  color_vec_o <- c("#e8e8e8",   # Absent
                   "#89cf72",   # Hosts only
                   "#90c3de",   # Insect only
                   "#8f5cd6")   # Hosts and insect
  names(color_vec_o) <- levels(overlap2)[[1]][,"desc"]
  
  overlap_plot_base <- ggplot() +
    geom_spatvector(data = countries, color = NA, fill = "white") +
    geom_spatraster(data = overlap2, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray65", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim) +
    theme_bw()
  overlap_plot <- overlap_plot_base +
    theme(axis.title = element_blank(),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          legend.key.width = unit(0.3, "cm"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 8))
  overlap_name <- paste0(output_basename, workflow_spp, "-overlap.png")
  if (!file.exists(overlap_name) | (file.exists(overlap_name) & replace)) {
    ggsave(filename = overlap_name,
           plot = overlap_plot,
           width = 6,
           height = 4,
           units = "in")
  }

# Figures: 2-swallowtail comparison (overlaps, deltas) ------------------------#
  
  # For now, picking P. indra (western species; some gain [incl GB], some loss; 
  # not limited by host plants) and P. cresphontes (eastern species; some gain
  # [incl midwest], some loss; limited by host plants in FL)

  # Species
  spp <- c("indra", "cresphontes")
  nice_names <- paste0("papilio_", spp)
  abbr_names <- paste0("P. ", spp)
  full_names <- paste0("Papilio ", spp)

  # Future scenario
  scen <- "ssp370_2041"
  
  # Load overlap maps
  overlapc1 <- readRDS(paste0("output/overlaps/", nice_names[1], 
                              "-overlap-current.rds"))
  overlapc2 <- readRDS(paste0("output/overlaps/", nice_names[2], 
                              "-overlap-current.rds"))
  overlapf1 <- readRDS(paste0("output/overlaps/", nice_names[1], 
                              "-overlap-ensemble_", scen, ".rds"))
  overlapf2 <- readRDS(paste0("output/overlaps/", nice_names[2], 
                              "-overlap-ensemble_", scen, ".rds"))
  
  labels_o <- c("Absent", "Hosts only", "Swallowtail only", "Swallowtail and hosts")  
  
  # Create new overlap rasters with specified levels
  overlapc1[overlapc1 %in% 1:2] <- 1
  overlapc1[overlapc1 == 3] <- 2
  overlapc1[overlapc1 %in% 4:5] <- 3
  overlapc1 <- as.factor(overlapc1)
  levels(overlapc1) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  overlapc2[overlapc2 %in% 1:2] <- 1
  overlapc2[overlapc2 == 3] <- 2
  overlapc2[overlapc2 %in% 4:5] <- 3
  overlapc2 <- as.factor(overlapc2)
  levels(overlapc2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  overlapf1[overlapf1 %in% 1:2] <- 1
  overlapf1[overlapf1 == 3] <- 2
  overlapf1[overlapf1 %in% 4:5] <- 3
  overlapf1 <- as.factor(overlapf1)
  levels(overlapf1) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  overlapf2[overlapf2 %in% 1:2] <- 1
  overlapf2[overlapf2 == 3] <- 2
  overlapf2[overlapf2 %in% 4:5] <- 3
  overlapf2 <- as.factor(overlapf2)
  levels(overlapf2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  
  color_vec_o <- c("#e8e8e8",   # Absent
                   "#89cf72",   # Hosts only
                   "#90c3de",   # Insect only
                   "#8f5cd6")   # Hosts and insect
  names(color_vec_o) <- levels(overlapc1)[[1]][,"desc"]
  
  # Get rid of NA values and calculate extent for plot areas
  overlapc1 <- drop_na(overlapc1)
  overlapc2 <- drop_na(overlapc2)
  overlapf1 <- drop_na(overlapf1)
  overlapf2 <- drop_na(overlapf2)
  xlim1 <- c(ext(overlapf1)[1], ext(overlapf1)[2])
  ylim1 <- c(ext(overlapf1)[3], ext(overlapf1)[4])
  xlim2 <- c(ext(overlapf2)[1], ext(overlapf2)[2])
  ylim2 <- c(ext(overlapf2)[3], ext(overlapf2)[4])
  
  margins <- c(4, 0, 6, 0)

  # Create overlap map objects
  overlapc1_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlapc1, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    scale_x_continuous(breaks = c(-120, -110, -100)) +
    theme_bw() +
    ylab("Current") +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  overlapc2_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlapc2, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim2, ylim = ylim2) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"),
          axis.text = element_text(size = 10))
  overlapf1_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlapf1, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    scale_x_continuous(breaks = c(-120, -110, -100)) +
    theme_bw() +
    ylab("Future") +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  overlapf2_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlapf2, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim2, ylim = ylim2) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"))
  
  # Load delta maps
  delta1 <- readRDS(paste0("output/deltas/", nice_names[1], "-delta-insecthost-",
                           scen, ".rds"))
  delta2 <- readRDS(paste0("output/deltas/", nice_names[2], "-delta-insecthost-",
                           scen, ".rds"))

  labels_d <- c("Absent", "Loss", "Gain", "Stable")  

  delta1 <- as.factor(delta1)
  levels(delta1) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
  delta2 <- as.factor(delta2)
  levels(delta2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
  
  color_vec_d <- c("#e8e8e8",   # Absent
                   "#f0b041",   # Loss
                   "#2f6cc2",   # Gain
                   "#f6e8c3")   # Stable
  names(color_vec_d) <- levels(delta1)[[1]][,"desc"]
  
  # Get rid of NA values and calculate extent for plot areas
  delta1 <- drop_na(delta1)
  delta2 <- drop_na(delta2)

  # Create delta map objects
  delta1_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = delta1, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    scale_x_continuous(breaks = c(-120, -110, -100)) +
    theme_bw() +
    ylab("Future - Current") +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  delta2_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = delta2, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim2, ylim = ylim2) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"))
    
  # Create legends (Had to change things up, since cowplot::get_legend didn't 
  # seem to work anymore)
    # Function to grab legend from ggplot object
    get_leg <- function(myggplot) {
      tmp <- ggplot_gtable(ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
    }
    overlap_for_legend <- ggplot() +
      geom_spatraster(data = overlapc1, maxcell = Inf) +
      scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    legend_o <- get_leg(overlap_for_legend)
    delta_for_legend <- ggplot() +
      geom_spatraster(data = delta1, maxcell = Inf) +
      scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
      theme(legend.position = "bottom",
            legend.title = element_blank())
    legend_d <- get_leg(delta_for_legend)

  # Combine things
  overlaps <- plot_grid(overlapc1_plot + theme(legend.position = "none"),
                        overlapc2_plot + theme(legend.position = "none"),
                        overlapf1_plot + theme(legend.position = "none"),
                        overlapf2_plot + theme(legend.position = "none"),
                        align = "vh",
                        nrow = 2,
                        byrow = TRUE)
  deltas <- plot_grid(delta1_plot + theme(legend.position = "none"),
                      delta2_plot + theme(legend.position = "none"),
                      align = 'vh',
                      nrow = 1)
  col_titles <- ggdraw() + 
    draw_label(full_names[1], fontface = "italic", size = 12, x = 0.29) +
    draw_label(full_names[2], fontface = "italic", size = 12, x = 0.79)
  
  p <- plot_grid(col_titles, overlaps, legend_o, NULL, deltas, legend_d,
                 ncol = 1, rel_heights = c(0.2, 5.6, 0.2, 0.2, 2.8, 0.2))

  twospecies_name <- paste0(output_basename, "twospecies_6panel.png")
  if (!file.exists(twospecies_name) | (file.exists(twospecies_name) & replace)) {
    ggsave(filename = twospecies_name,
           plot = p,
           width = 6.5,
           height = 9,
           units = "in")
  }

# Figures: richness maps ------------------------------------------------------#
  
  # Future scenario
  scen <- "ssp370_2041"
  
  # Type of map 
  # ov = insect distributed only where one or more host also occurs
  # io = insect distribution depends only on climate)
  type <- "ov"
  
  # Load richness rasters
  rich_c_name <- paste0("output/richness/current-richness-", type, ".rds")
  rich_c <- readRDS(rich_c_name)
  rich_f_name <- paste0("output/richness/ensemble_", scen, "-richness-",
                        type, ".rds")
  rich_f <- readRDS(rich_f_name)
  
  # Load delta raster
  delta_name <- paste0("output/richness/ensemble_", scen, "-delta-richness-",
                       type, ".rds")
  delta <- readRDS(delta_name)
  
  # Plot extent
  xlimr <- c(ext(delta)[1], ext(delta)[2])
  ylimr <- c(ext(delta)[3], ext(delta)[4])
  
  margins <- c(2, 0, 6, 0)
  
  # Create map objects
  richness_c <- ggplot() +
    geom_spatvector(data = countries, color = NA, fill = "white") +
    geom_spatraster(data = rich_c) +
    scale_fill_whitebox_c(palette = "gn_yl",
                          direction = -1,
                          breaks = c(6, 4, 2, 0),
                          name = "Richness") +
    geom_spatvector(data = states, color = "gray65", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr, ylim = ylimr) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"),
          legend.spacing.y = unit(10, 'pt'))
  richness_f <- ggplot() +
    geom_spatvector(data = countries, color = NA, fill = "white") +
    geom_spatraster(data = rich_f, maxcell = Inf) +
    scale_fill_whitebox_c(palette = "gn_yl",
                          direction = -1,
                          breaks = c(6, 4, 2, 0),
                          name = "Richness") +
    geom_spatvector(data = states, color = "gray65", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr, ylim = ylimr) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"),
          legend.spacing.y = unit(10, 'pt'))
  richness_d <- ggplot() +
    geom_spatvector(data = countries, color = NA, fill = "white") +
    geom_spatraster(data = delta) +
    scale_fill_whitebox_c(palette = "purple",
                          direction = -1,
                          breaks = c(5, 0 , -5),
                          name = "Change") +
    geom_spatvector(data = states, color = "gray65", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr, ylim = ylimr) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"),
          legend.spacing.y = unit(10, 'pt'))
  
  # Combine everything
  r <- plot_grid(richness_c, richness_f, richness_d,
                 align = "h",
                 ncol = 1,
                 labels = "auto",
                 vjust = 1,
                 hjust = 0)
  richness_3panel <- paste0(output_basename, "richness_3panel.png")
  if (!file.exists(richness_3panel) | (file.exists(richness_3panel) & replace)) {
    ggsave(filename = richness_3panel,
           plot = r,
           width = 4.5,
           height = 8,
           units = "in")
  }
    
  # Save figures, except with a different projection
  # Crop the eastern edge of states and countries layers
  state_ext <- ext(states)
  state_ext[2] <- -45
  states <- crop(states, state_ext)
  countries <- crop(countries, state_ext)
  
  # Project layers to Lambert Conformal Conic North America
  states_lcc <- project(states, "ESRI:102009")
  countries_lcc <- project(countries, crs(states_lcc))
  rich_c_lcc <- project(rich_c, crs(states_lcc))
  rich_f_lcc <- project(rich_f, crs(states_lcc))
  rich_d_lcc <- project(delta, crs(states_lcc))
  
  # Get rid of NA values and calculate extent for plot area
  rich_c_lcc <- drop_na(rich_c_lcc)
  limsr_lcc <- ext(rich_c_lcc) * 1.01
  xlimr_lcc <- c(ext(limsr_lcc)[1], ext(limsr_lcc)[2])
  ylimr_lcc <- c(ext(limsr_lcc)[3], ext(limsr_lcc)[4])
  
  # Note that when plotting, we're removing boundaries for all countries 
  # except the US, Canada, and Mexico because with this projection that is 
  # centered in the US, country boundaries become very distorted for Greenland 
  # and Central America
  richness_c_lcc <- ggplot() +
    geom_spatvector(data = states_lcc, color = NA, fill = "white") +
    geom_spatraster(data = rich_c_lcc) +
    scale_fill_whitebox_c(palette = "gn_yl",
                          direction = -1,
                          breaks = c(6, 4, 2, 0),
                          name = "Richness") +
    geom_spatvector(data = states_lcc, color = "gray50", fill = NA) +
    geom_spatvector(data = filter(countries_lcc, 
                                  countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                    color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
             expand = FALSE) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"),
          legend.spacing.y = unit(10, 'pt'))
  richness_f_lcc <- ggplot() +
    geom_spatvector(data = states_lcc, color = NA, fill = "white") +
    geom_spatraster(data = rich_f_lcc) +
    scale_fill_whitebox_c(palette = "gn_yl",
                          direction = -1,
                          breaks = c(6, 4, 2, 0),
                          name = "Richness") +
    geom_spatvector(data = states_lcc, color = "gray50", fill = NA) +
    geom_spatvector(data = filter(countries_lcc, 
                                  countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                    color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
             expand = FALSE) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"),
          legend.spacing.y = unit(10, 'pt'))
  richness_d_lcc <- ggplot() +
    geom_spatvector(data = countries_lcc, color = NA, fill = "white") +
    geom_spatraster(data = rich_d_lcc) +
    scale_fill_whitebox_c(palette = "purple",
                          direction = -1,
                          breaks = c(5, 0 , -5),
                          name = "Change") +
    geom_spatvector(data = states_lcc, color = "gray65", fill = NA) +
    geom_spatvector(data = filter(countries_lcc, 
                                  countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                    color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
             expand = FALSE) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"),
          legend.spacing.y = unit(10, 'pt'))
  
  # Combine everything
  r_lcc <- plot_grid(richness_c_lcc, richness_f_lcc, richness_d_lcc,
                     align = "vh",
                     ncol = 1,
                     labels = "auto",
                     vjust = 1,
                     hjust = 0)
  richness_lcc_3panel <- paste0(output_basename, "richness_lcc_3panel.png")
  if (!file.exists(richness_lcc_3panel) | (file.exists(richness_lcc_3panel) & replace)) {
    ggsave(filename = richness_lcc_3panel,
           plot = r_lcc,
           width = 4,
           height = 8,
           units = "in")
  }

  
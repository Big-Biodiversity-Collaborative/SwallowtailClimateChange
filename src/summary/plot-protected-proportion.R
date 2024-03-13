# Plotting protected area change over time
# Jeff Oliver; Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2024-03-13

# EXPLORATORY

library(dplyr)
library(ggplot2)
library(tidyr)

# Exploring amount of protected area for individual species -------------------#
  pa_results <- read.csv("output/summary-stats/protected-areas-allspp.csv")
  
  # Separate SSP and year data into two separate columns
  pa_results <- pa_results %>%
    filter(distribution == "insect + host") %>%
    mutate(ssp = if_else(climate == "current", 
                         true = "current",
                         false = substr(x = climate, start = 4, stop = 6))) %>%
    mutate(year = if_else(climate == "current",
                          true = 2020,
                          false = as.numeric(substr(x = climate, 
                                                    start = 8, 
                                                    stop = 11)))) %>%
    mutate(year = if_else(year == 2041, 
                          true = 2055, if_else(year == 2071,
                                               true = 2085, 2020))) %>%
    select(-c(climate, distribution))
  
  # Now we want to duplicate the climate data so we effectively have a row of 
  # current climate for each ssp (as starting points)
  curr_df <- tidyr::complete(data.frame(insect = unique(pa_results$insect),
                                        ssp = c("245", "370", "585")),
                             insect, ssp)
  curr_prop <- pa_results %>%
    filter(ssp == "current")
  
  # Add in the proportion of area protected
  new_df <- curr_prop %>%
    select(-ssp) %>%
    left_join(curr_df, by = c("insect" = "insect"))
  
  # And bind those rows back in
  pa_all <- pa_results %>%
    bind_rows(new_df) %>%
    filter(ssp != "current") %>%
    arrange(insect, ssp, year)
  
  # Create east/west indicator
  east <- "appalachiensis|brevicauda|canadensis|cresphontes|glaucus|palamedes|polyxenes|troilus"
  west <- "euymedon|indra|machaon|multicaudata|rumiko|rutulus|zelicaon"
  pa_all <- pa_all %>%
    mutate(ew = if_else(grepl(east, insect), "East", "West"))
  
  # Separate P. machaon (in case we want that option for visualization purposes)
  pa_all <- pa_all %>%
    mutate(group3 = if_else(insect == "Papilio machaon", "P. machaon", ew))
  
  # Species-level plots
  ggplot(data = pa_all, aes(x = year, y = proportion_protected, group = ssp,
                            color = ssp)) +
    geom_line() +
    geom_point() +
    facet_wrap(~insect, scales = "free") +
    labs(x = "Year", y = "Proportion protected", color = "SSP")
  
  # Not a lot of difference between SSPs or over time (note y-axes). Most 
  # significant thing to note is the difference between species in the east/west 
  # (though see that 2 eastern species have no areas deemed suitable for them 
  # and host plants in the future [and one has > 20% of current distribution in 
  # protected areas])
  
  # Calculate means (weighted by area or not) for east/west after removing species 
  # that aren't predicted to persist over time
  area_wgts <- pa_all %>%
    filter(!insect %in% c("Papilio appalachiensis", "Papilio palamedes")) %>%
    group_by(ew, year, ssp) %>%
    summarize(area_total = sum(area_sqkm), .groups = "keep")
  
  pa_means <- pa_all %>%
    filter(!insect %in% c("Papilio appalachiensis", "Papilio palamedes")) %>%
    left_join(area_wgts, by = c("ew", "year", "ssp")) %>%
    mutate(area_wgt = area_sqkm / area_total) %>%
    group_by(ew, year, ssp) %>%
    summarize(prop_protected = mean(proportion_protected),
              prop_protected_wgtd = sum(proportion_protected * area_wgt), 
              .groups = "keep") %>%
    data.frame()
  
  # Plot proportions protected for eastern and western spp over time, with 
  # average in each group (weighted by area)
  ggplot(data = pa_all,
         aes(x = year, y = proportion_protected, group = insect, color = ew)) +
    geom_line(linewidth = 0.5, alpha = 0.3) +
    geom_point(size = 0.5, alpha = 0.3) +
    geom_line(data = pa_means, 
              aes(x = year, y = prop_protected_wgtd, group = ew, 
                  color = ew), linewidth = 1.5, show.legend = FALSE) +
    geom_point(data = pa_means, 
               aes(x = year, y = prop_protected_wgtd, group = ew, 
                   color = ew), size = 1.5, show.legend = FALSE) +
    labs(x = "Year", y = "Proportion protected") +
    facet_grid(cols = vars(ssp)) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.95, 0.93))
  
# Exploring amount of protected area for swallowtail diversity hotspots -------#
  
  # TODO: Run (and make edits to) code below after calculating protected areas
  # for swallowtail diveristy hotspots
  
  pah_results <- read.csv("output/summary-stats/protected-areas-hotspots.csv")
  
  # Separate SSP and year data into two separate columns
  pah_results <- pa_results %>%
    filter(distribution == "insect + host") %>%
    mutate(ssp = if_else(climate == "current", 
                         true = "current",
                         false = substr(x = climate, start = 4, stop = 6))) %>%
    mutate(year = if_else(climate == "current",
                          true = 2020,
                          false = as.numeric(substr(x = climate, 
                                                    start = 8, 
                                                    stop = 11)))) %>%
    mutate(year = if_else(year == 2041, 
                          true = 2055, if_else(year == 2071,
                                               true = 2085, 2020))) %>%
    select(-c(climate, distribution))
  
  # Now we want to duplicate the climate data so we effectively have a row of 
  # current climate for each ssp (as starting points)
  curr_df <- tidyr::complete(data.frame(min_num_spp = unique(pah_results$min_num_spp),
                                        ssp = c("245", "370", "585")),
                             min_num_spp, ssp)
  curr_prop <- pah_results %>%
    filter(ssp == "current")
  
  # Add in the proportion of area protected
  new_df <- curr_prop %>%
    select(-ssp) %>%
    left_join(curr_df, by = c("min_num_spp" = "min_num_spp"))
  
  # And bind those rows back in
  pah_all <- pah_results %>%
    bind_rows(new_df) %>%
    filter(ssp != "current") %>%
    arrange(min_num_spp, ssp, year)
  
  # Select minimum number of species to define hotspot
  min_spp <- 4
  pah_all <- filter(pah_all, min_num_spp == min_spp)
  
  # Species-level plots
  ggplot(data = pah_all, aes(x = year, y = proportion_protected, group = ssp,
                            color = ssp)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Proportion protected", color = "SSP") +
    theme(legend.position.inside = c(0.95, 0.95))

# View predicted suitability and range maps for each species
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-09-20

require(stringr)
require(raster)
require(terra)
require(dplyr)
require(dismo)
library(ggplot2)
library(cowplot)

# Load up the functions from the functions folder
source(file = "load_functions.R")

# Identify which species to include (just insects for now) 
spp_data <- read.csv("data/gbif-pa-summary.csv", header = TRUE)
insect_data <- spp_data %>%
  filter(str_detect(species, "Papilio") & pa_csv == "yes") %>%
  select(species, n_filtered)
nice_names <- insect_data %>%
  select(species) %>%
  unlist() %>%
  str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# Load evaluation metrics from CV models
evals <- read.csv("development/output/evals-CV-insect.csv", header = TRUE)

# Pick a species
for (i in 1:nrow(insect_data)) {
  nice_name <- nice_names[i]
  insect <- insect_data$species[i]
  short_name <- paste0("P. ", str_split(insect, " ")[[1]][2])
  
  # Summarize evaluation metrics for each SDM across folds
  evals_ins <- evals %>%
    filter(insect == insect_data$species[i]) %>%
    group_by(sdm) %>%
    summarize(auc.avg = mean(AUC),
              auc.min = min(AUC),
              cbi.avg = mean(CBI),
              cbi.min = min(CBI),
              imae.avg = mean(IMAE),
              imae.min = min(IMAE),
              thr.avg = mean(thr.mss),
              or.avg = mean(OR.mss),
              or.max = max(OR.mss),
              tss.avg = mean(TSS.mss),
              tss.min = min(TSS.mss),
              .groups = "keep") %>%
    mutate(across(auc.avg:tss.min, function(x) round(x, 3))) %>%
    data.frame()  
  
  # Define "bad" models, which we may want to exclude from ensembles
  # For now, label those with mean CBI < 0 and/or mean AUC < 0.5 as "bad"
  sdms <- c("BRT", "GAM", "LASSO", "MAXENT", "RF")
  bad.models <- evals_ins$sdm[evals_ins$cbi.avg < 0 | evals_ins$auc.avg < 0.5]
  good.models <- setdiff(sdms, bad.models)
  
  # Create weights for calculating mean predictions across SDMs based on TSS
  # (for all models or "good" models)  
  evals_ins <- evals_ins %>%
    mutate(wt.tss = tss.avg / sum(tss.avg),
           good = ifelse(sdm %in% good.models, 1, NA),
           tss.good = tss.avg * good,
           wt.good.tss = tss.good / sum(tss.good, na.rm = TRUE))  
  
  # Calculate thresholds based on CV models 
    # Load predicted suitability values for pres/bg points from each SDM
    preds_file <- paste0("development/output/predicted-probabilities/",
                         nice_name, "-pred-probs-presbg.csv")
    preds_all <- read.csv(preds_file, header = TRUE)
    
    # Calculate mean (or weighted mean) of predicted suitability values for those
    # points across SDMs
    mn_all <- apply(preds_all[,sdms], 1, mean) 
    wtmn_all <- as.matrix(preds_all[,sdms]) %*% as.vector(evals_ins$wt.tss)
    mn_good <-  apply(preds_all[,good.models], 1, mean)  
    wtmn_good <- as.matrix(preds_all[,good.models]) %*% 
      as.vector(evals_ins$wt.good.tss[!is.na(evals_ins$wt.good.tss)])
    preds_all <- cbind(preds_all, mn_all, wtmn_all, mn_good, wtmn_good)
    # head(preds_all)
    # cor(preds_all[,5:13])
    
    # Calculate max(spec + sens) thresholds for each SDM and mean
    predtype <- colnames(preds_all)[5:13]
    thresholds <- data.frame(predtype = predtype, thr = NA)
    for (type in predtype) {
      p <- preds_all %>% filter(pa == 1) %>% select(any_of(type)) %>% pull()
      a <- preds_all %>% filter(pa == 0) %>% select(any_of(type)) %>% pull()
      eval <- dismo::evaluate(p = p, a = a)
      thr <- dismo::threshold(eval, stat = "spec_sens")
      thresholds$thr[predtype == type] <- thr
    }
  
  # Load rasters with predicted probabilities
    file_start <- paste0("development/output/predicted-probabilities/",
                         nice_name, "-pred-probs-")
    brt_current <- readRDS(paste0(file_start, "brt-current.rds"))
    gam_current <- readRDS(paste0(file_start, "gam-current.rds"))
    lasso_current <- readRDS(paste0(file_start, "lasso-current.rds"))
    maxent_current <- readRDS(paste0(file_start, "max-current.rds"))
    rf_current <- readRDS(paste0(file_start, "rf-current.rds"))
    brt_future <- readRDS(paste0(file_start, "brt-ensemble_ssp245_2041.rds"))
    gam_future <- readRDS(paste0(file_start, "gam-ensemble_ssp245_2041.rds"))
    lasso_future <- readRDS(paste0(file_start, "lasso-ensemble_ssp245_2041.rds"))
    maxent_future <- readRDS(paste0(file_start, "max-ensemble_ssp245_2041.rds"))
    rf_future <- readRDS(paste0(file_start, "rf-ensemble_ssp245_2041.rds"))
  
  # Create rasters with unweighted and weighted mean values
    mn_all_current <- app(rast(mget(paste0(tolower(sdms), "_current"))), mean)
    wtmn_all_current <- app(rast(mget(paste0(tolower(sdms), "_current"))),
                            function(x) sum(x * evals_ins$wt.tss))
    mn_all_future <- app(rast(mget(paste0(tolower(sdms), "_future"))), mean)
    wtmn_all_future <- app(rast(mget(paste0(tolower(sdms), "_future"))),
                            function(x) sum(x * evals_ins$wt.tss))
    
  # Create rasters with unweighted and weighted mean values, across GOOD models
    if (length(bad.models) > 0) {
      mn_good_current <- app(rast(mget(paste0(tolower(good.models), "_current"))), mean)
      wtmn_good_current <- app(rast(mget(paste0(tolower(good.models), "_current"))),
                               function(x) sum(x * evals_ins$wt.good.tss[!is.na(evals_ins$good)]))
      mn_good_future <- app(rast(mget(paste0(tolower(good.models), "_future"))), mean)
      wtmn_good_future <- app(rast(mget(paste0(tolower(good.models), "_future"))),
                               function(x) sum(x * evals_ins$wt.good.tss[!is.na(evals_ins$good)]))
    }
  
  # Extract predicted suitabilities for pres/bg points from rasters (from "full"
  # model) and use these to calculate thresholds
    preds_full <- preds_all[, 1:4]
    locs <- preds_full[, c("x", "y")]
    preds_full <- preds_full %>%
      mutate(BRT = extract(brt_current, locs, ID = FALSE)[, 1],
             GAM = extract(gam_current, locs, ID = FALSE)[, 1],
             LASSO = extract(lasso_current, locs, ID = FALSE)[, 1],
             MAXENT = extract(maxent_current, locs, ID = FALSE)[, 1],
             RF = extract(rf_current, locs, ID = FALSE)[, 1],
             mn_all = extract(mn_all_current, locs, ID = FALSE)[, 1],
             wtmn_all = extract(wtmn_all_current, locs, ID = FALSE)[, 1])
    if (length(bad.models) > 0) {
    preds_full <- preds_full %>%
      mutate(mn_good = extract(mn_good_current, locs, ID = FALSE)[, 1],
             wtmn_good = extract(wtmn_good_current, locs, ID = FALSE)[, 1])
    }
    predtype <- colnames(preds_full)[5:ncol(preds_full)]
    thresholds_full <- data.frame(predtype = predtype, thr = NA)
    for (type in predtype) {
      p <- preds_full %>% filter(pa == 1) %>% select(any_of(type)) %>% pull()
      a <- preds_full %>% filter(pa == 0) %>% select(any_of(type)) %>% pull()
      eval <- dismo::evaluate(p = p, a = a)
      thr <- dismo::threshold(eval, stat = "spec_sens")
      thresholds_full$thr[predtype == type] <- thr
    }
  
  # Visualize predicted suitabilities, Current ###################################
    brt_c_plot <- ggplot(data = as.data.frame(brt_current, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("BRT")
    gam_c_plot <- ggplot(data = as.data.frame(gam_current, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("GAM")
    lasso_c_plot <- ggplot(data = as.data.frame(lasso_current, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = last)) +
      ggtitle("LASSO")
    max_c_plot <- ggplot(data = as.data.frame(maxent_current, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("MAX")
    rf_c_plot <- ggplot(data = as.data.frame(rf_current, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("RF")
    wtmn_all_c_plot <- ggplot(data = as.data.frame(wtmn_all_current, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr.1)) +
      ggtitle("WT MEAN")
    
    suit_theme <- list(
      scale_fill_gradientn(colors = rev(terrain.colors(256))),
      coord_quickmap(),
      theme_classic(),
      theme(text = element_text(size = 8),
            legend.title = element_blank(),
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.15, "in"),
            legend.margin = margin(0, 0, 0, 0, "in"),
            axis.title = element_blank(), 
            axis.line = element_blank(),
            plot.title = element_text(hjust = 0.5, vjust = 0, size = 8),
            panel.border = element_rect(colour = "black", fill = NA),
            plot.margin = unit(c(0.05, 0.05, 0, 0.05), "in"))
    )
    
    if (length(bad.models) == 0) {
      suit_cur <- plot_grid(brt_c_plot + suit_theme,
                            gam_c_plot + suit_theme,
                            lasso_c_plot + suit_theme,
                            max_c_plot + suit_theme,
                            rf_c_plot + suit_theme,
                            wtmn_all_c_plot + suit_theme,
                            nrow = 2, ncol = 3)
    } else {
      wtmn_good_c_plot <- ggplot(data = as.data.frame(wtmn_good_current, xy = TRUE)) +
        geom_tile(aes(x = x, y = y, fill = lyr.1)) +
        ggtitle("WT MEAN, GOOD")
      suit_cur <- plot_grid(brt_c_plot + suit_theme,
                            gam_c_plot + suit_theme,
                            lasso_c_plot + suit_theme,
                            max_c_plot + suit_theme,
                            rf_c_plot + suit_theme,
                            wtmn_all_c_plot + suit_theme,
                            wtmn_good_c_plot + suit_theme,
                            nrow = 3, ncol = 3)
    }
    
    title <- ggdraw() + draw_label(paste0(short_name, ", Current, 9 vars"), size = 9)
    suit_c <- plot_grid(title, suit_cur, ncol=1, rel_heights = c(0.1, 3.5))
    ggsave(plot = suit_c,
           file = paste0("development/output/maps/", 
                         nice_name, "-cp-9var.jpg"),
           device = "jpeg",
           width = 6.5,
           height = 4.5,
           units = "in")
    
  # Visualize predicted ranges, Current, CV thresholds ###########################
    brt_range <- brt_current > thresholds$thr[thresholds$predtype == "BRT"]
    gam_range <- gam_current > thresholds$thr[thresholds$predtype == "GAM"]
    lasso_range <- lasso_current > thresholds$thr[thresholds$predtype == "LASSO"]
    maxent_range <- maxent_current > thresholds$thr[thresholds$predtype == "MAXENT"]
    rf_range <- rf_current > thresholds$thr[thresholds$predtype == "RF"]
    wtmn_all_range <- wtmn_all_current > thresholds$thr[thresholds$predtype == "wtmn_all"]
    
    brt_cr_plot <- ggplot(data = as.data.frame(brt_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("BRT")
    gam_cr_plot <- ggplot(data = as.data.frame(gam_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("GAM")
    lasso_cr_plot <- ggplot(data = as.data.frame(lasso_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = last)) +
      ggtitle("LASSO")
    max_cr_plot <- ggplot(data = as.data.frame(maxent_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("MAX")
    rf_cr_plot <- ggplot(data = as.data.frame(rf_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("RF")
    wtmn_all_cr_plot <- ggplot(data = as.data.frame(wtmn_all_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr.1)) +
      ggtitle("WT MEAN")
    
    range_theme <- list(
      scale_fill_manual(values = c("gray90", "forestgreen")),
      coord_quickmap(),
      theme_classic(),
      theme(text = element_text(size = 8),
            legend.title = element_blank(),
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.15, "in"),
            legend.margin = margin(0, 0, 0, 0, "in"),
            axis.title = element_blank(), 
            axis.line = element_blank(),
            plot.title = element_text(hjust = 0.5, vjust = 0, size = 8),
            panel.border = element_rect(colour = "black", fill = NA),
            plot.margin = unit(c(0.05, 0.05, 0, 0.05), "in"))
    )
    
    if (length(bad.models) == 0) {
      range_cur <- plot_grid(brt_cr_plot + range_theme,
                             gam_cr_plot + range_theme,
                             lasso_cr_plot + range_theme,
                             max_cr_plot + range_theme,
                             rf_cr_plot + range_theme,
                             wtmn_all_cr_plot + range_theme,
                             nrow = 2, ncol = 3)
    } else {
      wtmn_good_range <- wtmn_good_current > thresholds$thr[thresholds$predtype == "wtmn_good"]
      wtmn_good_cr_plot <- ggplot(data = as.data.frame(wtmn_good_range, xy = TRUE)) +
        geom_tile(aes(x = x, y = y, fill = lyr.1)) +
        ggtitle("WT MEAN, GOOD")
      range_cur <- plot_grid(brt_cr_plot + range_theme,
                             gam_cr_plot + range_theme,
                             lasso_cr_plot + range_theme,
                             max_cr_plot + range_theme,
                             rf_cr_plot + range_theme,
                             wtmn_all_cr_plot + range_theme,
                             wtmn_good_cr_plot + range_theme,
                             nrow = 3, ncol = 3)
    }
    
    title <- ggdraw() + draw_label(paste0(short_name, ", Current, CVthr, 9 vars"), size = 9)
    range_c <- plot_grid(title, range_cur, ncol=1, rel_heights = c(0.1, 3.5))
    ggsave(plot = range_c,
           file = paste0("development/output/maps/", 
                         nice_name, "-cr-9var-CVthr.jpg"),
           device = "jpeg",
           width = 6.5,
           height = 4.5,
           units = "in")
    
  # Visualize predicted ranges, Current, Full thresholds #########################  
    brt_range <- brt_current > thresholds_full$thr[thresholds_full$predtype == "BRT"]
    gam_range <- gam_current > thresholds_full$thr[thresholds_full$predtype == "GAM"]
    lasso_range <- lasso_current > thresholds_full$thr[thresholds_full$predtype == "LASSO"]
    maxent_range <- maxent_current > thresholds_full$thr[thresholds_full$predtype == "MAXENT"]
    rf_range <- rf_current > thresholds_full$thr[thresholds_full$predtype == "RF"]
    wtmn_all_range <- wtmn_all_current > thresholds_full$thr[thresholds_full$predtype == "wtmn_all"]
    
    brt_cr_plot <- ggplot(data = as.data.frame(brt_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("BRT")
    gam_cr_plot <- ggplot(data = as.data.frame(gam_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("GAM")
    lasso_cr_plot <- ggplot(data = as.data.frame(lasso_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = last)) +
      ggtitle("LASSO")
    max_cr_plot <- ggplot(data = as.data.frame(maxent_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("MAX")
    rf_cr_plot <- ggplot(data = as.data.frame(rf_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("RF")
    wtmn_all_cr_plot <- ggplot(data = as.data.frame(wtmn_all_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr.1)) +
      ggtitle("WT MEAN")
    
    range_theme <- list(
      scale_fill_manual(values = c("gray90", "forestgreen")),
      coord_quickmap(),
      theme_classic(),
      theme(text = element_text(size = 8),
            legend.title = element_blank(),
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.15, "in"),
            legend.margin = margin(0, 0, 0, 0, "in"),
            axis.title = element_blank(), 
            axis.line = element_blank(),
            plot.title = element_text(hjust = 0.5, vjust = 0, size = 8),
            panel.border = element_rect(colour = "black", fill = NA),
            plot.margin = unit(c(0.05, 0.05, 0, 0.05), "in"))
    )
    
    if (length(bad.models) == 0) {
      range_cur <- plot_grid(brt_cr_plot + range_theme,
                             gam_cr_plot + range_theme,
                             lasso_cr_plot + range_theme,
                             max_cr_plot + range_theme,
                             rf_cr_plot + range_theme,
                             wtmn_all_cr_plot + range_theme,
                             nrow = 2, ncol = 3)
    } else {
      wtmn_good_range <- wtmn_good_current > thresholds_full$thr[thresholds_full$predtype == "wtmn_good"]
      wtmn_good_cr_plot <- ggplot(data = as.data.frame(wtmn_good_range, xy = TRUE)) +
        geom_tile(aes(x = x, y = y, fill = lyr.1)) +
        ggtitle("WT MEAN, GOOD")
      range_cur <- plot_grid(brt_cr_plot + range_theme,
                             gam_cr_plot + range_theme,
                             lasso_cr_plot + range_theme,
                             max_cr_plot + range_theme,
                             rf_cr_plot + range_theme,
                             wtmn_all_cr_plot + range_theme,
                             wtmn_good_cr_plot + range_theme,
                             nrow = 3, ncol = 3)
    }
    
    title <- ggdraw() + draw_label(paste0(short_name, ", Current, FullThr, 9 vars"), size = 9)
    range_c <- plot_grid(title, range_cur, ncol=1, rel_heights = c(0.1, 3.5))
    ggsave(plot = range_c,
           file = paste0("development/output/maps/", 
                         nice_name, "-cr-9var-fullthr.jpg"),
           device = "jpeg",
           width = 6.5,
           height = 4.5,
           units = "in")
    
  # Visualize predicted suitabilities, Future #################################### 
    brt_f_plot <- ggplot(data = as.data.frame(brt_future, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("BRT")
    gam_f_plot <- ggplot(data = as.data.frame(gam_future, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("GAM")
    lasso_f_plot <- ggplot(data = as.data.frame(lasso_future, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = last)) +
      ggtitle("LASSO")
    max_f_plot <- ggplot(data = as.data.frame(maxent_future, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("MAX")
    rf_f_plot <- ggplot(data = as.data.frame(rf_future, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("RF")
    wtmn_all_f_plot <- ggplot(data = as.data.frame(wtmn_all_future, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr.1)) +
      ggtitle("WT MEAN")
    
    if (length(bad.models) == 0) {
      suit_fut <- plot_grid(brt_f_plot + suit_theme,
                            gam_f_plot + suit_theme,
                            lasso_f_plot + suit_theme,
                            max_f_plot + suit_theme,
                            rf_f_plot + suit_theme,
                            wtmn_all_f_plot + suit_theme,
                            nrow = 2, ncol = 3)
    } else {
      wtmn_good_f_plot <- ggplot(data = as.data.frame(wtmn_good_future, xy = TRUE)) +
        geom_tile(aes(x = x, y = y, fill = lyr.1)) +
        ggtitle("WT MEAN, GOOD")
      suit_fut <- plot_grid(brt_f_plot + suit_theme,
                            gam_f_plot + suit_theme,
                            lasso_f_plot + suit_theme,
                            max_f_plot + suit_theme,
                            rf_f_plot + suit_theme,
                            wtmn_all_f_plot + suit_theme,
                            wtmn_good_f_plot + suit_theme,
                            nrow = 3, ncol = 3)
    }
    
    title <- ggdraw() + draw_label(paste0(short_name, ", Future, 9 vars"), size = 9)
    suit_f <- plot_grid(title, suit_fut, ncol=1, rel_heights = c(0.1, 3.5))
    ggsave(plot = suit_f,
           file = paste0("development/output/maps/", 
                         nice_name, "-fp-9var.jpg"),
           device = "jpeg",
           width = 6.5,
           height = 4.5,
           units = "in")
  
  # Visualize predicted ranges, Future, CV thresholds ############################ 
    brt_f_range <- brt_future > thresholds$thr[thresholds$predtype == "BRT"]
    gam_f_range <- gam_future > thresholds$thr[thresholds$predtype == "GAM"]
    lasso_f_range <- lasso_future > thresholds$thr[thresholds$predtype == "LASSO"]
    maxent_f_range <- maxent_future > thresholds$thr[thresholds$predtype == "MAXENT"]
    rf_f_range <- rf_future > thresholds$thr[thresholds$predtype == "RF"]
    wtmn_all_f_range <- wtmn_all_future > thresholds$thr[thresholds$predtype == "wtmn_all"]
    
    brt_fr_plot <- ggplot(data = as.data.frame(brt_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("BRT")
    gam_fr_plot <- ggplot(data = as.data.frame(gam_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("GAM")
    lasso_fr_plot <- ggplot(data = as.data.frame(lasso_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = last)) +
      ggtitle("LASSO")
    max_fr_plot <- ggplot(data = as.data.frame(maxent_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("MAX")
    rf_fr_plot <- ggplot(data = as.data.frame(rf_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("RF")
    wtmn_all_fr_plot <- ggplot(data = as.data.frame(wtmn_all_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr.1)) +
      ggtitle("WT MEAN")
    
    if (length(bad.models) == 0) {
      range_fut <- plot_grid(brt_fr_plot + range_theme,
                             gam_fr_plot + range_theme,
                             lasso_fr_plot + range_theme,
                             max_fr_plot + range_theme,
                             rf_fr_plot + range_theme,
                             wtmn_all_fr_plot + range_theme,
                             nrow = 2, ncol = 3)
    } else {
      wtmn_good_f_range <- wtmn_good_future > thresholds$thr[thresholds$predtype == "wtmn_good"]
      wtmn_good_fr_plot <- ggplot(data = as.data.frame(wtmn_good_f_range, xy = TRUE)) +
        geom_tile(aes(x = x, y = y, fill = lyr.1)) +
        ggtitle("WT MEAN, GOOD")
      range_fut <- plot_grid(brt_fr_plot + range_theme,
                             gam_fr_plot + range_theme,
                             lasso_fr_plot + range_theme,
                             max_fr_plot + range_theme,
                             rf_fr_plot + range_theme,
                             wtmn_all_fr_plot + range_theme,
                             wtmn_good_fr_plot + range_theme,
                             nrow = 3, ncol = 3)
    }
    
    title <- ggdraw() + draw_label(paste0(short_name, ", Future, CVThr, 9 vars"), size = 9)
    range_f <- plot_grid(title, range_fut, ncol=1, rel_heights = c(0.1, 3.5))
    ggsave(plot = range_f,
           file = paste0("development/output/maps/", 
                         nice_name, "-fr-9var-CVthr.jpg"),
           device = "jpeg",
           width = 6.5,
           height = 4.5,
           units = "in")
    
  # Visualize predicted ranges, Future, Full thresholds ########################## 
    brt_f_range <- brt_future > thresholds_full$thr[thresholds_full$predtype == "BRT"]
    gam_f_range <- gam_future > thresholds_full$thr[thresholds_full$predtype == "GAM"]
    lasso_f_range <- lasso_future > thresholds_full$thr[thresholds_full$predtype == "LASSO"]
    maxent_f_range <- maxent_future > thresholds_full$thr[thresholds_full$predtype == "MAXENT"]
    rf_f_range <- rf_future > thresholds_full$thr[thresholds_full$predtype == "RF"]
    wtmn_all_f_range <- wtmn_all_future > thresholds_full$thr[thresholds_full$predtype == "wtmn_all"]
    
    brt_fr_plot <- ggplot(data = as.data.frame(brt_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("BRT")
    gam_fr_plot <- ggplot(data = as.data.frame(gam_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr1)) +
      ggtitle("GAM")
    lasso_fr_plot <- ggplot(data = as.data.frame(lasso_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = last)) +
      ggtitle("LASSO")
    max_fr_plot <- ggplot(data = as.data.frame(maxent_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("MAX")
    rf_fr_plot <- ggplot(data = as.data.frame(rf_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = layer)) +
      ggtitle("RF")
    wtmn_all_fr_plot <- ggplot(data = as.data.frame(wtmn_all_f_range, xy = TRUE)) +
      geom_tile(aes(x = x, y = y, fill = lyr.1)) +
      ggtitle("WT MEAN")
    
    if (length(bad.models) == 0) {
      range_fut <- plot_grid(brt_fr_plot + range_theme,
                             gam_fr_plot + range_theme,
                             lasso_fr_plot + range_theme,
                             max_fr_plot + range_theme,
                             rf_fr_plot + range_theme,
                             wtmn_all_fr_plot + range_theme,
                             nrow = 2, ncol = 3)
    } else {
      wtmn_good_f_range <- wtmn_good_future > thresholds_full$thr[thresholds_full$predtype == "wtmn_good"]
      wtmn_good_fr_plot <- ggplot(data = as.data.frame(wtmn_good_f_range, xy = TRUE)) +
        geom_tile(aes(x = x, y = y, fill = lyr.1)) +
        ggtitle("WT MEAN, GOOD")
      range_fut <- plot_grid(brt_fr_plot + range_theme,
                             gam_fr_plot + range_theme,
                             lasso_fr_plot + range_theme,
                             max_fr_plot + range_theme,
                             rf_fr_plot + range_theme,
                             wtmn_all_fr_plot + range_theme,
                             wtmn_good_fr_plot + range_theme,
                             nrow = 3, ncol = 3)
    }
    
    title <- ggdraw() + draw_label(paste0(short_name, ", Future, FullThr, 9 vars"), size = 9)
    range_f <- plot_grid(title, range_fut, ncol=1, rel_heights = c(0.1, 3.5))
    ggsave(plot = range_f,
           file = paste0("development/output/maps/", 
                         nice_name, "-fr-9var-fullthr.jpg"),
           device = "jpeg",
           width = 6.5,
           height = 4.5,
           units = "in")
}  
  
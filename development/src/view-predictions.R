# View predicted suitability and range maps for each species
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-09-07

require(stringr)
require(raster)
require(terra)
require(dplyr)
require(dismo)

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
i = 2
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

# Visualize predicted suitabilities - current time period
if (length(bad.models) > 0) {
  par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
  plot(brt_current, main = paste0("BRT, ", short_name))
  plot(gam_current, main = paste0("GAM, ", short_name))
  plot(lasso_current, main = paste0("LASSO, ", short_name))
  plot(maxent_current, main = paste0("MAX, ", short_name))
  plot(rf_current, main = paste0("RF, ", short_name))
  plot(wtmn_all_current, main = paste0("WTMN.ALL, ", short_name))
  plot(wtmn_good_current, main = paste0("WTMN.GOOD, ", short_name))
} else {
  par(mfrow = c(2, 3), mar = c(1, 1, 1, 1))
  plot(brt_current, main = paste0("BRT, ", short_name))
  plot(gam_current, main = paste0("GAM, ", short_name))
  plot(lasso_current, main = paste0("LASSO, ", short_name))
  plot(maxent_current, main = paste0("MAX, ", short_name))
  plot(rf_current, main = paste0("RF, ", short_name))
  plot(wtmn_all_current, main = paste0("WTMN.ALL, ", short_name))
}

# Visualize predicted ranges - current time period
if (length(bad.models) > 0) {
  par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
  plot(brt_current > thresholds$thr[thresholds$predtype == "BRT"], 
       main = paste0("BRT, ", short_name))
  plot(gam_current > thresholds$thr[thresholds$predtype == "GAM"], 
       main = paste0("GAM, ", short_name))
  plot(lasso_current > thresholds$thr[thresholds$predtype == "LASSO"], 
       main = paste0("LASSO, ", short_name))
  plot(maxent_current > thresholds$thr[thresholds$predtype == "MAXENT"], 
       main = paste0("MAX, ", short_name))
  plot(rf_current > thresholds$thr[thresholds$predtype == "RF"], 
       main = paste0("RF, ", short_name))
  plot(wtmn_all_current > thresholds$thr[thresholds$predtype == "wtmn_all"], 
       main = paste0("WTMN.ALL, ", short_name))
  plot(wtmn_good_current > thresholds$thr[thresholds$predtype == "wtmn_good"], 
       main = paste0("WTMN.GOOD, ", short_name))
} else {
  par(mfrow = c(2, 3), mar = c(1, 1, 1, 1))
  plot(brt_current > thresholds$thr[thresholds$predtype == "BRT"], 
       main = paste0("BRT, ", short_name))
  plot(gam_current > thresholds$thr[thresholds$predtype == "GAM"], 
       main = paste0("GAM, ", short_name))
  plot(lasso_current > thresholds$thr[thresholds$predtype == "LASSO"], 
       main = paste0("LASSO, ", short_name))
  plot(maxent_current > thresholds$thr[thresholds$predtype == "MAXENT"], 
       main = paste0("MAX, ", short_name))
  plot(rf_current > thresholds$thr[thresholds$predtype == "RF"], 
       main = paste0("RF, ", short_name))
  plot(wtmn_all_current > thresholds$thr[thresholds$predtype == "wtmn_all"], 
       main = paste0("WTMN.ALL, ", short_name))
}

# Visualize predicted suitabilities - future time period
if (length(bad.models) > 0) {
  par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
  plot(brt_future, main = paste0("BRT, ", short_name))
  plot(gam_future, main = paste0("GAM, ", short_name))
  plot(lasso_future, main = paste0("LASSO, ", short_name))
  plot(maxent_future, main = paste0("MAX, ", short_name))
  plot(rf_future, main = paste0("RF, ", short_name))
  plot(wtmn_all_future, main = paste0("WTMN.ALL, ", short_name))
  plot(wtmn_good_future, main = paste0("WTMN.GOOD, ", short_name))
} else {
  par(mfrow = c(2, 3), mar = c(1, 1, 1, 1))
  plot(brt_future, main = paste0("BRT, ", short_name))
  plot(gam_future, main = paste0("GAM, ", short_name))
  plot(lasso_future, main = paste0("LASSO, ", short_name))
  plot(maxent_future, main = paste0("MAX, ", short_name))
  plot(rf_future, main = paste0("RF, ", short_name))
  plot(wtmn_all_future, main = paste0("WTMN.ALL, ", short_name))
}

# Visualize predicted ranges - future time period
if (length(bad.models) > 0) {
  par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
  plot(brt_future > thresholds$thr[thresholds$predtype == "BRT"], 
       main = paste0("BRT, ", short_name))
  plot(gam_future > thresholds$thr[thresholds$predtype == "GAM"], 
       main = paste0("GAM, ", short_name))
  plot(lasso_future > thresholds$thr[thresholds$predtype == "LASSO"], 
       main = paste0("LASSO, ", short_name))
  plot(maxent_future > thresholds$thr[thresholds$predtype == "MAXENT"], 
       main = paste0("MAX, ", short_name))
  plot(rf_future > thresholds$thr[thresholds$predtype == "RF"], 
       main = paste0("RF, ", short_name))
  plot(wtmn_all_future > thresholds$thr[thresholds$predtype == "wtmn_all"], 
       main = paste0("WTMN.ALL, ", short_name))
  plot(wtmn_good_future > thresholds$thr[thresholds$predtype == "wtmn_good"], 
       main = paste0("WTMN.GOOD, ", short_name))
} else {
  par(mfrow = c(2, 3), mar = c(1, 1, 1, 1))
  plot(brt_future > thresholds$thr[thresholds$predtype == "BRT"], 
       main = paste0("BRT, ", short_name))
  plot(gam_future > thresholds$thr[thresholds$predtype == "GAM"], 
       main = paste0("GAM, ", short_name))
  plot(lasso_future > thresholds$thr[thresholds$predtype == "LASSO"], 
       main = paste0("LASSO, ", short_name))
  plot(maxent_future > thresholds$thr[thresholds$predtype == "MAXENT"], 
       main = paste0("MAX, ", short_name))
  plot(rf_future > thresholds$thr[thresholds$predtype == "RF"], 
       main = paste0("RF, ", short_name))
  plot(wtmn_all_future > thresholds$thr[thresholds$predtype == "wtmn_all"], 
       main = paste0("WTMN.ALL, ", short_name))
}

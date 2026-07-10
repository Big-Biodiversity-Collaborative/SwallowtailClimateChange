# Look at the predicted suitabilities for the four hosts of P. cresphontes
# Jeff Oliver
# jcoliver@arizona.edu
# 2026-07-10

library(terra)
library(dplyr)

ih <- read.csv(file = "data/insect-host.csv")
papcre <- ih %>% 
  filter(insect == "Papilio cresphontes") %>%
  select(insect, host_accepted) %>%
  distinct()

for (host in papcre$host_accepted) {
  nice_name <- tolower(gsub(pattern = " ",
                            replacement = "_",
                            x = host))
  current_suit <- readRDS(file = paste0("output/distributions/",
                                        nice_name,
                                        "-distribution-current.rds"))
  forecast_suit <- readRDS(file = paste0("output/distributions/",
                                         nice_name,
                                         "-distribution-ensemble_ssp370_2041.rds"))
  plot(current_suit, main = paste0(host, ", current"))
  plot(forecast_suit, main = paste0(host, ", forecast"))
}

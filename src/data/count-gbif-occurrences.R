# Estimate amount of GBIF data in each decade
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-04-30

require(dplyr)

# Extracts data/gbif folder (that contains individual csvs) from zip file
unzip(zipfile = "data/gbif.zip") 

gbif_files <- list.files(path = "data/gbif",
                         pattern = "*-gbif.csv",
                         full.names = TRUE)

# Create decadal bins for occurrence records (and one pre-1900 bin)
decades <- c(0, seq(1900, 2030, by=10))

# Create empty dataframe to store number of records in each decade 
# (e.g., d2000 = 2000-2009)
spp_nocc <- data.frame(matrix(NA, nrow = length(gbif_files), ncol = 16))
colnames(spp_nocc) <- c("species", 
                        "pre1900", 
                        paste0("d",seq(1900, 2020, by = 10)),
                        "date_missing")

for(i in 1:length(gbif_files)){
  dat <- read.csv(file = gbif_files[i])
  spp_nocc[i,1] <- dat$accepted_name[1]
  
  # Count the number of occurrence records in each decade
  spp_nocc[i,2:15] <- as.numeric(table(cut(dat$year, 
                                           breaks = decades, 
                                           right = FALSE)))
  
  # Count the number of records where year = NA
  spp_nocc[i,16] <- sum(is.na(dat$year))
}  

write.csv(spp_nocc, 
          file = "data/gbif-noccurrences.csv",
          row.names = FALSE)


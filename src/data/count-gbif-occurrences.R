# Estimate data reduction when restricting GBIF data to 2000-2018
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-04-30

require(dplyr)

# Extracts data/gbif folder (that contains individual csvs) from zip file
unzip(zipfile = "data/gbif.zip") 

gbif_files <- list.files(path = "data/gbif",
                         pattern = "*-gbif.csv",
                         full.names=TRUE)
year_list <- 2000:2018 

for(one_file in gbif_files){
  dat <- read.csv(file = one_file)
  
  # Count the total number of occurrence records (note that this includes records where year = NA)
  total <- nrow(dat)
  
  # Count the number of occurrences in 2000-2018
  recent <- sum(dat$year %in% year_list)
  
  if (one_file == gbif_files[1]) {
    spp_nocc <- data.frame(species=dat$species[1],total_occ=total,recent_occ=recent)
  } else {
    spp_nocc <- spp_nocc %>% bind_rows(data.frame(species=dat$species[1],total_occ=total,recent_occ=recent))
  }
}  

write.csv(spp_nocc, 
          file = "data/gbif_noccurrences.csv",
          row.names = FALSE)

# Need to remove data/gbif folder?

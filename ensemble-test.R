genus <- "Papilio"
species <- "cresphontes"
ih <- read.csv(file = "data/insect-host.csv")
ih <- ih[ih$insect == paste0(genus, " ", species), ]
nice_insect_name <- tolower(gsub(pattern = " ",
                                 replacement = "_",
                                 x = paste(genus, species)))

glm_model_script <- paste0("src/indiv/", nice_insect_name,
                           "-model-glm.R")
source(glm_model_script)

glm_prediction_script <- paste0("src/indiv/", nice_insect_name,
                           "-prediction-glm.R")
source(glm_prediction_script)

hosts <- ih$host_accepted
for (host in hosts) {
  nice_host_name <- tolower(host)
  nice_host_name <- gsub(pattern = " ", replacement = "_", x = nice_host_name)
  sourcefile <- paste0("src/indiv/", nice_host_name, 
                       "-model-glm.R")
  message(paste0("Abount to run ", sourcefile))
  source(file = sourcefile)
  
  sourcefile <- paste0("src/indiv/", nice_host_name, 
                       "-prediction-glm.R")
  message(paste0("Abount to run ", sourcefile))
  source(file = sourcefile)
}

glm_overlap_script <- paste0("src/indiv/", nice_insect_name,
                             "-overlap-raster-glm.R")
source(file = glm_overlap_script)

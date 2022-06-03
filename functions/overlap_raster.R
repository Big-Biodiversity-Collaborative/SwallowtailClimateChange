#' RasterLayer with ranges of insect and host plant species based on SDM 
#' predictions
#' 
#' @param species_name character vector with name of insect species, e.g. 
#' "Papilio multicaudata"
#' @param predictor character vector indicating which global climate models on 
#' which predictions are based
#' @param model character vector of model used to generate species distribution
#' model
#' 
#' @return a \code{RasterLayer} with cell values reflecting predicted 
#' distribution for insect and host species. Pixels take one of four values:
#' \describe{
#'   \item{0}{Insect and all host plant species absent}
#'   \item{1}{Insect present, but all host plant species absent}
#'   \item{2}{At least one host plant species present, but insect absent}
#'   \item{3}{Insect and at least one host plant species present}
#' } 
overlap_raster <- function(species_name, 
                           predictor,
                           model = c("glm", "svm")) {
  if (!require(raster)) {
    stop("overlap_raster requires raster package, but it could not be loaded")
  }
  # Load up the functions from the functions folder
  source(file = "load_functions.R")

  # predictor <- match.arg(predictor)
  model <- match.arg(model)
  
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))

  # identify all host plants used by that insect
  insects_hosts <- read.csv(file = "data/insect-host.csv")
  hosts <- insects_hosts$host_accepted[insects_hosts$insect == species_name]
  
  # Make sure we have the distribution of the insect before proceeding  
  insect_dist_file <- paste0("output/distributions/",
                             nice_name, 
                             "-distribution-",
                             model,
                             "-",
                             predictor,
                             ".rds")
  
  if (!file.exists(insect_dist_file)) {
    message(paste0("No distributions found for ", species_name, " based on ",
                   predictor, " predictors; no overlap raster created."))
    return(NULL)
  }
  
  # pull in presence / absence raster of insect
  insect_pa <- readRDS(file = insect_dist_file)
  
  # pull in presence / absence rasters of plants this bug eats
  if (length(hosts) >= 1) {
    host_pa_list <- list()
    for (host_name in hosts) {
      host_nice_name <- tolower(x = gsub(pattern = " ",
                                         replacement = "_",
                                         x = host_name))
      host_dist_file <- paste0("output/distributions/",
                               host_nice_name,
                               "-distribution-",
                               model,
                               "-",
                               predictor,
                               ".rds")
      
      # Only try to add distributions if the file exists
      if (file.exists(host_dist_file)) {
        host_pa_list[[host_nice_name]] <- readRDS(file = host_dist_file)
      } else {
        message(paste0("Skipping host ", host_name, 
                       ", no distribution found for ", predictor))
      }
    } # end iterating over all host species
    
    # Make sure at least only host had a map, otherwise go no further, message, 
    # and return NULL
    if (length(host_pa_list) == 0) {
      message(paste0("No corresponding host distributions for ", species_name,
                     " could be found; no overlap raster created."))
      return(NULL)
    }
    
    # Stack all those rasters (should work even for monophagous species)
    host_pa <- stack_rasters(r = host_pa_list, out = "binary")
    
    # Will need to set values to 2 for hosts to get different values
    host_pa[host_pa >= 1] <- 2
    
    # Finally, we do one more stack, 
    all_pa <- stack_rasters(r = list(insect_pa, host_pa))
    
    # Raster is done, send it back
    return(all_pa)

  } else {
    # No host plants listed in file, message and return NULL
    message(paste0("No host plants listed for ", species_name, 
                   "; no overlap raster created."))
    return(NULL)
  }
}

#' Execute an R script and catch errors or warnings
#' 
#' @param script_name Path to R script
#' @param log_file Path to file to write output to; if NULL, will print messages
#' to screen
#' 
#' @return If script runs with no errors or warnings, returns value produced 
#' from script (which is often \code{NULL})
#' 
#' @examples 
#' \dontrun{
#' script_run <- exec_script(script_name = "src/indiv/aletes_acaulis-SDM-brt.R,
#'                           log_file = "logs/SDM-brt-out.log")
#' }
exec_script <- function(script_name, log_file = NULL) {
  tryCatch(
    {
      source(script_name)
      message("Finished running: ", script_name)
    },
    # Handle errors
    error = function(e) {
      error_message <- paste0("Error while running: ", script_name, ": ", e)
      # Only try writing to a log if a filename was passed
      write_to_log <- !is.null(log_file)
      # ...and the file exists
      # Can't figure out how to do this in one step since R has to evaluate 
      # ALL conditions in an if statement...
      if (write_to_log) {
        write_to_log <- file.exists(log_file)
      }
      
      if (write_to_log) {
        write(x = error_message, 
              file = log_file,
              append = TRUE)
        
      } else {
        message(error_message)
      }
      return(error_message)
    }, # end of error function
    warning = function(w) {
      warning_message <- paste0("Warning while running: ", script_name, ": ", w)
      
      # Only try writing to a log if a filename was passed
      write_to_log <- !is.null(log_file)
      # ...and the file exists
      if (write_to_log) {
        write_to_log <- file.exists(log_file)
      }
      
      if (write_to_log) {
        write(x = warning_message, 
              file = log_file,
              append = TRUE)
        
      } else {
        message(warning_message)
      }
      return(warning_message)
    }
  )
}
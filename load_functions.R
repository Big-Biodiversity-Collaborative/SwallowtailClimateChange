# Load functions from functions directory
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-08

# Usage: source(file = "load_functions.R")

function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

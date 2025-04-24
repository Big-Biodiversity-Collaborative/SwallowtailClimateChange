# Render the manuscript cheatsheet
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-04-24

require(quarto)

# Render the cheatsheet. Largely done in this script so we can automatically 
# (i.e. with R) move the resulting pdf file to the right spot in the output 
# folder.

quarto::quarto_render(input = "templates/cheatsheet.qmd",
                      output_format = "pdf",
                      output_file = "Manuscript-cheatsheet.pdf")

# Move the file to where we want it to be
if (file.copy(from = "Manuscript-cheatsheet.pdf",
              to = "output/manuscript/Manuscript-cheatsheet.pdf",
              overwrite = TRUE)) {
  invisible(file.remove("Manuscript-cheatsheet.pdf"))
}

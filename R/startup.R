loadPaths <- function() {
  # package rprojroot required
  library(rprojroot)
  
  root <- rprojroot::is_rstudio_project
  root_file <- root$make_fix_file()
  cat("Setting up the project folders:\n")
  project.data <- root_file("data")
  project.extdata <- root_file("inst/extdata")
  project.R <- root_file("R")
  project.notebooks <- root_file("inst/notebooks")
  project.matlab <- root_file("inst/matlab")
  ret <- list(data = project.data, 
              extdata = project.extdata,
              R = project.R,
              notebooks = project.notebooks,
              matlab = project.matlab
  )
  print(ret)
  return(ret)
}
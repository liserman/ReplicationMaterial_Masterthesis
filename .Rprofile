.First <- function() {
  dir.create(paste0(getwd(), "/figures"), showWarnings = F)
  dir.create(paste0(getwd(), "/tables"), showWarnings = F)
  dir.create(paste0(getwd(), "/processed-data"), showWarnings = F)
  dir.create(paste0(getwd(), "/raw-data"), showWarnings = F)
  dir.create(paste0(getwd(), "/scripts"), showWarnings = F)
  
  if (!("renv" %in% list.files())) {
    renv::init()
  } else {
    source("renv/activate.R")
  }
  
  cat("\nWelcome to your R-Project:", basename(getwd()), "\n")
}
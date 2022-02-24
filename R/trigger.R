Sys.setenv(MAPSPAIN_CACHE_DIR = "data")


auth_as("create_token")
source("R/01_create_map.R")

# message("Start to create the journey")
# library(rmarkdown)
# rmarkdown::render("R/journey.Rmd", output_dir = "_pages/",
#                   output_format = "md_document", quiet = TRUE)

rm(list = ls())




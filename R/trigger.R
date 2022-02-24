source("R/01_create_map.R")

message("Start to create the journey")
library(rmarkdown)
rmarkdown::render("R/journey.Rmd", output_dir = "_pages/",
                  output_format = "md_document", quiet = TRUE)


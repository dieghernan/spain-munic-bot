options("rgdal_show_exportToProj4_warnings"="none")

hlp_install <- function(pkg){
  if (isFALSE(requireNamespace(pkg, quietly = TRUE))) {
    install.packages(pkg)
  }
}

hlp_install("mapSpain")
hlp_install("sf")
hlp_install("dplyr")
hlp_install("tmap")
hlp_install("slippymath")
hlp_install("stars")
hlp_install("rgdal")
hlp_install("osmdata")
hlp_install("rmarkdown")


source("R/01_create_map.R")

message("Create dashboard")
source("R/02_dashboard.R")

message("Start to create the journey")
library(rmarkdown)
rmarkdown::render("R/journey.Rmd", output_dir = "_pages/",
                  output_format = "md_document", quiet = TRUE)


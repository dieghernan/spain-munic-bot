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

output_dir <- file.path("data", "archive")

if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
} else {
    print("Dir already exists!")
}

source("R/01_create_map.R")






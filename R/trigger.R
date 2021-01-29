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

output_dir <- file.path("assets", "img","archive_satellite")

if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
} else {
    print("Dir already exists!")
}

output_dir <- file.path("assets", "img","archive_streets")

if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
} else {
  print("Dir already exists!")
}


source("R/01_create_map.R")

library(rmarkdown)

rmarkdown::render("R/journey.Rmd", output_dir = "_pages/",
                  output_format = "md_document")



# Manage cache, keep only 20 random images
f <- list.files(file.path("assets","img","archive_satellite"),
                full.names = TRUE)
fl <- length(f)
d <- sample.int(fl,min(fl,20))
fd <- f[-d]
file.remove(fd)
rm(list = ls())

f <- list.files(file.path("assets","img","archive_streets"),
                full.names = TRUE)
fl <- length(f)
d <- sample.int(fl,min(fl,20))
fd <- f[-d]
file.remove(fd)

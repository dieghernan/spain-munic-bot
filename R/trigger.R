Sys.setenv(MAPSPAIN_CACHE_DIR = "data")

for (i in 1:3) {
  source("R/01_create_map.R")
  Sys.sleep(3)
}

rm(list = ls())

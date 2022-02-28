Sys.setenv(MAPSPAIN_CACHE_DIR = "data")


source("R/01_create_map.R")

message("Start with second")
source("R/01_create_map.R")

message("Start with third")
source("R/01_create_map.R")


rm(list = ls())

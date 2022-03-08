Sys.setenv(MAPSPAIN_CACHE_DIR = "data")


try(source("R/01_create_map.R"))

message("Start with second")
try(source("R/01_create_map.R"))

message("Start with third")
try(source("R/01_create_map.R"))


rm(list = ls())

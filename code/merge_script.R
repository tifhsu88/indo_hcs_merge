## Merge concession and conservation area feature classes in geodatabases

# load packages
library(tidyverse)
library(sf)
library(here)

# read simple features from files

#this file doesn't open
hcsa_0083 <- st_read(dsn = here("data/hcsa_0083/hcsa_digitization.gdb"))

#this sample gdb from https://gisdata-piercecowa.opendata.arcgis.com/datasets/piercecowa::development-engineering-mobile-homes/explore opens
sample_gdb <- st_read(dsn = here("data/sample_gdb.gdb"))

# # get the list of files in the directory
# file_list <- list.files(here("insert_directory_here"), pattern = *gdb)

# read files
# gdb_list <- lapply(file_list, st_read)

# merge the files into a single dataframe
#create for loop 
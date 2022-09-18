## Merge concession and conservation area feature classes in geodatabases

# load packages
library(tidyverse)
library(sf)
library(here)


#this file doesn't open
hcsa_0083 <- st_read(dsn = here("data/hcsa_0083/hcsa_digitization.gdb"))

#this one does
gdb <- "remote/3_digitization/2022_digitization/hcsa_0083/hcsa_digitization.gdb"
fc <- st_read(gdb, layer = 'concession_boundaries') # Since geodatabase contains multiple layers, need to specify which one you want to open

#this sample gdb from https://gisdata-piercecowa.opendata.arcgis.com/datasets/piercecowa::development-engineering-mobile-homes/explore opens
sample_gdb <- st_read(dsn = here("data/sample_gdb.gdb"))

# # get the list of files in the directory
# file_list <- list.files(here("insert_directory_here"), pattern = *gdb)

# read files
# gdb_list <- lapply(file_list, st_read)

# merge the files into a single dataframe
#create for loop 
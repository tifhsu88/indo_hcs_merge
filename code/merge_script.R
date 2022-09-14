
# load packages
library(tidyverse)
library(sf)
library(here)


#import shapefiles as sf objects

#this doesn't open
hcsa_0083 <- st_read(dsn = here("data/hcsa_0083/hcsa_digitization.gdb")) #, layer = "conservation_areas")

# this sample gdb from https://gisdata-piercecowa.opendata.arcgis.com/datasets/piercecowa::development-engineering-mobile-homes/explore works
sample_gdb <- st_read(dsn = here("data/sample_gdb.gdb"))


# merge the concession boundaries and conservation areas


#merging files into one directory

# #set working directory to the one containing all the files that need to be merged,
# 
# # get the list of files in that directory
# file_list <- list.files(here("insert_directory_here"), pattern = *gdb)
# 

# #read files
# gdb_list <- lapply(file_list, st_read)

# # merge the files into a single dataframe
# 
# for (file in file_list){
#   
#   # if the merged dataset doesn't exist, create it
#   if (!exists("dataset")){
#     dataset <- read.table(file, header=TRUE, sep="\t")
#   }
#   
#   # if the merged dataset does exist, append to it
#   if (exists("dataset")){
#     temp_dataset <-read.table(file, header=TRUE, sep="\t")
#     dataset<-rbind(dataset, temp_dataset)
#     rm(temp_dataset)
#   }
#   
# }
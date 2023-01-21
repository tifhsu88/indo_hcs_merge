## Merge concession and conservation area feature classes in geodatabases

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Package imports --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(sf)

# Check if file can open
hcsa_0083_gdb <- "remote/3_digitization/2022_digitization/hcsa_0083/hcsa_digitization.gdb"
hcsa_0083_fc <- st_read(hcsa_0083_gdb, layer = 'conservation_areas') # Since geodatabase contains multiple layers, need to specify which one you want to open

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define paths -----------------------------------------------------------
#%%%%%%%%%%%%a%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

root_data_dir <- 'remote/3_digitization/2022_digitization/'
report_list <- c('hcsa_0005',
                 'hcsa_0006',
                 'hcsa_0011',
                 'hcsa_0012',
                 'hcsa_0013',
                 'hcsa_0014',
                 'hcsa_0020',
                 'hcsa_0023',
                 'hcsa_0024', #needs to be tweaked
                 'hcsa_0025',
                 'hcsa_0027',
                 'hcsa_0034',
                 'hcsa_0083', 
                 'hcsa_0146', 
                 'hcsa_0152', 
                 'hcsa_0155', 
                 'hcsa_0159',
                 'hcsa_0170')




#' report_list <- c(#'hcsa_0005', # unable to open layer
#'                  #'hcsa_0006', # unable to open layer
#'                  'hcsa_0034', 
#'                  'hcsa_0083', 
#'                  'hcsa_0146', 
#'                  'hcsa_0152', 
#'                  'hcsa_0155', 
#'                  # 'hcsa_0157', # no concession boundary or conservation areas digitized
#'                  'hcsa_0159', # benise had typed in wrong report file code, manually fixed
#'                  'hcsa_0170')

# Output path for new, merged dataset
out_file <- 'remote/4_merging/hcsa_merging/merged_hcs.gpkg'

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Open and merge digitized conservation_areas-----------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# r <- report_list[7]
conservation_areas <- list()

## TODO: Add crs match check

for (i in 1:length(report_list)){
  r <- report_list[i]
  gdb <- paste0(root_data_dir, r, "/", 'hcsa_digitization.gdb')
  fc <- st_read(gdb, layer = 'conservation_areas')
  fc$code = r
  conservation_areas[[i]] <- fc
}

# turns the list into dataframes
#conservation_areas_binded <- map_dfr(conservation_areas, rbind)

# separate HCS and HCV columns 
conservation_areas_binded <- conservation_areas %>%
  # convert list to dataframe
  map_dfr(rbind) 

conservation_areas_binded <- conservation_areas_binded %>%
  # separate HCS and HCV columns based on conservation_type
  mutate(HCS = case_when(
    conservation_type == "HCV only" ~ FALSE,
    conservation_type == "HCS only" ~ TRUE,
    conservation_type == "HCV AND HCS" ~ TRUE,
    conservation_type == "HCV OR HCS" ~ TRUE # NOTE: Somewhat arbitrary decision to assign this to both
  )) %>% 
  mutate(HCV = case_when(
    conservation_type == "HCV only" ~ TRUE,
    conservation_type == "HCS only" ~ FALSE,
    conservation_type == "HCV AND HCS" ~ TRUE,
    conservation_type == "HCV OR HCS" ~ TRUE # NOTE: Somewhat arbitrary decision to assign this to both
    )) %>% 
  # drop conservation_type_column
  select(-conservation_type)

# Drop 4 empty features in hcsa_0034
conservation_areas_binded %>% filter(!conservation_areas_binded$Shape_Length == 0)

# # Convert multisurfaces (curved) into multipolygons
# conservation_areas_binded <- 
#   conservation_areas_binded %>% st_cast("MULTIPOLYGON")

# Create separate hcv and hcs datasets
hcv_df <- conservation_areas_binded %>% 
  filter(HCV == TRUE) %>% 
  select(code, HCV, Shape)
hcs_df <- conservation_areas_binded %>% 
  filter(HCS == TRUE) %>% 
  select(code, HCS, Shape)

## TODO? Add clip based on concession boundaries?


# add data to the output file
merged_map <- conservation_areas_binded %>% 
  st_write(out_file,
           layer = "conservation_areas",
           append = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Open and merge digitized concession_boundaries--------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
r <- report_list[7]
concession_boundaries <- list()

for (i in 1:length(report_list)){
  r <- report_list[i]
  gdb <- paste0(root_data_dir, r, "/", 'hcsa_digitization.gdb')
  fc <- st_read(gdb, layer = 'concession_boundaries')
  concession_boundaries[[i]] <- fc
}

# turns the list into dataframes
concession_boundaries_binded <- map_dfr(concession_boundaries, rbind)

merged_map <- concession_boundaries_binded %>% 
  st_write(out_file,
           layer = "concession_boundaries",
           append = FALSE)

# Open merged_hcs.gpkg to check the dfs
#merged_hcs <- st_read(out_file, layer = "concession_boundaries")
merged_hcs <- st_read(out_file, layer = "conservation_areas")

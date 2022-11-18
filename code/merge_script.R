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
report_list <- c(#'hcsa_0005', # unable to open layer
                 #'hcsa_0006', # unable to open layer
                 'hcsa_0034', 
                 'hcsa_0083', 
                 'hcsa_0146', 
                 'hcsa_0152', 
                 'hcsa_0155', 
                 # 'hcsa_0157', # no concession boundary or conservation areas digitized
                 'hcsa_0159', # benise had typed in wrong report file code, manually fixed
                 'hcsa_0170')

# Output path for new, merged dataset
out_file <- 'remote/4_merging/hcsa_merging/merged_hcs.gpkg'

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Open and merge digitized conservation_areas-----------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
r <- report_list[7]
conservation_areas <- list()

for (i in 1:length(report_list)){
  r <- report_list[i]
  gdb <- paste0(root_data_dir, r, "/", 'hcsa_digitization.gdb')
  fc <- st_read(gdb, layer = 'conservation_areas')
  conservation_areas[[i]] <- fc
}

# turns the list into dataframes
#conservation_areas_binded <- map_dfr(conservation_areas, rbind)

# separate HCS and HCV columns 
conservation_areas_binded <- conservation_areas %>%
  # convert list to dataframe
  map_dfr(rbind) %>%
  # separate HCS and HCV columns based on conservation_type
  mutate(HCS = case_when(
    conservation_type == "HCV only" ~ "no",
    conservation_type == "HCS only" ~ "yes",
    conservation_type == "HCV AND HCS" ~ "yes",
  )) %>% 
  mutate(HCV = case_when(
    conservation_type == "HCV only" ~ "yes",
    conservation_type == "HCS only" ~ "no",
    conservation_type == "HCV AND HCS" ~ "yes",
  )) %>% 
  # drop conservation_type_column
  select(-conservation_type)
  
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

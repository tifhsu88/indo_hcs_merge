#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Package imports --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(rgdal)
library(tidyverse)
library(sf)
library(tidylog)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define paths --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Note: Less than ideal since this is specific to Robert's file system 
# due to the fact that we're drawing from multiple projects

# New digitized datasets
root_data_dir <- 'D:/cloud/Dropbox/collaborations/prop_and_env/georef92/'
digitizer_list <- c('chavez_celeste', 'dickison_cole', 'hsu_tiffany', 
                    'huy_matthieu', 'santos_julia', 'villasenor_vicente')

# PropAndEnv original digitized datasets
original_gdb <- 'D:/cloud/Dropbox/collaborations/prop_and_env/GeoreferenceCensus1992_master/georeferenced_maps/georeferencing/georeferencing.gdb'

# Output path for new, merged dataset
out_file <- 'D:/cloud/Dropbox/collaborations/plantingPoverty/data/processed/georef92/sectors_georef.gpkg'



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Open and merge sectors digitized in 2021-2022 --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
d <- digitizer_list[6]
sectors <- list()

for (i in 1:length(digitizer_list)){
  d <- digitizer_list[i]
  gdb <- paste0(root_data_dir, d, '/georeferencing/georeferencing.gdb')
  fc <- st_read(gdb, layer = 'sectors')
  fc <- fc %>% 
    mutate(digitizer = d) %>% 
    replace_na(list(settlement = 0,
                    portafolio = -9999)) %>% 
    select(portafolio, district, digitizer, settlement, Shape) %>% 
    drop_na(c("Shape"))
  sectors[[i]] <- fc
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Add original data digitized in 2020-2021 for PropAndEnv project --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fc <- st_read(original_gdb, layer = 'sectors_merge')
fc <- fc %>% 
  mutate(digitizer = case_when(
    digitizer == "nisha" ~ "jagwota_nisha",
    digitizer == "julia" ~ "santos_julia",
    digitizer == "melanie" ~ "leung_melanie",
    TRUE ~ digitizer))  %>% 
  replace_na(list(settlement = 0,
                  portafolio = -9999)) %>% 
  select(portafolio, district, digitizer, settlement, Shape)

sectors[[i+1]] <- fc
sectors <- sectors %>% 
  reduce(rbind)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Deduplicate repeated sectors --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
duplicates_tab <- sectors %>% 
  as_tibble() %>% 
  dplyr::select(-Shape) %>% 
  group_by(portafolio) %>% 
  tally() %>% 
  arrange(desc(n))

duplicates <- duplicates_tab %>% 
  filter(n>1) %>% 
  pull(portafolio)

duplicates_tab %>% 
  filter(n>1) %>% 
  pull(n) %>% 
  sum()

sectors.dedup <- sectors[match(unique(sectors$portafolio), sectors$portafolio),] # Just taking first instance arbitrarily


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Save out final merged file --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sectors.dedup %>% 
  st_write(out_file, overwrite = TRUE, append = FALSE)

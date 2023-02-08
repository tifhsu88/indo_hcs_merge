## Merge concession and conservation area feature classes in geodatabases

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Package imports --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(sf)

# # Check if file can open
# hcsa_0083_gdb <- "remote/3_digitization/2022_digitization/hcsa_0083/hcsa_digitization.gdb"
# hcsa_0083_fc <- st_read(hcsa_0083_gdb, layer = 'conservation_areas') # Since geodatabase contains multiple layers, need to specify which one you want to open

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define paths -----------------------------------------------------------
#%%%%%%%%%%%%a%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
root_data_dir <- 'remote/3_digitization/2022_digitization/'
report_list <- c('hcsa_0005',
                 'hcsa_0006',
                 'hcsa_0008', # Note - some ambiguity between HCS/HCV areas in maps.
                 'hcsa_0010', # Note - uses HCSA give and take. We digitized pre give-and-take HCS map
                 'hcsa_0011',
                 'hcsa_0012',
                 'hcsa_0013',
                 'hcsa_0014',
                 # 'hcsa_0015', #
                 'hcsa_0020',
                 'hcsa_0021',
                 'hcsa_0023',
                 'hcsa_0024',
                 'hcsa_0025',
                 'hcsa_0027',
                 'hcsa_0034',
                 'hcsa_0083', 
                 'hcsa_0146', 
                 'hcsa_0152', 
                 'hcsa_0155', 
                 'hcsa_0159',
                 'hcsa_0170')


# Output path for new, merged dataset
out_file <- 'remote/4_merging/hcsa_merging/merged_hcs.gpkg'

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CEL undergrad digitizing -----------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# r <- report_list[7]
conservation_areas <- list()

for (i in 1:length(report_list)){
  r <- report_list[i]
  gdb <- paste0(root_data_dir, r, "/", 'hcsa_digitization.gdb')
  fc <- st_read(gdb, layer = 'conservation_areas')
  fc <- st_transform(fc, "EPSG:4326")
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

## TODO: Add clip based on concession boundaries?


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Michael digitizing -----------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# GAR original data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conservation_areas <- list()

gar_data_dir <- 'remote/1_original/gar_website/'

gar_list <- list("hcsa_0002" = "PT. Persada Graha Mandiri/pgm",
                 "hcsa_0040" = "PT. Paramitra Internusa Pratama/pip",
                 "hcsa_0042" = "PT. Buana Adhitama/bat",
                 "hcsa_0043" = "PT. Bangun Nusa Mandiri/bnm",
                 "hcsa_0044" = "PT. Agrolestari Mandiri/amnl",
                 "hcsa_0045" = "PT. Cahayanusa Gemilang/cng",
                 "hcsa_0046" = "PT. Kencana Graha Permai/kgp",
                 "hcsa_0047" = "PT. Mitrakarya Agroindo/mka",
                 "hcsa_0048" = "PT. Agrolestari Sentosa/als",
                 "hcsa_0049" = "PT. Aditunggal Mahajaya/atm",
                 "hcsa_0050" = "PT. Tapian Nadenggan/tnd",
                 "hcsa_0051" = "PT. Agrokarya Primalestari/akpl",
                 "hcsa_0052" = "PT. Kresna Duta Agroindo/kda",
                 "hcsa_0053" = "PT. Binasawit Abadipratama/bap",
                 "hcsa_0054" = "PT. Buana Artha Sejahtera/bas",
                 "hcsa_0055" = "PT. Bumi Sawit Permai/bsp",
                 "hcsa_0056" = "PT. Satya Kisma Usaha/sku",
                 "hcsa_0179" = "PT. Kartika Prima Cipta/kpc")

for (i in seq_along(gar_list)){
  r <- gar_list[i]
  shp_path <- paste0(gar_data_dir, r, "hcv.shp")
  fc <- st_read(shp_path)
  fc$code = names(gar_list[i])
  conservation_areas[[i]] <- fc
}



# add data to the output file
merged_map <- conservation_areas_binded %>% 
  st_write(out_file,
           layer = "conservation_areas",
           append = FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# APP original data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cargill original data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Goodhope original data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gh_conservation <- list()

gh_data_dir <- 'remote/1_original/goodhope/'
gh_list <- list("hcsa_0029" = "AWL-KMS/Patch Process_HCS/Step_8_sd_14_HCS_Area.shp",
                "hcsa_0030" = "Ketapang/Shapefile HCS_PT AJB-BMS-SMS 14 Aug 2018/Shpefile HCS_14 Aug 2018/AJB_BMS_SMS_Patch_Class_v6_140718.shp")

for (i in seq_along(gh_list)){
  r <- gh_list[i]
  shp_path <- paste0(gh_data_dir, r)
  fc <- st_read(shp_path)
  fc <- st_transform(fc, "EPSG:4326")
  fc$code = names(gh_list[i])
  gh_boundaries[[i]] <- fc
}

# turns the list into dataframes
gh_df <- map_dfr(gh_boundaries, rbind)

# reduce to minimum attributes
gh_df <- gh_df %>% 
  select(code, geometry)

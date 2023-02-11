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
# Load supplementary data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
boundaries_df <- st_read("remote/4_merging/r_merge/boundaries.shp")

strata_key <- read_csv("remote/1_original/company_strata_key.csv") %>% 
  select(strata_code_data, strata_code_eng, description, code_simpl)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define helper functions --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_data <- function(shp_path, code){
  fc <- st_read(shp_path)
  fc$code = code
  fc <- st_transform(fc, "EPSG:4326")
  return(fc)
}

load_lyr <- function(shp_path, lyr_name, code){
  fc <- st_read(shp_path, layer = lyr_name)
  fc$code = code
  fc <- st_transform(fc, "EPSG:4326")
  return(fc)
}

join_key = function(fc, key){
  fc <- fc %>% 
    rename("strata_code_data" = !!key) %>% 
    left_join(strata_key, by = "strata_code_data") %>% 
    rename(hcs = code_simpl)
  
  if (any(is.na(fc$hcs))){ 
    stop("Land use label missing from strata key")
  }

  return(fc)
}

'%ni%' <- function(x,y)!('%in%'(x,y))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CEL undergrad digitizing -----------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cel_data_dir <- 'remote/3_digitization/2022_digitization/'
cel_list <- c('hcsa_0005',
                 'hcsa_0006',
                 'hcsa_0008', # Note - some ambiguity between HCS/HCV areas in maps.
                 'hcsa_0010', # Note - uses HCSA give and take. We digitized pre give-and-take HCS map
                 'hcsa_0011',
                 'hcsa_0012',
                 'hcsa_0013',
                 'hcsa_0014',
                 'hcsa_0015',
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


cel_hcs <- list()

for (i in 1:length(report_list)){
  r <- cel_list[i]
  gdb <- paste0(cel_data_dir, r, "/", 'hcsa_digitization.gdb')
  fc <- load_lyr(gdb, lyr_name = 'conservation_areas', code = r)

  if (any(is.na(fc$conservation_type))){ 
    stop(paste0("HCS/HCV label missing in concession ", r))
  }
  
  cel_hcs[[i]] <- fc
}

# convert list to dataframe
cel_df <- cel_hcs %>%
  map_dfr(rbind) 

cel_df <- cel_df %>%
  # separate HCS and HCV columns based on conservation_type
  mutate(hcs = case_when(
    conservation_type == "HCV only" ~ 0,
    conservation_type == "HCS only" ~ 1,
    conservation_type == "HCV AND HCS" ~ 1,
    conservation_type == "HCV OR HCS" ~ 2 # NOTE: Category 2 is an uncertain class. Flags that an area is labeled as HCS/HCV but without specificity
  )) %>% 
  mutate(hcv = case_when(
    conservation_type == "HCV only" ~ 1,
    conservation_type == "HCS only" ~ 0,
    conservation_type == "HCV AND HCS" ~ 1,
    conservation_type == "HCV OR HCS" ~ 2 # NOTE: Category 2 is an uncertain class. Flags that an area is labeled as HCS/HCV but without specificity
    )) %>% 
  # drop conservation_type_column
  select(-conservation_type)

# Drop 4 empty features in hcsa_0034
cel_df <- cel_df %>% 
  filter(!cel_df$Shape_Length == 0)

# Identify concessions where we didn't map HCV
have_hcv <- cel_df %>% 
  st_drop_geometry() %>% 
  filter(hcv==1) %>% 
  pull(code) %>% 
  unique()

cel_df <- cel_df %>% 
  mutate(hcv = ifelse(code %ni% have_hcv, -1, hcv), # For some concessions, we didn't digitize HCV. Want to explicitly include this in data.
         hcs = ifelse((code == "hcsa_0008" & hcv==1), 2, hcs),
         hcv = ifelse((code == "hcsa_0008" & hcs==1), 2, hcv))# For hcsa_0008, digitized map overlays HCV on top of HCS. As a result, somewhat hard to disambiguate.

cel_df <- cel_df %>% 
  rename(geometry = Shape) %>%
  select(code, hcs, hcv, geometry)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Michael digitizing -----------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## NOTE: Michael only digitized HCS areas
michael_hcs <- list()

michael_data_dir <- 'remote/3_digitization/archive/digitization/'
michael_list <- c("hcsa_0003" = 'hcsa0003_PT_Nabire_Baru_PT_Sariwana_Adi_Perkasa/hcsa0003_hcs_wgs84_all.shp',
                  "hcsa_0004" = 'hcsa0004_PT_Kalimantan_Prima_Argo_Mandiri/hcsa0004_hcs_wgs84.shp',
                  "hcsa_0017" = 'hcsa0017_PT_Varia_Mitra_Andalan/hcsa0017_hcs_wgs84.shp')

for (i in seq_along(michael_list)){
  r <- michael_list[i]
  shp_path <- paste0(michael_data_dir, r)
  code = names(michael_list[i])
  fc <- load_data(shp_path, code)
  
  fc <- fc %>% 
    mutate(hcs = (area_type == "HCS") %>% as.integer(),
           hcv = -1) %>% 
    filter(hcs == 1)
  
  michael_hcs[[i]] <- fc
}

michael_list_gdb <- c("hcsa_0018" = 'hcsa0018_PT_Hungarindo_Persada/Arc_hcsa0018_PT_Hungarindo_Persada/hcsa0018.gdb', 
                   "hcsa_0019" = 'hcsa0019_PT_Gemilang_Makmur_Subur/Arc_hcsa0019_PT_Gemilang_Makmur_Subur/hcsa_PT_Germilang_Makm.gdb',
                   "hcsa_0026" = 'hcsa0026_PT_Tunas_Sawaerma/Arc_hcsa0026_PT_Tunas_Sawaerma/hcsa0026_PT_Tunas_Sawaerma.gdb')

for (i in seq_along(michael_list_gdb)){
  r <- michael_list_gdb[i]
  code = names(michael_list_gdb[i])
  gdb <- paste0(michael_data_dir, r)
  lyr_name <- paste0(code %>% str_remove("_"), "_hcs_wgs84")
  fc <- load_lyr(gdb, lyr_name = lyr_name, code = code)
  
  
  
  fc <- fc %>% 
    mutate(hcs = (area_type == "HCS") %>% as.integer(),
           hcv = -1) %>% 
    filter(hcs == 1) %>% 
    rename(geometry = Shape)
  
  michael_hcs[[length(michael_list) + i]] <- fc
}


# turns the list into dataframes
michael_df <- map_dfr(michael_hcs, rbind)

# reduce to minimum attributes
michael_df <- michael_df %>% 
  select(code, geometry, hcs, hcv)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# GAR original data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## NOTE: GAR data doesn't allow the same location to be both HCV and HCS.
## Lots of HCS areas are marked as HCV and, as a result, aren't included 
## in the HCS layers. Probably need to use combined HCV / HCS
gar_hcs <- list()

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
  code <- names(gar_list[i])
  r <- gar_list[i]
  
  shp_path <- paste0(gar_data_dir, r, "hcv.shp")
  hcv_fc <- load_data(shp_path, code)
  hcv_fc <- hcv_fc %>% 
    mutate(hcv = 1, 
           hcs = 2) %>% 
    select(code, geometry, hcv, hcs)
    
  shp_path <- paste0(gar_data_dir, r, "hcs.shp")
  hcs_fc <- load_data(shp_path, code)
  hcs_fc <- hcs_fc %>% 
    mutate(hcv = 2,
           hcs = 1) %>% 
    select(code, geometry, hcv, hcs)
    
  fc <- rbind(hcv_fc, hcs_fc)

  gar_hcs[[i]] <- fc
}

# turns the list into dataframes
gar_df <- map_dfr(gar_hcs, rbind)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# APP original data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## NOTE: don't have any HCV data for APP
app_hcs <- list()
app_data_dir <- 'remote/1_original/app/stratified_shapefiles/'
app_list <- list("hcsa_0001" = "Muba_BPP2_Stratification.shp",
                 "hcsa_0035" = "Jambi_WKS_Stratification.shp",
                 "hcsa_0036" = "Kalbar_DTK_Stratification.shp",
                 "hcsa_0037" = "Kaltim_KHL_Stratification.shp",
                 "hcsa_0038" = "OKI_BMH_Stratification.shp",
                 "hcsa_0039" = "Riau_MSK_SK_Stratification.shp")

for (i in seq_along(app_list)){
  r <- app_list[i]
  shp_path <- paste0(app_data_dir, r)
  code = names(app_list[i])
  fc <- load_data(shp_path, code)
  fc <- join_key(fc, "AMG_LC")
  fc <- fc %>% 
    mutate(hcv = -1) %>% 
    select(code, hcs, hcv, geometry) %>% 
    filter(hcs == 1)
  app_hcs[[i]] <- fc
}

# turns the list into dataframes
app_df <- map_dfr(app_hcs, rbind)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cargill original data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## NOTE: don't have any HCV data for Cargill
cargill_boundaries = list()
code = "hcsa_0085"
shp_path = 'remote/1_original/cargill/PT_STAL/Spatial_Data/Final _Strata.shp'
fc <- load_data(shp_path, code)
fc <- join_key(fc, "Strata")

fc <- fc %>% 
  mutate(hcv = -1) %>% 
  select(code, hcs, hcv, geometry) %>% 
  filter(hcs == 1)
cargill_boundaries[[1]] <- fc

# turns the list into dataframes
cargill_df <- map_dfr(cargill_boundaries, rbind)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Goodhope original data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## NOTE: don't have any HCV data for Goodhope. All polygons are HCS

gh_hcs <- list()

gh_data_dir <- 'remote/1_original/goodhope/'
gh_list <- list("hcsa_0029" = "AWL-KMS/Patch Process_HCS/Step_8_sd_14_HCS_Area.shp",
                "hcsa_0030" = "Ketapang/Shapefile HCS_PT AJB-BMS-SMS 14 Aug 2018/Shpefile HCS_14 Aug 2018/AJB_BMS_SMS_Patch_Class_v6_140718.shp")

for (i in seq_along(gh_list)){
  r <- gh_list[i]
  shp_path <- paste0(gh_data_dir, r)
  code = names(gh_list[i])
  fc <- load_data(shp_path, code)
  fc <- fc %>% 
    mutate(hcs = 1,
           hcv = -1) %>% 
    select(code, hcs, hcv, geometry)

  gh_hcs[[i]] <- fc
}

# turns the list into dataframes
gh_df <- map_dfr(gh_hcs, rbind)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Combine and clean data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hcs_df <- rbind(cel_df, michael_df, gar_df, app_df, cargill_df, gh_df)


# Convert multisurfaces (curved) into multipolygons (necessary for dissolve in next step - should confirm this isn't messing up the boundaries)
hcs_df <- hcs_df %>% 
  st_cast("MULTIPOLYGON")

hcs_df <- hcs_df %>% 
  group_by(code, hcv, hcs) %>% 
  summarize()

# # Dissolve into single HCS and HCV areas within each concession
# cel_df <- cel_df %>% 
#   group_by(code, HCS, HCV) %>% 
#   summarise()


# # Create separate hcv and hcs datasets
# cel_hcv <- cel_df %>% 
#   filter(HCV == TRUE) %>% 
#   select(code, HCV, Shape)
# cel_hcs <- cel_df %>% 
#   filter(HCS == TRUE) %>% 
#   select(code, HCS, Shape)


# ## TODO: Clip by concession boundaries as a final check? 
# # ## Clip based on concession boundaries
# michael_codes <- michael_df$code %>% unique()
# for i in length(michael_codes]){
#   code <- michael_codes[i]
#   print(code)
#   bounds <- boundaries_df %>% 
#     filter(code == code)
#   # %>% 
#   #   st_transform("EPSG:23835")
#   hcs_subset <- hcs_df %>% 
#     filter(code == code)
#   # %>% 
#   #   st_transform("EPSG:23835")
#   hcs_subset <- hcs_subset %>% 
#     st_intersection(bounds)
#   hcs_subset_list[[i]] <- hcs_subset 
# }
# 
# bounds <- boundaries_df %>%
#   filter(code %in% cel_list)
# 
# test <- bound_df  %>%  st_intersection(cel_bounds)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Export data --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bound_df %>% 
  write_sf("remote/4_merging/r_merge/conservation.shp")

## Note - after running, I run a clip in arcGIS to clip conservation areas to concession boundaries.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Add metadata --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Package imports --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(sf)

# # Check if file can open
# hcsa_0083_gdb <- "remote/3_digitization/2022_digitization/hcsa_0083/hcsa_digitization.gdb"
# hcsa_0083_fc <- st_read(hcsa_0083_gdb, layer = 'conservation_areas') # Since geodatabase contains multiple layers, need to specify which one you want to open

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CEL undergrad digitized concession_boundaries--------------------------
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

cel_boundaries <- list()
for (i in 1:length(cel_list)){
  r <- cel_list[i]
  gdb <- paste0(cel_data_dir, r, "/", 'hcsa_digitization.gdb')
  fc <- st_read(gdb, layer = 'concession_boundaries')
  fc <- st_transform(fc, "EPSG:4326")
  fc$code = r
  fc <- fc %>% group_by(code) %>% summarize()
  fc <- fc %>% 
    st_cast(to = "GEOMETRY")
  cel_boundaries[[i]] <- fc
}

# turns the list into dataframes
cel_df <- map_dfr(cel_boundaries, rbind)

# reduce to minimum attributes
cel_df <- cel_df %>% 
  rename(geometry = Shape) %>%
  select(code, geometry)

# merged_map <- concession_boundaries %>% 
#   st_write(out_file,
#            layer = "concession_boundaries",
#            append = FALSE)
# 
# # Open merged_hcs.gpkg to check the dfs
# #merged_hcs <- st_read(out_file, layer = "concession_boundaries")
# merged_hcs <- st_read(out_file, layer = "conservation_areas")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Michael's digitized concession boundaries --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
michael_boundaries <- list()

michael_data_dir <- 'remote/3_digitization/archive/digitization/'
michael_list <- c("hcsa_0003" = 'hcsa0003_PT_Nabire_Baru_PT_Sariwana_Adi_Perkasa/hcsa0003_boundary_wgs84.shp',
                  "hcsa_0004" = 'hcsa0004_PT_Kalimantan_Prima_Argo_Mandiri/hcsa0004_boundary_wgs84.shp',
                  "hcsa_0017" = 'hcsa0017_PT_Varia_Mitra_Andalan/hcsa0017_boundary_wgs84.shp', 
                  "hcsa_0018" = 'hcsa0018_PT_Hungarindo_Persada/hcsa0018_boundary_wgs84.shp', 
                  "hcsa_0019" = 'hcsa0019_PT_Gemilang_Makmur_Subur/hcsa0019_boundary_wgs84.shp',
                  "hcsa_0026" = 'hcsa0026_PT_Tunas_Sawaerma/hcsa0026_boundary_wgs84.shp')
                 
for (i in seq_along(michael_list)){
  r <- michael_list[i]
  shp_path <- paste0(michael_data_dir, r)
  fc <- st_read(shp_path)
  fc <- st_transform(fc, "EPSG:4326")
  fc$code = names(michael_list[i])
  fc <- fc %>% group_by(code) %>% summarize()
  michael_boundaries[[i]] <- fc
}

# turns the list into dataframes
michael_df <- map_dfr(michael_boundaries, rbind)

# reduce to minimum attributes
michael_df <- michael_df %>% 
  select(code, geometry)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# GAR concession boundaries --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gar_boundaries <- list()

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
  shp_path <- paste0(gar_data_dir, r, "bou.shp")
  fc <- st_read(shp_path)
  fc <- st_transform(fc, "EPSG:4326")
  fc$code = names(gar_list[i])
  fc <- fc %>% 
    select(code, geometry) %>% # reduce to minimum attriutes
    group_by(code) %>%  # union within code to get single row for each concession
    summarize()
  gar_boundaries[[i]] <- fc
}

# turns the list into dataframes
gar_df <- map_dfr(gar_boundaries, rbind)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# APP digitized concession boundaries --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Note: APP data doesn't have a concession boundary file, but can be 
## reconstructed from dissolve of HCS class maps

app_boundaries <- list()
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
  fc <- st_read(shp_path)
  fc <- st_transform(fc, "EPSG:4326")
  fc$code = names(app_list[i])
  fc <- fc %>% select(code, geometry)
  fc <- fc %>% group_by(code) %>% summarize()  
  app_boundaries[[i]] <- fc
}

# turns the list into dataframes
app_df <- map_dfr(app_boundaries, rbind)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cargill concession boundaries --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cargill_boundaries = list()
code = "hcsa_0085"
shp_path = 'remote/1_original/cargill/PT_STAL/Spatial_Data/Area_STAL.shp'
fc <- st_read(shp_path)
fc <- st_transform(fc, "EPSG:4326")
fc$code = code
fc <- fc %>% 
  select(code, geometry) %>% 
  group_by(code) %>% 
  summarize()
cargill_boundaries[[1]] <- fc

# turns the list into dataframes
cargill_df <- map_dfr(cargill_boundaries, rbind)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Goodhope concession boundaries --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Goodhope only provided shapefile of HCS area, not concession boundaries.
## Might need to go back and digitize boundaries...


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Combine concession boundaries and export --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bound_df <- rbind(cel_df, michael_df, gar_df, app_df, cargill_df)

bound_df %>% 
  write_sf("remote/4_merging/r_merge/boundaries.shp")


bound_df %>% 
  group_by(code) %>% 
  tally() %>% 
  arrange(desc(n))

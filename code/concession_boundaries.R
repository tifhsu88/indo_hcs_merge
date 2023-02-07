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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CEL undergrad digitized concession_boundaries--------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
concession_boundaries <- list()

for (i in 1:length(report_list)){
  r <- report_list[i]
  print(r)
  gdb <- paste0(root_data_dir, r, "/", 'hcsa_digitization.gdb')
  fc <- st_read(gdb, layer = 'concession_boundaries')
  fc <- st_transform(fc, "EPSG:4326")
  fc$code = r
  concession_boundaries[[i]] <- fc
}

# turns the list into dataframes
concession_df <- map_dfr(concession_boundaries, rbind)

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
  gar_boundaries[[i]] <- fc
}

# turns the list into dataframes
gar_df <- map_dfr(gar_boundaries, rbind)

# reduce to minimum attributes
gar_df <- gar_df %>% 
  select(code, geometry)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# APP digitized concession boundaries --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Other digitized concession boundaries --------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


####
# Constants and Lists
####
maxcell<-5e6
#base_tileset <- "Stadia.AlidadeSmooth"
#base_tileset <- "OpenStreetMap.BZH"
base_tileset <- "CartoDB.VoyagerNoLabels"

#bbox_4326 <- c(-84.65134,  32.03136, -46.60617,  63.30299)
bbox_4326 <- c(-85.06227,  37.35765, -43.84547,  58.27117)

toplevel_dir <- "McHenry_et_al_in_Rshiny_NWA_kelp_projections"
current_dist_dir <- "Current_distributions"
env_dist_dir <- "Environmental_layers/baseline_2010"

# palettes
#kelp_pal <- colorFactor()
  
# filenames of layers
kelp_layers <- list(
    "Saccharina latissima" = "Sl_PA_current.tif",
    "Laminaria digitata" = "Ld_PA_current.tif",
    "Agarum clathratum" = "Ac_PA_current.tif",
    "Alaria esculenta" = "Ae_PA_current.tif"
  )
  
env_layers <- list(
    `Longterm Bottom Temp Maximum` = "bt_ltmax_2010_baseline.tif",
    `Longterm Bottom Temp Minimum` = "bt_ltmin_2010_baseline.tif",
    `Kd PAR` = "kdpar_mean_2010_baseline.tif",
    Salinity = "sal_mean_2010_baseline.tif",
    Silicone = "siconc_mean_2010_baseline.tif",
    `Longterm SST Maximum` = "sst_ltmax_2010_baseline.tif",
    `Longterm SST Mininum` = "sst_ltmin_2010_baseline.tif",
    `Wave Exposure` = "we_2010_baseline.tif"
  )

## Generalized function to load different rasters based on similar 
## directory structure
load_resample_rast <- function(
    toplevel_dir = toplevel_dir,
    current_dir,
    layer_obj,
    input_name,
    input){
    print("layers:")
    print(names(layer_obj))
    print("obj:")
  print(input_name)
  print("input:")
  print(input[[input_name]])
  filename <- glue("{toplevel_dir}/{current_dir}/{layer_obj[[ input[[input_name]] ]]}") 
  print(filename)
  #debug
  print(glue("Loading {filename}"))
  print(list.files(glue("{toplevel_dir}/{current_dir}")))
  
  r<-  rast(filename)
  print("Raster Loaded")
  print(r)
  print("Sampling...")
  r <- r|>    spatSample(size = maxcell, 
                         as.raster=TRUE, 
                         warn=TRUE,
                         method = "regular")
  print("success!")
  
  r
}
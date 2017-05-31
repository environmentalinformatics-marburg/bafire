for (product in c("MOD08_D3", "MYD08_D3")) {
  
  fls <- list.files(paste0("MODIS/", product), full.names = TRUE, recursive = TRUE,
                    pattern = paste(product, "006", ".hdf$", sep = ".*"))
  
  sds <- getSwathSDS(fls, prm = "Aerosol_Optical_Depth_Land_Ocean", 
                     dsn = paste0("MODIS/", product))[[1]]
  
}

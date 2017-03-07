setwd("/home/fdetsch/data/bale/arcsidata/Outputs")

library(raster)
fls <- list.files(pattern = "srefdem.kea$")

lst <- lapply(fls, function(i) {
  
  nm <- gsub(".kea$", ".tif", i)
  
  if (file.exists(nm)) {
    brick(nm)
  } else {
    jnk <- gdalUtils::gdal_translate(i, nm, sds = TRUE, output_Raster = TRUE)
    
    sds <- list.files(pattern = paste0(gsub(".kea$", "", i), ".*.tif$"))
    rst <- stack(sds)
    projection(rst) <- "+init=epsg:32637"
    rst <- writeRaster(rst, nm)
    
    jnk <- file.remove(sds)
    return(rst)
  }
})

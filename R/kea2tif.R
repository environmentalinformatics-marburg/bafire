kea2tif <- function(path = ".", pattern = "srefdem.kea$", crs = NULL, ext = NULL,
                    overwrite = FALSE, keep_bands = FALSE, ...) {

  fls <- list.files(path, pattern, full.names = TRUE)
  
  lst <- lapply(fls, function(i) {
    
    nm <- gsub(".kea$", ".tif", i)
    
    if (!file.exists(nm) | overwrite) {
      try(file.remove(nm), silent = TRUE)
      jnk <- gdalUtils::gdal_translate(i, nm, sds = TRUE, output_Raster = TRUE, 
                                       ...)
      sds <- list.files(path, paste0(gsub(".kea$", "", basename(i)), ".*.tif$"), 
                        full.names = TRUE)
      rst <- raster::stack(sds)
      if (!is.null(crs)) raster::projection(rst) <- crs
      if (!is.null(ext)) rst <- raster::setExtent(rst, ext)
      jnk <- raster::writeRaster(rst, nm, overwrite = TRUE)
      if (!keep_bands) jnk <- file.remove(sds); rm(jnk)
    }
    
    return(raster::brick(nm))
  })
  
  return(if (length(lst) == 1) lst[[1]] else lst)
}
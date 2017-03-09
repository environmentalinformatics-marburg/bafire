kea2tif <- function(path = ".", pattern = "srefdem.kea$", keep_kea = TRUE, 
                    overwrite = FALSE, ...) {

  ## list and loop over .kea files in 'path'
  fls <- list.files(path, pattern, full.names = TRUE)
  lst <- lapply(fls, function(i) {

    # target file name    
    nm <- gsub(".kea$", ".tif", i)
    
    # if target file does not exist or 'overwrite', translate
    if (!file.exists(nm) | overwrite) {
      if (file.exists(nm)) jnk <- file.remove(nm)
      jnk <- gdalUtils::gdal_translate(i, nm, ...)
      if (!keep_kea) jnk <- file.remove(i)
    }
    
    return(raster::stack(nm))
  })
  
  ## return (list of) images
  return(if (length(lst) == 1) lst[[1]] else lst)
}
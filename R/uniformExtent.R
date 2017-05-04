uniformExtent <- function(template = "data/reference_grid.tif", f = 5000L, 
                          outProj = NULL, sp = TRUE) {
  
  ## required packages
  lib <- c("raster", "rgdal")
  jnk <- sapply(lib, function(x) library(x, character.only = TRUE))
  
  ## import reference grid
  rst_ref <- raster(template)
  
  ## add expansion factor
  ext <- extent(rst_ref)
  ymin(ext) <- ymin(ext) - f
  ymax(ext) <- ymax(ext) + f
  xmin(ext) <- xmin(ext) - f
  xmax(ext) <- xmax(ext) + f
  
  spy <- as(ext, "SpatialPolygons")
  proj4string(spy) <- projection(rst_ref)
  if (!is.null(outProj)) {
    spy <- spTransform(spy, CRS = CRS(outProj))
    ext <- extent(spy)
  }
  
  if (sp) {
    return(spy)
  } else {
    return(extent(spy))
  }
}
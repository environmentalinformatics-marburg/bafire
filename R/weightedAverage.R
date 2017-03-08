weightedAverage <- function(x, y, weights = TRUE, ...) {
  
  if (inherits(x, "character"))
    x <- raster::raster(x)
  
  crp <- raster::crop(x, y, ...)
  
  ## normal average, ie without weights
  if (!weights) {
    mean(crp[], na.rm = TRUE)
    
  ## weighted average  
  } else {
    spy <- as(raster::extent(y), "SpatialPolygons")
    sp::proj4string(spy) <- sp::proj4string(y)
    
    xtr <- raster::extract(crp, spy, weights = TRUE)[[1]]
    if (all(is.na(xtr[, 1]))) {
      NA 
    } else {
      if (any(is.na(xtr[, 1])))
        xtr[which(is.na(xtr[, 1])), 2] <- NA
      
      sum(apply(xtr, 1, prod), na.rm = TRUE) / sum(xtr[, 2], na.rm = TRUE)
    }
  }
}

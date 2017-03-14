#' RapidEye Quality Control
#' 
#' @description 
#' Perform RapidEye quality control based on the accompanying unusable data mask 
#' (UDM) file.
#' 
#' @param x RapidEye band(s) as \code{Raster*} or \code{character}.
#' @param qa UDM layer as \code{Raster*} or \code{character}.
#' @param directions Directions used to identify neighboring cells passed to 
#' \code{\link{adjacent}}, defaults to \code{8L} (queen's case, see Details).
#' @param ... Additional arguments passed to \code{\link{writeRaster}}.
#' 
#' @return 
#' A quality-controlled \code{Raster*}.
#' 
#' @details 
#' As explicitly stated in the RapidEye Satellite Imagery Product Specifications 
#' (see References), "it is suggested that when using the [UDM] file to check 
#' for usable data, a buffer of at least 1 pixel should be considered" due to 
#' 11+ meters of horizontal geolocation uncertainty and low spatial resolution.
#' 
#' @author 
#' Florian Detsch
#' 
#' @references 
#' BlackBridge (2015) Satellite Imagery Product Specifications, Version 6.1 
#' (April 2015). Available online: \url{http://www.e-geos.it/images/Satellite_data/RAPIDEYE/RE_Product_Specifications_ENG.pdf}.
#' 
#' @export rapid_qc
#' @name rapid_qc
rapid_qc <- function(x, qa, directions = 8L, ...) {
  
  ## if required, import 'character' input
  if (inherits(x, "character")) x <- raster::brick(x)
  if (inherits(qa, "character")) qa <- raster::raster(qa)

  ## apply pixel buffer
  if (inherits(directions, "matrix") | directions[1] > 0) {
    val <- qa[]; ids <- which(val > 0)
    adj <- raster::adjacent(qa, ids, directions, pairs = FALSE)
    adj <- adj[-which(adj %in% ids)]
    val[adj] <- NA; qa <- raster::setValues(qa, val)
  }
  
  ## resample qa layer to target resolution
  qa <- raster::resample(qa, x, method = "ngb")
  
  # discard all pixels with a quality flag value larger than 0    
  raster::overlay(x, qa, fun = function(x, y) {
    x[y[] != 0 | is.na(y[])] <- NA
    return(x)
  }, ...)
}
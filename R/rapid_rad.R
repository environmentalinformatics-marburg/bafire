#' RapidEye At-Sensor Radiances
#' 
#' @description 
#' Create RapidEye at-sensor radiances from raw digital numbers (DN).
#' 
#' @param x RapidEye band(s) with raw DN values as \code{Raster*} or 
#' \code{character}. The number and order of bands must match with the scale 
#' factors in 'meta'.
#' @param meta Associated metadata as \code{character} or \code{xml*} (see 
#' \code{\link{rapid_sf}}).
#' @param digits Number of decimal places passed to \code{\link{round}}. Values  
#' other than \code{NULL} (default) enable rounding which likely results in 
#' reduced memory usage.
#' @param ... Additional arguments passed to \code{\link{writeRaster}}.
#' 
#' @return 
#' A \code{Raster*} with at-sensor radiances in watts per steradian per square 
#' meter.
#' 
#' @author 
#' Florian Detsch
#' 
#' @seealso \code{\link{rapid_sf}}
#' 
#' @export rapid_rad
#' @name rapid_rad
rapid_rad <- function(x, meta, digits = NULL, ...) {
  
  ## if required, import 'character' input
  if (inherits(x, "character")) x <- raster::brick(x)
  if (inherits(meta, "character")) meta <- xml2::read_xml(meta)

  ## extract scale factors from metadata  
  scf <- rapidr::rapid_sf(meta)
  
  ## calculate at-sensor radiances from raw dn and layer-specific scale factors
  raster::calc(x, fun = function(x) {
    val <- x * scf
    if (!is.null(digits)) val <- round(val, digits)
    return(val)
  }, ...)
}
#' RapidEye Top-of-Atmosphere Reflectances
#' 
#' @description 
#' Create RapidEye top-of-atmosphere (TOA) reflectances from raw digital numbers 
#' (DN) or at-sensor radiances.
#' 
#' @param x RapidEye band(s) with either raw DN values or at-sensor radiances 
#' (typially derived from \code{\link{rapid_rad}}) as \code{Raster*} or 
#' \code{character}. The number and order of bands must match with the scale 
#' factors in 'meta'.
#' @param meta Associated metadata as \code{character} or \code{xml*} (see 
#' \code{\link{rapid_sf}}).
#' @param dn A \code{logical} indicating whether 'x' includes raw DN (default) 
#' or at-sensor radiances.
#' @param digits Number of decimal places passed to \code{\link{round}}. Values  
#' other than \code{NULL} (default) enable rounding which likely results in 
#' reduced memory usage.
#' @param ... Additional arguments passed to \code{\link{writeRaster}}.
#' 
#' @return 
#' A \code{Raster*} with TOA reflectances.
#' 
#' @author 
#' Florian Detsch
#' 
#' @seealso \code{\link{rapid_sf}}, \code{\link{rapid_rad}}.
#' 
#' @export rapid_ref
#' @name rapid_ref
rapid_ref <- function(x, meta, digits = NULL, dn = TRUE, ...) {
  
  ## if required, import 'character' input
  if (inherits(x, "character")) x <- raster::brick(x)
  if (inherits(meta, "character")) meta <- xml2::read_xml(meta)
  
  ## if required, extract scale factors from metadata to create at-sensor 
  ## radiances from raw digital numbers later on
  if (dn) scf <- rapid_sf(meta)
    
  ## extract relevant information from metadata  
  dst <- satellite::calcEarthSunDist(rapid_date(meta))
  eai <- c(1997.8, 1863.5, 1560.4, 1395.0, 1124.4)
  
  dt <- rapid_date(meta)
  tm <- rapid_hour(meta)
  dtm <- as.POSIXct(paste(dt, "00:00:00")) + 3600 * tm
  
  lat <- rapid_latlon(meta)[1]
  dcl <- satellite::declination(rapid_date(meta), formula = "Spencer")
  sha <- satellite::solarHourAngle(dtm)
  
  ## compute solar zenith angle (in deg, should be moved to separate function)
  szn <- acos(sin(lat * pi / 180) * sin(dcl) + 
                cos(lat * pi / 180) * cos(dcl) * cos(sha))
  
  ## calculate toa reflectances from at-sensor radiances
  raster::calc(x, fun = function(y) {
    if (dn) y <- y * scf
    val <- y * pi * dst^2 / (eai * cos(szn))
    if (!is.null(digits)) val <- round(val, digits)
    return(val)
  }, ...)
}
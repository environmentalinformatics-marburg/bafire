#' RapidEye Top-of-Atmosphere Radiances
#' 
#' @description 
#' Create RapidEye top-of-Atmosphere Radiances (ToAR) from raw digital numbers 
#' (DN).
#' 
#' @param x RapidEye band(s) as \code{Raster*} or \code{character}. The number 
#' and order of bands must match with the scale factors in 'meta'.
#' @param meta Associated metadata as \code{character} or \code{xml*} (see 
#' \code{\link{rapid_sf}}).
#' @param digits Number of decimal places passed to \code{\link{round}}. Values  
#' other than \code{NULL} enable rounding which might lead to reduced memory 
#' usage through an adapting \code{Raster*} \code{\link{dataType}}.
#' @param ... Additional arguments passed to \code{\link{writeRaster}}.
#' 
#' @return 
#' A ToAR \code{Raster*}.
#' 
#' @author 
#' Florian Detsch
#' 
#' @seealso \code{\link{rapid_sf}}
#' 
#' @export rapid_toar
#' @name rapid_toar
rapid_toar <- function(x, meta, digits = NULL, ...) {
  
  ## if required, import 'character' input
  if (inherits(x, "character")) x <- raster::brick(x)
  if (inherits(meta, "character")) meta <- xml2::read_xml(meta)

  ## extract scale factors from metadata  
  scf <- rapid_sf(meta)
  
  ## calculate toar from raw dn and layer-specific scale factors
  raster::calc(x, fun = function(x) {
    val <- x * scf
    if (!is.null(digits)) val <- round(val, digits)
    return(val)
  }, ...)
}
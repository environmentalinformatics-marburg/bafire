#' Create RapidEye Shadow Mask
#' 
#' @param x RapidEye band(s) as \code{Raster*} or \code{character}.
#' @param weights A \code{matrix} of weights passed to 
#' \code{\link{VeloxRaster_meanFocal}}.
#' @param limit Threshold value to capture spatially isolated shadows, see 
#' Details. Only applicable if 'weights' is specified.
#' @param b4,b5 Indices of red-edge and near-infrared RapidEye bands in 'x'.
#' @param ... Additional arguments passed to \code{\link{writeRaster}}.
#' 
#' @details If 'weights' is specified, 'limit' denotes the relative amount of 
#' adjacent shadow pixels required to retain a focal shadow pixel (ie do not 
#' convert it to non-shadow).
#' 
#' @export rapid_shadows
#' @name rapid_shadows
rapid_shadows <- function(x, weights, limit = 0.5, b4 = 4, b5 = 5) {
  
  lib <- c("raster", "ggplot2", "Orcs", "lattice", "velox")
  Orcs::loadPkgs(lib)
  
  if (inherits(x, "character")) x <- brick(x)
  
  lst <- lapply(unstack(x)[c(b4, b5)], function(i) {
    d <- ggplot() + geom_density(aes(x = i[]))
    g <- print(d)
    dev.off()
    
    dat <- g$data[[1]]
    pts <- findpeaks(dat$y, bw = 5L)
    pts <- t(list2df(pts[3:4]))
    
    p <- xyplot(y ~ seq(y), data = dat, panel = function(x, y, ...) {
      panel.xyplot(x, y, type = "l", col = "gray75", ...)
      panel.xyplot(pts[, 1], pts[, 2], cex = 1.2, col = "black")
    })
    
    out <- list(dat$x[pts[1, 1]], p)
    return(out)
  })
  
  ## create shadow template (0: no shadow, 1: shadow)
  thr <- sapply(lst, "[[", 1)
  ids <- (rst[[4]][] < thr[1]) | (rst[[5]][] < thr[2])
  tmp <- setValues(rst[[1]], rep(0, ncell(rst[[1]])))
  tmp[ids] <- 1
  
  if (!missing(weights)) {
    vlx <- velox(tmp)
    vlx$meanFocal(adj)
    fcl <- vlx$as.RasterLayer()
    
    msk <- overlay(tmp, fcl, fun = function(x, y) {
      x[x[] == 1 & y[] <= limit] <- 0
      return(x)
    }); rm(tmp)
  }
  
  overlay(rst, if (!missing(weights)) msk else tmp, fun = function(x, y) {
      x[y[] == 1] <- NA
      return(x)
    }, ...)
}

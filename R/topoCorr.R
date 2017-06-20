### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## working directory
setwd("/media/fdetsch/XChange/bale/DigitalGlobeFoundation/ImageryGrant")

## packages
lib <- c("rgdal", "satellite")
Orcs::loadPkgs(lib)

## parallelization
library(parallel)
cl <- makePSOCKcluster(detectCores() - 1)
clusterExport(cl, "lib"); jnk <- clusterEvalQ(cl, Orcs::loadPkgs(lib))

## temporary raster folder
rasterOptions(tmpdir = "/media/fdetsch/XChange/tmp")
tmpDir()


### processing -----

## union extent of worldview-1 images required for aster data download
drs <- dir("arcsidata/Raw", full.names = TRUE)
drs <- drs[grep("PAN$", drs)]

# pys <- do.call("bind", lapply(drs, function(i) {
#   fls <- list.files(i, pattern = ".TIF$", full.names = TRUE)
#   do.call("bind", lapply(fls, function(j) {
#     rst <- raster(j)
#     spy <- as(extent(rst), "SpatialPolygons")
#     proj4string(spy) <- projection(rst)
#     return(spy)
#   }))
# }))
# 
# spy <- as(extent(pys), "SpatialPolygons"); proj4string(spy) <- proj4string(pys)
# spy <- SpatialPolygonsDataFrame(spy, data = data.frame(ID = 1))
# writeOGR(spy, "shp", "extent", "ESRI Shapefile")
spy <- readOGR("shp", "extent")

## slope and aspect from aster dem
# dms <- list.files("dem/ASTER", pattern = "dem.tif$", recursive = TRUE,
#                   full.names = TRUE)
# dms <- do.call("merge", lapply(dms, raster))
# dms <- crop(dms, spTransform(spy, CRS(projection(dms))), snap = "out")
# dms <- trim(projectRaster(dms, crs = proj4string(spy)), datatype = "INT2U",
#             filename = "dem/ASTER/ASTGTM2_N0XE039", format = "GTiff")
dms <- raster("dem/ASTER/ASTGTM2_N0XE039.tif")
trr <- lapply(c("slope", "aspect"), function(opt) terrain(dms, opt))

## hillshade and topographic correction of worldview-1 images
getWVSunEle <- function(x) {
  require(magrittr)
  x %>%
    xml2::xml_find_all("///MEANSUNEL") %>%
    xml2::xml_text() %>%
    as.numeric()
}

getWVSunAzi <- function(x) {
  require(magrittr)
  x %>%
    xml2::xml_find_all("///MEANSUNAZ") %>%
    xml2::xml_text() %>%
    as.numeric()
}

lapply(drs, function(i) {
  fls <- list.files(i, pattern = ".TIF$", full.names = TRUE)
  mtd <- xml2::read_xml(list.files(i, pattern = ".XML$", full.names = TRUE))
  sel <- getWVSunEle(mtd); szn <- getWVSunAzi(mtd)
  
  do.call("merge", lapply(fls, function(j) {
    rst <- raster(j)
    tmp <- lapply(trr, function(k) crop(k, rst, snap = "out"))
    hsd <- hillShade(tmp[[1]], tmp[[2]], sel, szn); rm(tmp)
    
    nms <- paste0("dem/ASTER/", gsub("P001.TIF", "P001_HLSH.tif", basename(j)))
    if (file.exists(nms)) {
      rsm <- raster(nms)
    } else {
      tmp1 <- resample(hsd, rst) 
      rsm <- writeRaster(tmp1, nms); rm(tmp1)
    }
    
    calcTopoCorr(rst, rsm)
  }))
})
# hsh <- hillShade(trr[[1]], trr[[2]])



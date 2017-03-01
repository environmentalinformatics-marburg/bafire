setwd("/media/permanent/data/bale")

library(raster)
library(rgdal)

dem <- raster("dem/dem_srtm_01.tif")
tls <- readOGR("rapideye/download", "download")
ext <- as(extent(tls), "SpatialPolygons")
proj4string(ext) <- proj4string(tls)
ext <- suppressWarnings(rgeos::gBuffer(ext, width = .01))
dem <- crop(dem, ext, snap = "out")

ref <- brick(tfs[2])
dem_utm <- as.integer(projectRaster(dem, crs = proj4string(ref)))
dataType(dem_utm) <- dataType(dem)
dem_utm <- writeRaster(dem_utm, "dem/dem_srtm_01_utm.tif", format = "GTiff")

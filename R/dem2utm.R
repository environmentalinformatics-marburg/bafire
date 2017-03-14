Orcs::setwdOS(path_win = "E:/", path_ext = "data/bale")

library(raster)
library(rgdal)

dem <- raster("dem/dem_srtm_01.tif")
tls <- readOGR("rapideye/download", "download")
ext <- as(extent(tls), "SpatialPolygons")
proj4string(ext) <- proj4string(tls)
ext <- suppressWarnings(rgeos::gBuffer(ext, width = .01))
dem <- crop(dem, ext, snap = "out")

tfs <- "rapideye/3742218_2016-02-11_RE2_3A_627058/3742218_2016-02-11_RE2_3A_627058.tif"
ref <- brick(tfs)
dem_utm <- projectRaster(dem, crs = proj4string(ref))
dem_utm <- writeRaster(dem_utm, "dem/dem_srtm_01_utm.tif", format = "GTiff", 
                       overwrite = TRUE)

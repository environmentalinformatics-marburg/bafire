setwd("/media/fdetsch/XChange/bale")

library(MODIS)
MODISoptions("/media/fdetsch/XChange/MODIS_ARC", 
             "/media/fdetsch/XChange/MODIS_ARC/PROCESSED", 
             outProj = "+init=epsg:32637", MODISserverOrder = c("LPDAAC", "LAADS"))

# dem <- raster("dem/dem_srtm_01.tif")
# dem <- as(extent(dem), "SpatialPolygons"); proj4string(dem) <- "+init=epsg:4326"

stn <- shapefile("/media/permanent/data/bale/ema/ema_stations.shp")
ext <- as(extent(stn), "SpatialPolygons"); proj4string(ext) <- proj4string(stn)
bff <- rgeos::gBuffer(ext, width = 1e3)

tfs <- runGdal("M*D13A2", collection = getCollection("MOD13A2", forceCheck = TRUE), 
               extent = bff, SDSstring = "101000000011", job = "balendvi", 
               quiet = TRUE)


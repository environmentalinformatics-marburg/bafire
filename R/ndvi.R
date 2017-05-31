setwd("/media/fdetsch/data/bale")

library(MODIS)
MODISoptions("/media/fdetsch/data/bale/MODIS_ARC", 
             "/media/fdetsch/data/bale/MODIS_ARC/PROCESSED", 
             outProj = "+init=epsg:32637", MODISserverOrder = c("LPDAAC", "LAADS"))

dem <- raster("dem/dem_srtm_01_utm_3742218.tif")
dem <- as(extent(dem), "SpatialPolygons"); proj4string(dem) <- "+init=epsg:32637"

tfs <- runGdal("M*D13Q1", collection = getCollection("MOD13Q1", forceCheck = TRUE), 
               extent = dem, SDSstring = "101000000011", job = "balendvi", 
               begin = "2013001", end = "2016366")

library(ESD)
mcd <- preprocessMODIS(tfs, dsn = "ndvi", cores = 3L, lambda = 500L)

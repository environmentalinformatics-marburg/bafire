### environment -----

## packages
library(raster)
library(mapview)

## functions
source("../../../repo/bafire/R/rapid_qc.R")


### preprocessing -----

## import rough national park boundaries
bmn <- shapefile("protectedplanet/balemountains.shp")

## import dem, discarding all pixels below 4000 m.a.s.l.
dem <- raster("../../../data/bale/dem/dem_srtm_01.tif")
dem <- crop(dem, bmn)
dem[dem[] < 4000] <- NA # better: ndvi threshold or coupling of both

## select largest contiguous high-elevation area for testing
spy <- rasterToPolygons(dem)
spu <- rgeos::gUnaryUnion(spy); rm(spy)
spd <- disaggregate(spu); rm(spu)
bnd <- spd[which.max(sapply(spd@polygons, function(i) attr(i, "area"))), ]
bnd <- spTransform(bnd, CRS = CRS("+init=epsg:32637"))


### classification using rapideye -----

drs <- "../../../data/bale/rapideye/Outputs"
rpd <- brick(list.files(drs, pattern = "3742218.*srefdem.tif$", full.names = TRUE))
  
## custom cloud mask
cld <- raster(list.files(drs, pattern = "3742218.*cloud.tif$", full.names = TRUE))
rpd_clf <- overlay(rpd, cld, fun = function(x, y) {
  x[y[] == 4] <- NA
  return(x)
}); rm(cld)

## official unusable data mask (udm)
udm <- list.files(drs, pattern = "3742218.*udm.tif$", full.names = TRUE)
rpd_udf <- rapid_qc(rpd_clf, udm); rm(rpd_clf)

rpd_crp <- crop(rpd_udf, bnd, snap = "out"); rm(rpd_udf)
rpd_msk <- mask(rpd_crp, bnd)


### classification using google maps -----

library(Rsenal)

# ref <- as(extent(rpd), "SpatialPolygons")
# proj4string(ref) <- projection(rpd)
# ctr <- rgeos::gCentroid(ref)

loc <- data.frame(y = 6.8265, x = 39.8194, Location = "Mount Tullu Dimtu")
coordinates(loc) <- ~ x + y; proj4string(loc) <- "+init=epsg:4326"
loc <- spTransform(loc, CRS("+init=epsg:32637"))

rps <- "/home/fdetsch/repo/magic/dfg_for_kilimanjaro/osm_google_kili/src/"
source(paste0(rps, "getTileCenters.R")); source(paste0(rps, "getGoogleTiles.R"))

tls <- getTileCenters(20000, 400)

dsm <- getGoogleTiles(tile.cntr = tls, location = loc, plot.res = 400, 
                      path.out = "/media/fdetsch/data/bale/aerials/dsm/C3742218", 
                      scale = 2, rgb = TRUE, type = "satellite", prefix = "bale_tile_", 
                      mosaic = "/media/fdetsch/data/bale/aerials/dsm/C3742218/bale.tif")

fls_msc <- "/media/fdetsch/data/bale/aerials/dsm/C3742218/C3742218.tif"
fls_tmp <- "/media/fdetsch/data/bale/aerials/dsm/C3742218/tmp.tif"

for (i in 2:nrow(dsm)) {
  if (i == 2) {
    msc <- merge(brick(dsm$fls[i-1]), brick(dsm$fls[i]), tolerance = 100, 
                 filename = fls_msc)
  } else {
    tmp <- merge(msc, brick(dsm$fls[i]), tolerance = 100, 
                 filename = fls_tmp, overwrite = TRUE)
    msc <- writeRaster(tmp, fls_msc, overwrite = TRUE); rm(tmp)
  }
}


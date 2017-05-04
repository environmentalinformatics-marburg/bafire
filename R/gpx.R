### waypoints -----

setwd("../../data/bale/gpx")

library(plotKML)
fls <- list.files(pattern = ".gpx$", full.names = TRUE)

library(plyr)
pts <- do.call("rbind.fill", lapply(fls, function(i) {
  gpx <- readGPX(i)
  if (is.null(gpx$waypoints) & !is.null(gpx$tracks)) {
    do.call("rbind", gpx$tracks[[1]])
  } else if (!is.null(gpx$waypoints) & is.null(gpx$tracks)) {
    gpx$waypoints
  } else if (!is.null(gpx$waypoints) & !is.null(gpx$tracks)) {
    rbind.fill(do.call("rbind", if (i == "./gps60.gpx") lapply(gpx$tracks, "[[", 1) else gpx$tracks[[1]]), 
               gpx$waypoints)
  }
}))

library(sp)
coordinates(pts) <- ~ lon + lat
proj4string(pts) <- "+init=epsg:4326"

library(mapview)
mapview(pts[!is.na(pts$name), ])


### excel -----

wb <- loadWorkbook("Fire_BMNP_GPS_Location_31-01-2017.xlsx")
ws <- readWorksheet(wb, sheet = 1L, startRow = 3L, header = TRUE)

ids <- sapply(1:ncol(ws), function(i) all(is.na(ws[, i])))
ws <- ws[, !ids]

ids <- sapply(1:nrow(ws), function(i) all(is.na(ws[i, ])))
ws <- ws[!ids, ]

pts <- vector("list", 3L)
for (i in 1:ncol(ws)) {
  crd <- ws[, i]
  if (any(is.na(crd))) 
    crd <- as.character(na.omit(crd))
  
  crd <- gsub(" ", "", crd)
  crd <- gsub("37N", "", crd)
  crd <- gsub("UTM", "", crd)
  crd <- as.integer(crd)
  
  x <- crd[seq(1, length(crd), 2)]
  y <- crd[seq(2, length(crd), 2)]
  pts[[i]] <- SpatialPoints(data.frame(x, y), proj4string = CRS("+init=epsg:32637"))
}

y16 <- shapefile("../fire/viirs/fire_nrt_V1_10850.shp")
bmn <- shapefile("../../../work/projects/bale/protectedplanet/balemountains.shp")
mapview(y16[bmn, ]) + bmn

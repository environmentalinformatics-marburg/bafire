### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## working directory
setwd("/media/XChange/bale/DigitalGlobeFoundation/ImageryGrant")

## packages
# devtools::install_github("environmentalinformatics-marburg/reset")
# devtools::install_github("environmentalinformatics-marburg/Rsenal")
# devtools::install_github("italocegatta/rapidr")
# devtools::install_github("fdetsch/Orcs")
lib <- c("doParallel", "rapidr", "Orcs", "reset", "Rsenal", "gimms")
Orcs::loadPkgs(lib)

## functions
bfr <- "~/repo/bafire/"
jnk <- sapply(c("getInfo", "rapid_qc", "weightedAverage", "kea2tif", 
                "rapid_shadows", "findpeaks"), function(i) {
  source(paste0(bfr, "R/", i, ".R"))
})

## parallelization
cl <- makePSOCKcluster(detectCores() - 1)
jnk <- clusterEvalQ(cl, library(raster))


### atmospheric correction (see 
### http://rsgislib.org/arcsi/scripts.html#arcsi-py) -----

## valid arcsi values for aot, water content
# aod_vld <- c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 0.95)
# wct_vld <- c(0.5, 1:6, 8:9)

## directions for adjacent pixels
adj <- matrix(rep(1, 25), ncol = 5); adj[3, 3] <- 0

## inputs (ie metadata files)
raw <- list.dirs("arcsidata/Raw", recursive = FALSE)
inp <- paste0("arcsidata/Inputs/", basename(raw))
mtd <- sapply(raw, function(i) {
  list.files(i, pattern = ".XML$", full.names = TRUE)
})
xml <- lapply(mtd, xml2::read_xml)
lcl <- Sys.getlocale(category = "LC_TIME")
gimms:::setLocale()
dts <- as.Date(tolower(substr(basename(mtd), 1, 7)), format = "%y%b%d")
tms <- as.POSIXct(paste(dts, substr(basename(mtd), 8, 13)), format = "%Y-%m-%d %H%M%S")
gimms:::setLocale(reset = TRUE, locale = lcl)

scl <- sapply(xml, function(i) {
  i %>%
    xml2::xml_find_all("///ABSCALFACTOR") %>%
    xml2::xml_text() %>%
    as.double()
})

## digital elevation models per rapideye tile
dem_utm <- raster("dem/dem_srtm_01_utm.tif")
cid <- sapply(strsplit(raw, "_"), "[[", 3)

dms <- lapply(raw, function(h) {
  fls <- list.files(h, pattern = ".TIF$", full.names = TRUE)
  nms <- paste0(gsub(".tif$", "_", attr(dem_utm@file, "name")), 
                gsub(".TIF$", ".tif", basename(fls)))
  
  clusterExport(cl, c("fls", "nms", "dem_utm"), envir = environment())
  parLapply(cl, 1:length(fls), function(g) {
    if (file.exists(nms[g])) {
      raster(nms[g])
    } else {
      b1 <- raster(fls[g])
      resample(dem_utm, b1, filename = nms[g])
    }
  })
}); names(dms) <- unique(cid)

## loop over scenes
for (h in 1:length(mtd)) {
  
  ## status message
  cat("Scene", raw[h], "is in, start processing.\n")

  tfs <- list.files(raw[h], ".TIF$", full.names = TRUE)
  for (v in 1:length(tfs)) {

    ## digital elevation model
    dem <- dms[[h]][[v]]

    ## aerosol optical depth (aod; which modis overpass is closer to rapideye?), 
    ## atmospheric water (wct) and ozone content (oct)
    prj <- projectExtent(dem, crs = "+init=epsg:4326")
    pry <- as(extent(prj), "SpatialPolygons")
    proj4string(pry) <- proj4string(dem)
    
    prm <- c("Aerosol_Optical_Depth", "Water_Vapor", "Total_Ozone")
    atm <- sapply(prm, function(z) {
                    
      mod_all <- list.files("modis", full.names = TRUE, pattern = paste0(z, ".tif$"), 
                            recursive = TRUE)
      isc <- sapply(mod_all, function(w) {
        rst <- raster(w) 
        spy <- as(extent(rst), "SpatialPolygons")
        proj4string(spy) <- proj4string(pry)
        rgeos::gIntersects(spy, pry)
      })
      mod_all <- mod_all[isc]
      
      mod_dts <- mod_all[grep(format(dts[[h]], "%Y%j"), mod_all)]
      
      val <- sapply(mod_dts, function(i) {
        tmp <- try(weightedAverage(i, prj, snap = "out"), silent = TRUE)
        if (inherits(tmp, "try-error")) return(NA) else return(tmp)
      })
      
      if (z == "Aerosol_Optical_Depth") {
        out <- mean(val, na.rm = TRUE)
      } else {
        dfs <- abs(difftime(tms[[h]], getSwathDateTime(mod_dts)))
        out <- NA
        for (y in val[order(dfs)]) {
          out <- y
          if (!is.na(out)) break
        }
      }
    
      ## if no value for the respective date is available, retrieve long-term mean
      if (is.na(out)) {
        out <- mean(sapply(mod_all, function(i) {
          weightedAverage(i, prj, snap = "out")
        }), na.rm = TRUE)
      }
      
      if (z == "Total_Ozone") out <- out / 1000
      return(round(out, digits = 2L))
  
    # aod <- if (is.na(aod)) .01 else aod_vld[which.min(abs(aod - aod_vld))]
    # wct <- if (is.na(wct)) .01 else wct_vld[which.min(abs(wct - wct_vld))]
  })
  
  ## apply atmospheric correction
  # input <- gsub("Raw", "Inputs", mtd[h]); jnk <- file.copy(mtd[h], input)
  cmd <- paste("arcsi.py -s wv2 -f KEA -p SREF",
               "--aeropro Continental --atmospro Tropical",
               "--atmoswater", atm[2], "--atmosozone", atm[3],
               "--aot", atm[1], "--dem", attr(dem@file, "name"),
               "-i", mtd[h], "-t arcsidata/Temp -o arcsidata/Outputs")

  system(cmd)

  # tfs <- kea2tif("arcsidata/Outputs", overwrite = FALSE, keep_kea = TRUE)
  # fno <- attr(tfs@layers[[1]]@file, "name")
  # 
  # val <- qcl[[1]][]
  # ids <- is.na(val); val[ids] <- 0; val[!ids] <- 1
  # tmp <- setValues(qcl[[1]], val)
  # tfs <- overlay(tfs, tmp, fun = function(x, y) {
  #   x[y[] == 0] <- NA
  #   return(x)
  # })
  # 
  # for (v in c(".kea$", ".xml$"))
  #   file.remove(list.files("arcsidata/Outputs", pattern = v, full.names = TRUE))
  # 
  # writeRaster(tfs, fno, overwrite = TRUE)
}

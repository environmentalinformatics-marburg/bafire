### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## working directory
setwd("/media/fdetsch/data/bale")

## packages
# devtools::install_github("environmentalinformatics-marburg/reset")
# devtools::install_github("italocegatta/rapidr")
# devtools::install_github("fdetsch/Orcs")
lib <- c("parallel", "rapidr", "Orcs", "reset")
Orcs::loadPkgs(lib)

## functions
bfr <- "~/repo/bafire/"
jnk <- sapply(c("getInfo", "rapid_qc", "weightedAverage", "kea2tif"), function(i) {
  source(paste0(bfr, "R/", i, ".R"))
})


# ### extraction -----
# 
# ## list available file containers
# ctn <- list.files("rapideye", pattern = "^getProduct.*streaming=True$",
#                   full.names = TRUE)
# 
# ## loop over containers and extract required files
# lst <- parLapply(cl, ctn, function(i) {
#   fls <- unzip(i, list = TRUE)$Name
#   ord <- unique(getOrder(fls))
#   ids <- sapply(c(paste0(c(ord, "udm"), ".tif$"), "metadata.xml"), 
#                 function(j) grep(j, fls))
#   unzip(i, files = fls[ids], exdir = dirname(i), overwrite = FALSE)
# })


### atmospheric correction (see 
### http://rsgislib.org/arcsi/scripts.html#arcsi-py) -----

## valid arcsi values for aot, water content
# aod_vld <- c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 0.95)
# wct_vld <- c(0.5, 1:6, 8:9)

## directions for adjacent pixels
mat <- matrix(rep(1, 25), ncol = 5); mat[3, 3] <- 0

## inputs (ie metadata files)
drs <- list.dirs("arcsidata/Inputs", recursive = FALSE)
mtd <- sapply(drs, function(i) {
  list.files(i, pattern = "metadata.xml$", full.names = TRUE)
})
xml <- lapply(mtd, xml2::read_xml)
dts <- lapply(xml, function(i) rapid_date(i))
tms <- lapply(seq(xml), function(i) {
  as.POSIXct(paste(dts[[i]], "00:00:00")) + rapid_hour(xml[[i]]) * 3600
})
scl <- sapply(xml, function(i) 1 / round(unique(rapid_sf(i)), 2L))

## digital elevation models per rapideye tile
dem_utm <- raster("dem/dem_srtm_01_utm.tif")
cid <- sapply(strsplit(basename(drs), "_"), "[[", 1)

dms <- lapply(unique(cid), function(h) {
  dr <- drs[grep(h, drs)][1]
  fl <- list.files(dr, pattern = paste0(getOrder(dr), ".tif"), full.names = TRUE)
  nm <- paste0(gsub(".tif$", "_", attr(dem_utm@file, "name")), h, ".tif")
  
  if (file.exists(nm)) {
    raster(nm)
  } else {
    b1 <- raster(fl)
    resample(dem_utm, b1, filename = nm)
  }
}); names(dms) <- unique(cid)

## loop over scenes
out <- lapply(1:length(mtd), function(h) {
  
  ## status message
  cat("Image", dirname(mtd[[h]]), "is in, start processing.\n")
  
  # ## quality control
  # bds <- brick(gsub("_metadata.xml$", ".tif", mtd[h]))
  # udm <- raster(gsub(".tif$", "_udm.tif", attr(bds@file, "name")))
  # 
  # nm <- gsub(".tif$", "_QC.tif", attr(bds@file, "name"))
  # if (!file.exists(nm))
  #   jnk <- rapid_qc(bds, udm, directions = mat, filename = nm)
  
  ## digital elevation model
  dem <- dms[[grep(cid[h], names(dms))]]

  ## aerosol optical depth (aod; which modis overpass is closer to rapideye?), 
  ## atmospheric water (wct) and ozone content (oct)
  prj <- projectExtent(dem, crs = "+init=epsg:4326")
  pry <- as(extent(prj), "SpatialPolygons")
  proj4string(pry) <- proj4string(dem)
  atm <- sapply(c("Aerosol_Optical_Depth", "Water_Vapor", "Total_Ozone"), 
                function(z) {

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
  cmd <- paste("arcsi.py -s rapideye -f KEA -p RAD SREF",
               "--aeropro Continental --atmospro Tropical",
               "--atmoswater", atm[2], "--atmosozone", atm[3],
               "--aot", atm[1], "--dem", attr(dem@file, "name"),
               "--scalefac", scl[h], "-i", mtd[h],
               "-t arcsidata/Temp -o arcsidata/Outputs")

  system(cmd)

  # if (h == 1) {
  #   cat("#!/bin/bash\n\n", file = "arcsibatch.sh")
  # }
  # 
  # cat(paste0(cmd, "\n\n"), file = "arcsibatch.sh", append = TRUE)
})

srf <- kea2tif("arcsidata/Outputs", crs = "+init=epsg:32637", ext = extent(dem), 
               overwrite = TRUE)

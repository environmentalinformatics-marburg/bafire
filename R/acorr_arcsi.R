### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## working directory
setwd("/media/fdetsch/data/bale")

## packages
# devtools::install_github("environmentalinformatics-marburg/reset")
# devtools::install_github("italocegatta/rapidr")
# devtools::install_github("fdetsch/Orcs")
lib <- c("parallel", "rapidr", "Orcs")
Orcs::loadPkgs(lib)

## functions
bfr <- "~/repo/bafire/"
jnk <- sapply(c("getInfo", "rapid_qc", "rapid_rad", "rapid_ref"), function(i) {
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
aod_vld <- c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 0.95)
wct_vld <- c(0.5, 1:6, 8:9)

## directions for adjacent pixels
mat <- matrix(rep(1, 25), ncol = 5); mat[3, 3] <- 0

## inputs (ie metadata files)
drs <- list.dirs("arcsidata/Inputs", recursive = FALSE)
mtd <- sapply(drs, function(i) {
  list.files(i, pattern = "metadata.xml$", full.names = TRUE)
})
dts <- lapply(mtd, function(i) rapid_date(xml2::read_xml(i)))
scl <- lapply(mtd, function(i) round(unique(rapid_sf(xml2::read_xml(i))), 2L))

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
for (h in 1:length(mtd)) {
  
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
  atm <- sapply(c("Aerosol_Optical_Depth", "Water_Vapor", "Total_Ozone"), 
                function(z) {
    
    mod <- list.files("modis", full.names = TRUE, pattern = paste0(z, ".tif$"), 
                      recursive = TRUE)
    mod <- mod[grep(format(dts[[h]], "%Y%j"), mod)]
  
    val <- mean(sapply(mod, function(i) {
      weightedAverage(i, prj, snap = "out")
    }), na.rm = TRUE)
    
    if (z == "Total_Ozone") val <- val / 1000
    return(round(val, digits = 2L))
  
    # aod <- if (is.na(aod)) .01 else aod_vld[which.min(abs(aod - aod_vld))]
    # wct <- if (is.na(wct)) .01 else wct_vld[which.min(abs(wct - wct_vld))]
  })
  
  ## apply atmospheric correction
  cmd <- paste("arcsi.py -s rapideye -f KEA -p TOA SREF", 
               "--aeropro Continental --atmospro Tropical", 
               "--atmoswater", atm[2], "--atmosozone", atm[3],
               "--aot", atm[1], "--dem", attr(dem@file, "name"), 
               "--scalefac", 1 / scl[[h]], "-i", mtd[h],
               "-t arcsidata/Temp -o arcsidata/Outputs")
  
  # if (h == 1) {
  #   cat("#!/bin/bash\n\n", file = "arcsibatch.sh")
  # }
  # 
  # cat(paste0(cmd, "\n\n"), file = "arcsibatch.sh", append = TRUE)
}

### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## set working directory
setwd("/media/memory01/data/casestudies/bale")

## required packages
# devtools::install_github("MatMatt/MODIS", ref = "develop")
lib <- c("parallel", "raster", "rgdal", "MODIS")
jnk <- sapply(lib, function(x) library(x, character.only = TRUE))

## required functions
bfr <- "../../users/fdetsch/repo/bafire/"
source(paste0(bfr, "R/getInfo.R"))

## parallelization
cl <- makePSOCKcluster(8L)
clusterExport(cl, "bfr")
jnk <- clusterEvalQ(cl, source(paste0(bfr, "R/getInfo.R")))


### extraction -----

## list available file containers
ctn <- list.files("rapideye", pattern = "^getProduct.*streaming=True$",
                  full.names = TRUE)

## loop over containers and extract required files
lst <- parLapply(cl, ctn, function(i) {
  fls <- unzip(i, list = TRUE)$Name
  ord <- unique(getOrder(fls))
  ids <- sapply(c(paste0(c(ord, "udm"), ".tif$"), "metadata.xml"), 
                function(j) grep(j, fls))
  unzip(i, files = fls[ids], exdir = dirname(i), overwrite = FALSE)
})


### -----

## import rapideye tiles
spy_tls <- readOGR("data", "tiles")
spy_tls <- spTransform(spy_tls, CRS = CRS("+init=epsg:32637"))

## import modis files
fls_mds <- list.files("data/MCD14A1.006", pattern = "FireMask.tif$", 
                      full.names = TRUE)

dts_mds <- extractDate(fls_mds, asDate = TRUE)$inputLayerDates
dts_rpd <- getAcquisitionDate(fls_qc, as_date = TRUE)
ids_lwr <- difftime(min(dts_rpd), dts_mds, units = "days") < 0
fls_mds <- fls_mds[ids_lwr]; dts_mds <- dts_mds[ids_lwr]

ids_upr <- difftime(max(dts_rpd), dts_mds, units = "days") > 0
fls_mds <- fls_mds[ids_upr]; dts_mds <- dts_mds[ids_upr]

## loop over modis scenes
n <- 1
for (i in 1:length(fls_mds)) {
  
  # import current scene and extract date
  rst_mds <- try(raster(fls_mds[i]), silent = TRUE)
  if (class(rst_mds) == "try-error") next
  rst_mds <- projectRaster(rst_mds, crs = "+init=epsg:32637", method = "ngb")

  # if no fires were detected, skip current scene  
  val_mds <- rst_mds[]
  if (all(is.na(val_mds))) next 
  
  # else loop over fire pixels
  cls <- which(!is.na(val_mds))
  for (j in cls) {
    # in which rapideye tile lies current modis pixel
    rst <- rst_mds
    rst[][-j] <- NA
    spy <- rasterToPolygons(rst)
    tls <- spy_tls[spy, ]$TILE_ID
    
    # if target rapideye tile is not available, skip current scene
    if (length(tls) == 0) {
      next
      
    # which is the next rapideye scene for current modis scene
    } else if (length(tls) == 1) {
      fls <- fls_qc[grep(tls, fls_qc)]
    } else {
      ids <- unlist(lapply(tls, function(k) grep(k, fls_qc)))
      fls <- fls_qc[ids]
    }

    # if no files are available for target rapideye tile, skip current scene    
    if (length(fls) == 0)
      next
    
    dts <- getAcquisitionDate(fls, as_date = TRUE)
    dft <- difftime(dts, dts_mds[i], units = "days")
    
    ids_bfr <- dft[dft < 0]; ids_afr <- dft[dft > 0]
    if (any(length(ids_bfr) == 0 | length(ids_afr) == 0)) next
    
    # # import and clip chronologically closest rapideye scenes to modis 1-km 
    # # pixel extent
    # rst_bfr <- crop(stack(fls[which(dft == max(ids_bfr))[1]]), spy, snap = "out")
    # rst_afr <- crop(stack(fls[which(dft == min(ids_afr))[1]]), spy, snap = "out")

    if (n == 1) {
      dat <- data.frame(CellID = j, Date = dts_mds[i], File = fls_mds[i], 
                        FileREbefore = fls[which(dft == max(ids_bfr))[1]], 
                        FileREafter = fls[which(dft == min(ids_afr))[1]], 
                        stringsAsFactors = FALSE)
    } else {
      dat <- rbind(dat, data.frame(CellID = j, Date = dts_mds[i], File = fls_mds[i], 
                                   FileREbefore = fls[which(dft == max(ids_bfr))[1]], 
                                   FileREafter = fls[which(dft == min(ids_afr))[1]], 
                                   stringsAsFactors = FALSE))
    }
    
    n <- n + 1
  }
}

## write to file
write.csv(dat, "inst/extdata/modis_fires__rapideye_files.csv", 
          row.names = FALSE, quote = FALSE)

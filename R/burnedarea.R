### environmental stuff -----

## required packages
# devtools::install_github("MatMatt/MODIS", ref = "develop")
lib <- c("MODIS", "doParallel", "rgdal")
jnk <- sapply(lib, function(x) library(x, character.only = TRUE))

## modis options
MODISoptions(localArcPath = "/media/fdetsch/XChange/MODIS_ARC", 
             outDirPath = "/media/fdetsch/XChange/MODIS_ARC/PROCESSED/", 
             outProj = "+init=epsg:32637")

## parallelization
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)


### data download -----

## download and extract data
cll <- getCollection("MCD45A1", forceCheck = TRUE)
tfs <- runGdal("MCD45A1", collection = cll, tileH = 21, tileV = 8, 
               job = paste0("MCD45A1.", cll), SDSstring = "11000000")


### preprocessing -----

## target folders
dir_dat <- "data"
if (!dir.exists(dir_dat)) dir.create(dir_dat)

dir_prd <- paste(dir_dat, "MCD45A1.051", sep = "/")
if (!dir.exists(dir_prd)) dir.create(dir_prd)


### clipping -----

## reference extent
ref <- raster("data/MOD14A1.006/crp/MOD14A1.A2000049.FireMask.tif")
ref <- projectExtent(ref, crs = "+init=epsg:32637")

## target folder for clipping
dir_crp <- paste0(dir_prd, "/crp")
if (!dir.exists(dir_crp)) dir.create(dir_crp)

rst_crp <- foreach(i = c("burndate", "ba_qa")) %do% {                                      
  
  # list and import available files
  fls <- list.files(paste0(getOption("MODIS_outDirPath"), "/MCD45A1.051"),
                    pattern = paste0(i, ".tif$"), full.names = TRUE)
  
  # target files
  fls_crp <- paste(dir_crp, basename(fls), sep = "/")
  
  # loop over files
  lst_crp <- foreach(j = fls, k = fls_crp, .packages = "raster", 
                     .export = ls(envir = globalenv())) %dopar% {
    if (file.exists(k)) {
      raster(k)
    } else {                  
      rst <- raster(j)
      rst_crp <- crop(rst, ref, snap = "near", filename = k)
    }
  }
  
  stack(lst_crp)
}


### reclassification -----

## target folder and files
dir_rcl <- paste0(dir_prd, "/rcl")
if (!dir.exists(dir_rcl)) dir.create(dir_rcl)

fls_rcl <- paste0(dir_rcl, "/", names(rst_crp[[1]]), ".tif")

## reclassification matrix
rcl <- matrix(c(0, 1, NA, 
                367, 10000, NA), ncol = 3, byrow = TRUE)

## loop over layers
lst_rcl <- foreach(j = 1:nlayers(rst_crp[[1]]), .packages = lib) %dopar% {
  if (file.exists(fls_rcl[j])) {
    raster(fls_rcl[j])
  } else {
    reclassify(rst_crp[[1]][[j]], rcl, include.lowest = TRUE, 
               right = FALSE, filename = fls_rcl[j])
  }
}

rst_rcl <- stack(lst_rcl); rm(lst_rcl)


### annual fires -----

dts <- extractDate(rst_rcl)$inputLayerDates
indices <- format(as.Date(dts, "%Y%j"), "%Y")
indices <- as.integer(indices)

rst_agg <- stackApply(rst_rcl, indices, fun = function(x, ...) {
  sum(!is.na(x), ...)
}, na.rm = FALSE)

## write to disk
dir_agg <- paste0(dir_prd, "/agg1yr")
if (!dir.exists(dir_agg)) dir.create(dir_agg)

fls_agg <- paste0(unique(substr(names(rst_rcl), 1, 13)), ".burndate.tif")
fls_agg <- paste(dir_agg, fls_agg, sep = "/")

rst_agg <- foreach(i = 1:nlayers(rst_agg), .combine = "stack") %do%
  writeRaster(rst_agg[[i]], filename = fls_agg[i], format = "GTiff", 
              overwrite = TRUE)

## deregister parallel backend
stopCluster(cl)

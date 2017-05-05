### environmental stuff -----

## required packages
# devtools::install_github("MatMatt/MODIS", ref = "develop")
lib <- c("MODIS", "rgdal")
jnk <- sapply(lib, function(x) library(x, character.only = TRUE))

## modis options
MODISoptions(localArcPath = "/media/fdetsch/XChange/MODIS_ARC", 
             outDirPath = "/media/fdetsch/XChange/MODIS_ARC/PROCESSED/", 
             outProj = "+init=epsg:32637")


### data download -----

## download and extract data
cll <- getCollection("MCD45A1", forceCheck = TRUE)
tfs <- runGdal("MCD45A1", collection = cll, 
               extent = readRDS("inst/extdata/uniformExtent.rds"), 
               job = paste0("MCD45A1.", cll), SDSstring = "11000000", 
               overwrite = TRUE)


### preprocessing -----

## target folders
dir_dat <- "data"
if (!dir.exists(dir_dat)) dir.create(dir_dat)

dir_prd <- paste(dir_dat, "MCD45A1.051", sep = "/")
if (!dir.exists(dir_prd)) dir.create(dir_prd)


### reclassification -----

## target folder and files
dir_rcl <- paste0(dir_prd, "/rcl")
if (!dir.exists(dir_rcl)) dir.create(dir_rcl)

## reclassification matrix
rcl <- matrix(c(0, 1, NA, 
                367, 10000, NA), ncol = 3, byrow = TRUE)

## start and end date
lmt <- sapply(c(names(fls)[1], names(fls[length(fls)])), function(z) {
  transDate(z)$beginDOY
})

fls <- unlist(sapply(tfs[[1]], "[[", 1))
fls_rcl <- paste0(dir_rcl, "/MCD45A1.", 
                  paste(lmt, collapse = "_"), ".burndate.tif")

## reclassify layers
rst <- stack(fls)
rst_rcl <- reclassify(rst, rcl, include.lowest = TRUE, right = FALSE, 
                      filename = fls_rcl)


### annual fires -----

dts <- extractDate(fls)$inputLayerDates
indices <- format(as.Date(dts, "%Y%j"), "%Y")
indices <- as.integer(indices)

rst_agg <- stackApply(rst_rcl, indices, fun = function(x, ...) {
  sum(!is.na(x), ...)
}, na.rm = FALSE)

## write to disk
dir_agg <- paste0(dir_prd, "/agg1yr")
if (!dir.exists(dir_agg)) dir.create(dir_agg)

fls_agg <- paste0(dir_agg, "/MCD45A1.", 
                  paste(substr(lmt, 1, 4), collapse = "_"), ".burndate.tif")

rst_agg <- writeRaster(rst_agg, filename = fls_agg)

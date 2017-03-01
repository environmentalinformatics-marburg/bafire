### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## working directory
setwd("/media/permanent/data/bale")

## packages
# devtools::install_github("environmentalinformatics-marburg/reset")
library(reset)
# devtools::install_github("italocegatta/rapidr")
library(rapidr)

## functions
source("../../repo/bafire/R/rapid_qc.R")
source("../../repo/bafire/R/rapid_toar.R")


### quality control -----

drs <- "rapideye/3742218_2016-02-11_RE2_3A_627058"
fls <- list.files(drs, pattern = "_3A_", full.names = TRUE)

## rapideye bands
tfs <- fls[grep(".tif$", fls)]
bds <- brick(tfs[2])

## unusable data mask (udm)
qty <- raster(tfs[3])

## perform quality control
qcl <- rapid_qc(bds, qty)


### top-of-atmosphere radiances -----

## metadata
mtd <- fls[grep(".xml$", fls)]

fls <- paste(drs, "scl", basename(tfs[2]), sep = "/")
scl <- rapid_toar(qcl, mtd, digits = 3L, filename = fls)


### atmospheric correction (see 
### https://grasswiki.osgeo.org/wiki/Atmospheric_correction) -----

## geometrical conditions
mtd <- xml2::read_xml(mtd)
re_dt <- rapid_date(mtd); re_hr <- rapid_hour(mtd); re_ll <- rapid_latlon(mtd)

geo <- paste(format(re_dt, "%m %d")                         # month and day
             , round(re_hr, 2)                              # decimal hours
             , paste(rev(round(re_ll, 3)), collapse = " ")) # coordinates

## mean elevation (tile-based information could be stored in separate images)
dem_utm <- raster("dem/dem_srtm_01_utm.tif")
dem_res <- resample(dem_utm, bds)

ele <- round(mean(dem_res[]) / 1000 * (-1), digits = 3)

## aerosol optical depth (aod)
hdf <- list.files("modis", pattern = ".hdf$", full.names = TRUE)
hdf <- hdf[grep(format(re_dt, "%Y%j"), hdf)]

aod <- do.call(mean, lapply(hdf, function(fl) {
  sds <- getSwathSDS(fl, "Aerosol_Optical_Depth_Land_Ocean_Mean", 
                     ext = projectExtent(bds, crs = "+init=epsg:4326"))
  round(mean(sds[[1]][[1]][], na.rm = TRUE), digits = 3)
}))

## band code
bnd <- switch(as.character(i), "1" = 88, "2" = 89, "3" = 90, "4" = 91, "5" = 92)

## create parameter file
prm <- c(13      # sensor, 13 = "rapideye"
         , geo   # geometrical conditions 
         , 1     # atmospheric model, 1 = "tropical"
         , 1     # aerosols model, 1 = "continental"
         , 0     # visibility, 0 = use aod (next line)
         , aod   # aod
         , ele   # mean elevation
         , -1000 # sensor height, -1000 = sensor onboard a satellite
         , bnd   # satellite band, 88 (:92) = rapideye blue (to nir) band
         )

fls_prm <- paste0(drs, "/prm/", gsub("tif$", "", basename(tfs[2])), i, ".txt")
writeLines(prm, "rapideye/3742218_2016-02-11_RE2_3A_627058/prm/parameters.txt")


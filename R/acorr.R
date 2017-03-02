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
library(rgrass7)
# devtools::install_github("fdetsch/Orcs")
library(Orcs)

## functions
source("../../repo/bafire/R/rapid_qc.R")
source("../../repo/bafire/R/rapid_rad.R")
source("../../repo/bafire/R/rapid_ref.R")

## raster options, write format used for temporary files and readable from GRASS
jnk <- capture.output(opt <- rasterOptions())
rasterOptions(format = "HFA")

## grass-gis initialization parameters
os <- Sys.info()[["sysname"]]
gisBase <- system("grass72 --config path", intern = TRUE) # works on windows?
gisDbase <- paste0(switch(os, "Linux" = "/media/permanent/", "Windows" = "E:/"), 
                   "data/grassdata/")

## parallelization
library(parallel)
cl <- makePSOCKcluster(3L)


### quality control -----

drs <- "rapideye/3742218_2016-02-11_RE2_3A_627058"
fls <- list.files(drs, pattern = "_3A_", full.names = TRUE)

## rapideye bands
tfs <- fls[grep(".tif$", fls)]
bds <- brick(tfs[2])

## unusable data mask (udm)
qty <- raster(tfs[3])

## perform quality control
mat <- matrix(rep(1, 25), ncol = 5); mat[3, 3] <- 0
qcl <- rapid_qc(bds, qty, directions = mat)


### top-of-atmosphere (toa) reflectances -----

## metadata
mtd <- fls[grep(".xml$", fls)]
mtd <- xml2::read_xml(mtd)

## compute toa reflectances
rfl <- rapid_ref(qcl, mtd, digits = 3L)
rm(qcl)


### atmospheric correction (see 
### https://grasswiki.osgeo.org/wiki/Atmospheric_correction) -----

## geometrical conditions
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

## initialize new location and baseline mapset with required epsg code
initGRASS(gisBase = gisBase, home = raster::tmpDir(), 
          gisDbase = gisDbase, location = "bale_utm", 
          mapset = "PERMANENT", override = TRUE)

execGRASS("g.proj", flags = "c", epsg = 32637)

## loop over single bands
atc <- vector("list", nlayers(rfl))
for (i in 1:nlayers(rfl)) {
  
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
  
  fls_prm <- paste0(drs, "/", gsub(".tif$", "", basename(tfs[2])), "_param.txt")
  writeLines(prm, fls_prm)
  
  ## during first run, initialize new mapset inheriting projection info
  if (i == 1) {
    execGRASS("g.mapset", flags = "c", mapset = pureBasename(tfs[2]))
    
    ## assign spatial settings (extent, number of rows and columns, resolution)  
    ## from resampled digital elevation model to current geographic region
    execGRASS("r.external", flags = "overwrite",
              parameters = list(input = attr(dem_res@file, "name"), 
                                output = "dem"))
    
    execGRASS("g.region", raster = "dem")
  }

  ## import current rapideye band and perform atmospheric correction
  execGRASS("r.external", flags = "overwrite", 
            parameters = list(input = attr(rfl@file, "name"), 
                              band = i, output = "bnd"))

  execGRASS("i.atcorr", flags = c("r", "overwrite"), 
            parameters = list(input = "bnd", elevation = "dem", 
                              parameters = fls_prm, output = "atc"))
  file.remove(fls_prm)

  ## write image to disk  
  fls_atc <- paste0(drs, "/", pureBasename(tfs[2]), "_ATC-", i, ".tif")
  execGRASS("r.out.gdal", flags = "overwrite", 
            parameters = list(input = "atc", output = fls_atc, 
                              format = "GTiff"))
  
  atc[[i]] <- raster(fls_atc)
}

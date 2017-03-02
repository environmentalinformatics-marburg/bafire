## parallelization
library(parallel)
cl <- makePSOCKcluster(12L)
jnk <- clusterEvalQ(cl, {library(reset); library(rapidr); library(xml2)})

drs <- list.dirs("rapideye", recursive = FALSE)



aod <- parLapply(cl, drs, function(i) {

  ## metadata
  fls <- list.files(drs, pattern = "_3A_", full.names = TRUE)
  mtd <- fls[grep(".xml$", fls)]
  mtd <- read_xml(mtd)

  ## aerosol optical depth (aod)
  re_dt <- rapid_date(mtd)
  
  hdf <- list.files("modis", pattern = ".hdf$", full.names = TRUE)
  hdf <- hdf[grep(format(re_dt, "%Y%j"), hdf)]
  
  ext <- projectExtent(bds, crs = "+init=epsg:4326")
  do.call(mean, lapply(hdf, function(fl) {
    sds <- getSwathSDS(fl, "Aerosol_Optical_Depth_Land_Ocean_Mean", ext = ext)
    round(mean(sds[[1]][[1]][], na.rm = TRUE), digits = 3)
  }))
})
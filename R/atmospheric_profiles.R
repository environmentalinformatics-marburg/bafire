### environment -----

## clear workspace
rm(list = ls(all = TRUE))

## load packages
lib <- c("reset", "parallel")
Orcs::loadPkgs(lib)

## set working directory
setwd("/media/fdetsch/data/bale")

## parallelization
cl <- makePSOCKcluster(detectCores() - 1)
jnk <- clusterEvalQ(cl, library(reset))

## reference extent
ref <- raster("dem/dem_srtm_01_utm.tif")
ref <- projectExtent(ref, crs = "+init=epsg:4326")
ref <- as(extent(ref), "SpatialPolygons")

## required sds
prm <- c("Total_Ozone", "Water_Vapor", "Water_Vapor_Direct")
clusterExport(cl, "prm")


### rearrange data -------------------------------------------------------------

## loop over products
for (product in c("MOD07_L2", "MYD07_L2")) {
  
  ## status message
  cat("Commencing with processing of", product, "...\n")
  
  ## list available files and remove duplicates
  fls <- list.files(paste0("modis/", product), full.names = TRUE, recursive = TRUE,
                    pattern = paste(product, "006", ".hdf$", sep = ".*"))
  
  if (anyDuplicated(basename(fls)) != 0) {
    dpl <- which(duplicated(basename(fls)))
    jnk <- file.remove(fls[dpl])
    fls <- fls[-dpl]
  }

  ## sort by datetime and stop if any date is missing
  mtd <- Orcs::list2df(strsplit(basename(fls), "\\."), stringsAsFactors = FALSE)
  names(mtd) <- c("product", "date", "time", "collection", "processing", "filetype")
  
  mtd$datetime <- strptime(paste(mtd$date, mtd$time), format = "A%Y%j %H%M")
  fls <- fls[order(mtd$datetime)]; # mtd <- mtd[order(mtd$datetime), ]


  ### process coordinates -----

  # ## discard nighttime scenes (infrared-based, i.e. 5 km spatial resolution)
  # tms <- as.numeric(getSwathTime(fls)) 
  # fls <- fls[tms >= 600 & tms <= 1800]
  
  ## discard scenes not covering reference extent
  lst_ext <- vector("list", length(fls)); n <- 1
  for (i in fls) { 
    if (n %% 100 == 0) cat("Completed", n, "out of", length(fls), "files ...\n")
    lst_ext[[n]] <- try(reset::getSwathExtent(i), silent = TRUE); n <- n + 1
  }
  
  inside <- sapply(lst_ext, function(i) {
    if (!inherits(i, "try-error")) {
      rgeos::gIntersects(as(i, "SpatialPolygons"), ref)
    } else {
      FALSE
    }
  })
  
  if (any(!inside)) {
    jnk <- file.remove(fls[!inside])
    fls <- fls[inside]
  } 
  
  ## extract relevant sds
  clusterExport(cl, "product")
  parLapply(cl, fls, function(i) {
    sds <- getSwathSDS(i, prm = prm, dsn = paste0("modis/", product))[[1]]
    stack(sds)
  })
}

## deregister parallel backend
stopCluster(cl)

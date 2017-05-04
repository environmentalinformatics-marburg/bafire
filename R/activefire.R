### environmental stuff -----

## required packages
# devtools::install_github("MatMatt/MODIS", ref = "develop")
lib <- c("MODIS", "doParallel", "rgdal")
jnk <- sapply(lib, function(x) library(x, character.only = TRUE))

## modis options
os <- switch(Sys.info()[["sysname"]]
             , "Windows" = "F:/"
             , "Linux" = "/media/fdetsch/XChange/")

MODISoptions(localArcPath = paste0(os, "MODIS_ARC")
             , outDirPath = paste0(os, "MODIS_ARC/PROCESSED/")
             , outProj = "+init=epsg:32637")

## parallelization
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)


### data download -----

## reference extent
source("R/uniformExtent.R")
ref <- uniformExtent(f = 0)

## download and extract data
cll <- getCollection("M*D14A1")
tfs <- runGdal("M*D14A1", extent = ref, job = "MCD14A1.006",
               end = "2015365", collection = unique(unlist(cll)))


### preprocessing -----

## data folder
dir_dat <- "data"
if (!dir.exists(dir_dat)) dir.create(dir_dat)

## loop over products
lst_prd <- lapply(c("MOD14A1.006", "MYD14A1.006"), function(product) {
  
  ## product-specific target folder
  dir_prd <- paste(dir_dat, product, sep = "/")
  if (!dir.exists(dir_prd)) dir.create(dir_prd)
  
  rst <- foreach(i = c("FireMask", "QA", "MaxFRP", "sample")) %do% {                                      
    
    # list and import available files
    fls <- list.files(paste0(getOption("MODIS_outDirPath"), "/MCD14A1.006"),
                      pattern = paste(gsub("\\.006", "", product), i, ".tif$", sep = ".*"), 
                      full.names = TRUE)
    
    lst <- foreach(j = fls, .packages = "raster", 
                   .export = ls(envir = environment())) %dopar% {
      rst <- stack(j)
      
      if (i == "MaxFRP")
        rst <- rst * 0.1
      
      return(rst)
    }
    
    stack(lst)
  }
    
  
  ### reclassification -----
  
  ## target folder and files
  dir_rcl <- paste0(dir_prd, "/rcl")
  if (!dir.exists(dir_rcl)) dir.create(dir_rcl)
  
  fls_rcl <- paste0(dir_rcl, "/", names(rst_crp[[1]]), ".tif")
  
  ## reclassification matrix
  rcl <- matrix(c(0, 7, NA, 
                  7, 10, 1, 
                  10, 255, NA), ncol = 3, byrow = TRUE)
  
  ## loop over layers
  lst_rcl <- foreach(j = 1:nlayers(rst_crp[[1]]), .packages = lib, 
          .export = ls(envir = globalenv())) %dopar% {
    if (file.exists(fls_rcl[j])) {
      raster(fls_rcl[j])
    } else {
      reclassify(rst_crp[[1]][[j]], rcl, include.lowest = TRUE, 
                 right = FALSE, filename = fls_rcl[j], 
                 format = "GTiff", overwrite = TRUE)
    }
  }
  
  rst_rcl <- stack(lst_rcl); rm(lst_rcl)
  return(rst_rcl)
  
  # ### annual fire frequencies -----
  # 
  # ## indices
  # dts <- extractDate(names(rst[[1]]))$inputLayerDates
  # yrs <- substr(dts, 1, 4)
  # ids <- as.numeric(as.factor(yrs))
  # 
  # ## target folder and files
  # dir_yrs <- paste0(dir_prd, "/yrs")
  # if (!dir.exists(dir_yrs)) dir.create(dir_yrs)
  # 
  # fls_yrs <- sapply(unique(yrs), function(j) {
  #   gsub(dts[1], j, names(rst[[1]])[1])
  # })
  # fls_yrs <- paste0(dir_yrs, "/", fls_yrs, ".tif")
  # 
  # for (j in seq(unique(ids))) {
  #   if (!file.exists(fls_yrs[j])) {
  #     rst_yr <- rst_rcl[[which(ids == unique(ids)[j])]]
  #     calc(rst_yr, fun = function(x) {
  #       sum(x, na.rm = TRUE) / length(x)
  #     }, filename = fls_yrs[j], format = "GTiff", overwrite = TRUE)
  #     rm(rst_yr)
  #   }
  # }
  # 
  # rst_frq <- stack(fls_yrs)
  
})


### combined product -----

## target folder and files
dir_cmb <- paste0(dir_dat, "/MCD14A1.006")
if (!dir.exists(dir_cmb)) dir.create(dir_cmb)

## layer dates
lst_dts <- foreach(product = c("MOD14A1.006", "MYD14A1.006"), i = 1:2) %do% {
                     
  # list available .hdf files                   
  dir_hdf <- paste0(getOption("MODIS_localArcPath"), "/MODIS/", product)
  fls_hdf <- list.files(dir_hdf, pattern = "h21v08.*.hdf$", full.names = TRUE, 
                        recursive = TRUE)
  
  # extract corresponding dates
  dts <- fireDates(fls_hdf)
  dts <- unlist(dts)

  if (length(dts) != nlayers(lst_prd[[i]]))
    stop("Number of layers and dates must be the same.\n")
  
  # remove duplicated layers
  dpl <- which(duplicated(dts))
  lst_prd[[i]] <- lst_prd[[i]][[-dpl]]; dts <- dts[-dpl]

  list(lst_prd[[i]], dts)
}

## combine layers from the same day
dts_terra <- lst_dts[[1]][[2]]; dts_aqua <- lst_dts[[2]][[2]]
dts <- c(dts_terra, dts_aqua)
dts <- sort(unique(dts))

## target files
chr <- format(as.Date(dts), "%Y%j")
fls_cmb <- paste0(dir_cmb, "/MCD14A1.A", chr, ".FireMask.tif")

lst_cmb <- foreach(i = dts, j = seq(dts), .packages = "raster") %dopar% {
  
  if (file.exists(fls_cmb[j])) {
    raster(fls_cmb[j])
  } else {
    avl_terra <- i %in% dts_terra; avl_aqua <- i %in% dts_aqua
    
    if (avl_terra & !avl_aqua) {
      writeRaster(lst_dts[[1]][[1]][[grep(i, dts_terra)]], filename = fls_cmb[j], 
                  format = "GTiff", overwrite = TRUE)
    } else if (!avl_terra & avl_aqua) {
      writeRaster(lst_dts[[2]][[1]][[grep(i, dts_aqua)]], filename = fls_cmb[j], 
                  format = "GTiff", overwrite = TRUE)
    } else if (avl_terra & avl_aqua) {
      id_terra <- grep(i, dts_terra); id_aqua <- grep(i, dts_aqua)
      overlay(lst_dts[[1]][[1]][[id_terra]], lst_dts[[2]][[1]][[id_aqua]], 
              fun = function(x, y) {
                val_x <- x[]; val_y <- y[]
                val_x[!is.na(val_y)] <- val_y[!is.na(val_y)]
                return(val_x)
              }, filename = fls_cmb[j], format = "GTiff", overwrite = TRUE)
    } else {
      stop("Date '", i, "' not present in any of the products.\n")
    }
  }
}

rst_cmb <- stack(lst_cmb); rm(lst_cmb)


### annual fires -----

indices <- format(as.Date(dts), "%Y")
indices <- as.integer(indices)

rst_agg <- stackApply(rst_cmb, indices, fun = sum)

## write to disk
dir_agg <- paste0(dir_cmb, "/agg1yr")
if (!dir.exists(dir_agg)) dir.create(dir_agg)

fls_agg <- paste0(unique(substr(names(rst_cmb), 1, 13)), ".FireMask.tif")
fls_agg <- paste(dir_agg, fls_agg, sep = "/")

rst_agg <- foreach(i = 1:nlayers(rst_agg), .combine = "stack") %do%
  writeRaster(rst_agg[[i]], filename = fls_agg[i], format = "GTiff", 
              overwrite = TRUE)

## deregister parallel backend
stopCluster(cl)
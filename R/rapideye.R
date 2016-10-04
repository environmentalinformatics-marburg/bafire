### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## required packages
# devtools::install_github("MatMatt/MODIS", ref = "develop")
lib <- c("doParallel", "raster")
jnk <- sapply(lib, function(x) library(x, character.only = TRUE))

## required functions
source("R/getInfo.R")

## parallelization
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)


### extraction -----

## list available file containers
dir_prd <- switch(Sys.info()[["sysname"]], 
                  "Linux" = "/media/fdetsch/XChange/RapidEye/", 
                  "Windows" = "D:/RapidEye/")

fls_ctn <- list.files(dir_prd, pattern = "^getProduct.*streaming=True$", 
                      full.names = TRUE)

## loop over containers and extract required files
lst <- foreach(i = fls_ctn) %dopar% {
  tfs <- unzip(i, list = TRUE)$Name
  
  ord <- unique(getOrder(tfs))
  ids <- sapply(paste0(c(ord, "udm"), ".tif$"), function(j) grep(j, tfs))
  tfs <- tfs[ids]
  
  unzip(i, files = tfs, exdir = dirname(i))
  paste0(dir_prd, tfs)
}
  

### quality control -----

## target folder and files
dir_qc <- paste0(dir_prd, "qc/")
if (!dir.exists(dir_qc)) dir.create(dir_qc)

fls_qc <- basename(sapply(lst, "[[", 1))
fls_qc <- paste0(dir_qc, fls_qc)

lst_qc <- foreach(i = lst, j = as.list(fls_qc), .packages = "raster") %dopar% {
  # if target file exists, import right away
  if (file.exists(j)) {
    stack(j)
  } else {
    # else import 5-band raster (optionally apply scale factor of 
    # 9.999999776482582e-03)
    rst <- stack(i[1])
    
    # import quality layer and resample to 5-m target resolution
    qc <- raster(i[2])
    qc <- resample(qc, rst, method = "ngb")

    # discard all pixels with a quality flag value larger than 0    
    overlay(rst, qc, fun = function(x, y) {
      x[y[] != 0] <- NA
      return(x)
    }, filename = j, format = "GTiff", overwrite = TRUE)
  }
}

  ## crop images
  rst_crp <- foreach(i = c("NDVI", "pixel_reliability", "VI_Quality"), 
                     .packages = lib, .export = ls(envir = globalenv())) %dopar% {                                      
                       
                       # list and import available files
                       fls <- list.files(paste0(getOption("MODIS_outDirPath"), "/", product, ".006"),
                                         pattern = paste0(i, ".tif$"), full.names = TRUE)
                       rst <- raster::stack(fls)
                       
                       # crop
                       dir_out <- paste0(dir_prd, "/crp")
                       if (!dir.exists(dir_out)) dir.create(dir_out)
                       
                       fls_out <- paste0(dir_out, "/", basename(fls))
                       
                       lst_out <- lapply(1:(raster::nlayers(rst)), function(j) {
                         if (file.exists(fls_out[j])) {
                           raster::raster(fls_out[j])
                         } else {
                           rst_out <- raster::crop(rst[[j]], ext, snap = "out")
                           
                           # apply scale factor
                           if (i %in% c("NDVI", "EVI"))
                             rst_out <- rst_out * 0.0001
                           
                           # save and return cropped layers
                           raster::writeRaster(rst_out, filename = fls_out[j],
                                               format = "GTiff", overwrite = TRUE)
                         }
                       })
                       
                       raster::stack(lst_out)
                     }
  
  
  ### quality control, step #1: -----
  ### discard clouds, snow/ice and filled pixels using 'pixel_reliability'
  
  dir_qc1 <- paste0(dir_prd, "/qc1")
  if (!dir.exists(dir_qc1)) dir.create(dir_qc1)
  
  ## perform quality check #1 for both NDVI and EVI
  fls_qc1 <- paste0(dir_qc1, "/", names(rst_crp[[1]]), ".tif")
  
  lst_qc1 <- foreach(i = 1:nlayers(rst_crp[[1]]), .packages = lib, 
                     .export = ls(envir = globalenv())) %dopar% {
                       if (file.exists(fls_qc1[i])) {
                         raster(fls_qc1[i])
                       } else {
                         overlay(rst_crp[[1]][[i]], rst_crp[[2]][[i]], fun = function(x, y) {
                           x[!y[] %in% c(0, 1)] <- NA
                           return(x)
                         }, filename = fls_qc1[i], overwrite = TRUE, format = "GTiff")
                       }
                     }
  
  rst_qc1 <- stack(lst_qc1); rm(lst_qc1)
  
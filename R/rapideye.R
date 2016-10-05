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

# fls_ctn <- list.files(dir_prd, pattern = "^getProduct.*streaming=True$", 
#                       full.names = TRUE)
# 
# ## loop over containers and extract required files
# lst <- foreach(i = fls_ctn) %dopar% {
#   tfs <- unzip(i, list = TRUE)$Name
#   
#   fls <- paste0(dir_prd, tfs)
#   if (any(!file.exists(fls))) {
#     ord <- unique(getOrder(tfs))
#     ids <- sapply(paste0(c(ord, "udm"), ".tif$"), function(j) grep(j, tfs))
#     tfs <- tfs[ids]
#     
#     unzip(i, files = tfs, exdir = dirname(i))
#   }
#   
#   return(fls)
# }

## re-list extracted files
drs <- dir(dir_prd, pattern = "_3A_", full.names = TRUE)

lst <- lapply(drs, function(i) {
  list.files(i, pattern = ".tif$", full.names = TRUE)
})
  

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

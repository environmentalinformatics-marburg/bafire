library(ESD)

## modis options
MODISoptions(MODISserverOrder = c("LAADS", "LPDAAC"), 
             outProj = "+init=epsg:32637")

## reference extent
bmn <- shapefile("inst/extdata/balemountains")
ref <- suppressWarnings(rgeos::gBuffer(bmn, width = .1))


### data download -----

sds <- download("MODIS", product = "M*D11A1", collection = "006", extent = ref,
                SDSstring = "110011000011", job = "MCD11A1.006_Bale")


### data processing (should be included in a separate function) -----

setwd("/media/fdetsch/data/bale")

## loop over single product
lst <- lapply(c("MOD11A1", "MYD11A1"), function(product) {
  
  ## status message
  cat("Commencing with the processing of", product, "...\n")
  
  ### crop layers -----
  
  ## setup output folder
  dir_prd <- paste0("data/", product, ".006")
  if (!dir.exists(dir_prd)) dir.create(dir_prd)
  
  ## perform crop
  pattern <- c("Day_1km", "QC_Day", "Clear_day_cov", 
               "Night_1km", "QC_Night", "Clear_night_cov")
  
  rst_crp <- foreach(i = pattern, .packages = "MODIS", 
                     .export = ls(envir = globalenv())) %dopar% {
                       
    # list and import available files
    fls <- list.files(paste0(getOption("MODIS_outDirPath"), "/MCD11A1.006_Bale"),
                      pattern = paste0(i, ".tif$"), full.names = TRUE)
    

    ### scaling -----
    
    ## apply scale factor
    dir_scl <- paste0(dir_prd, "/scl")
    if (!dir.exists(dir_scl)) dir.create(dir_scl)
    fls_raw <- sapply(x[[which(names(x) == product)]], "[[", 1)
    fls_scl <- paste0(dir_scl, "/", basename(fls_raw))
    
    parallel::clusterExport(cl, c("rst_crp", "fls_scl"), envir = environment())
    
    lst_scl <- parallel::parLapply(cl, 1:raster::nlayers(rst_crp[[1]]), 
                                   function(i) {
                                     if (file.exists(fls_scl[i])) {
                                       raster::raster(fls_scl[i])
                                     } else {
                                       raster::calc(rst_crp[[1]][[i]], fun = function(x) x * 0.0001, 
                                                    filename = fls_scl[i], overwrite = TRUE)
                                     }
                                   })
    
    rst_scl <- raster::stack(lst_scl); rm(lst_scl)
    
    # crop
                       dir_crp <- paste0(dir_out, "/crp")
                       if (!dir.exists(dir_crp)) dir.create(dir_crp)
                       
                       fls_crp <- paste(dir_crp, basename(fls), sep = "/")
                       
                       lst_crp <- lapply(1:(raster::nlayers(rst)), function(j) {
                         if (file.exists(fls_crp[j])) {
                           raster::raster(fls_crp[j])
                         } else {
                           rst_crp <- raster::crop(rst[[j]], rst_ref, snap = "near")
                           
                           # if dealing with (day or night) lst bands, convert to 16-bit unsigned
                           # integer and apply scale factor of 0.02 and offset of -273.15
                           if (i %in% c("Day_1km", "Night_1km")) {
                             raster::dataType(rst_crp) <- "INT2U"
                             rst_crp <- rst_crp * 0.02 - 273.15
                             
                             # else if dealing with (day or night) no. of clear-sky observations, 
                             # convert to 16-bit unsigned integer and apply scale factor of 0.0005
                           } else if (i %in% c("Clear_sky_days", "Clear_sky_nights")) {
                             raster::dataType(rst_crp) <- "INT2U"
                             rst_crp <- rst_crp * 0.0005
                             
                             # else convert to 8-bit unsigned integer
                           } else {
                             raster::dataType(rst_crp) <- "INT1U"
                           }
                           
                           # save and return cropped layers
                           raster::writeRaster(rst_crp, filename = fls_crp[j],
                                               format = "GTiff", overwrite = TRUE)
                         }
                       })
                       
                       raster::stack(lst_crp)
                     }
  
  
  ### quality control ----------------------------------------------------------
  ### discard cloudy pixels based on companion quality information ('QC_Day', 
  ### 'QC_Night')
  
  dir_qc <- paste0(dir_out, "/qc")
  if (!dir.exists(dir_qc)) dir.create(dir_qc)
  
  ## perform quality check for day and night separately
  lst_qc <- foreach(i = rst_crp[c(1, 4)], j = rst_crp[c(2, 6)]) %do% {
    
    ## loop over layers
    fls_qc <- paste0(dir_qc, "/", names(i), ".tif")
    lst_out <- foreach(k = 1:nlayers(i), .packages = "raster", 
                       .export = ls(envir = globalenv())) %dopar% {
                         
                         if (file.exists(fls_qc[k])) {
                           raster(fls_qc[k])
                         } else {
                           overlay(i[[k]], j[[k]], fun = function(x, y) {
                             id <- sapply(y[], function(l) {
                               bin <- Rsenal::number2binary(l, 8, TRUE)
                               mandatory_qa <- substr(bin, 7, 8)
                               data_quality <- substr(bin, 5, 6)
                               
                               # pixel produced, good quality
                               if (mandatory_qa == "00" | data_quality == "00") {
                                 return(TRUE)
                                 
                                 # pixel produced, unreliable or unquantifiable quality
                               } else if (mandatory_qa == "01" | data_quality == "01") {
                                 emis_error <- substr(bin, 3, 4) == "00"
                                 lst_error <- substr(bin, 1, 2) == "00"
                                 
                                 return(all(emis_error, lst_error))
                                 
                                 # pixel not produced due to cloud effects or other reasons
                               } else {
                                 return(FALSE)
                               }
                             })
                             
                             x[!id] <- NA
                             return(x)
                           }, filename = fls_qc[k], overwrite = TRUE, format = "GTiff")
                         }
                       }
    
    stack(lst_out)
  }
})

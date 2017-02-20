library(ESD)

## modis options
MODISoptions(MODISserverOrder = c("LAADS", "LPDAAC"), 
             outProj = "+init=epsg:32637")

## reference extent
bmn <- shapefile("inst/extdata/balemountains")
ref <- suppressWarnings(rgeos::gBuffer(bmn, width = .1))


### data download -----

sds <- download("MODIS", product = "M*D13Q1", collection = "006", extent = ref,
                SDSstring = "101000000011", job = "MCD13Q1.006_Bale")


### data processing -----

## ndvi
rst <- preprocess(x = sds, dsn = "/media/fdetsch/data/bale/data", cores = 3L)


## write to disc
fls_qc2 <- list.files("/media/fdetsch/data/bale/data/MCD13Q1.006", 
                      pattern = ".tif$", full.names = TRUE)
fls_wht <- paste0(unique(dirname(fls_qc2)), "/whittaker/", basename(fls_qc2))

library(parallel)
cl <- makePSOCKcluster(detectCores() - 1)
jnk <- clusterEvalQ(cl, library(raster))
clusterExport(cl, c("rst", "fls_wht"))
lst_wht <- parLapply(cl, 1:(raster::nlayers(rst)), function(i) {
  tmp <- rst[[i]]
  tmp[tmp[] > 1 | tmp[] < (-1)] <- NA
  
  writeRaster(tmp, filename = fls_wht[i], format = "GTiff", overwrite = TRUE)
})

rst_wht <- stack(lst_wht)

## remove deprecated whittaker-related files
fls_old <- list.files(unique(dirname(fls_wht)), pattern = "yL5000", 
                      full.names = TRUE)
jnk <- file.remove(fls_old)

stopCluster(cl)

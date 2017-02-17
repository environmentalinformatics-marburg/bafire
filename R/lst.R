library(ESD)

## modis options
MODISoptions(MODISserverOrder = c("LAADS", "LPDAAC"), outProj = "32637")

## reference extent
bmn <- shapefile("inst/extdata/balemountains")
ref <- suppressWarnings(rgeos::gBuffer(bmn, width = .1))


### data download -----

sds <- download("MODIS", product = "M*D11A1", collection = "006", extent = ref,
                SDSstring = "110011000011", job = "MCD11A1.006_Bale")

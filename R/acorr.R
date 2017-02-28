setwd("/media/fdetsch/data/bale/data/RapidEye")

library(raster)
library(rgdal)

fls <- list.files("3742218_2016-02-11_RE2_3A_627058", full.names = TRUE)
tfs <- fls[grep(".tif$", fls)]

mtd <- fls[grep(".xml$", fls)]
mtd <- xml2::read_xml(mtd)

bds <- brick(tfs[2])
qty <- raster(tfs[3])

# devtools::install_github("italocegatta/rapidr")
library(rapidr)
?rapid_reflec
scl <- rapid_sf(mtd)

library(parallel)
cl <- makePSOCKcluster(detectCores() - 1)
jnk <- clusterEvalQ(cl, library(raster))
clusterExport(cl, c("bds", "scl"))

lst <- parLapply(cl, 1:length(scl), function(i) {
  bds[[i]] * scl[i]
})
rst <- brick(lst)

rfl <- rapid_reflec(bds, xml2::read_xml(mtd))


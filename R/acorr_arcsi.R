### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## working directory
setwd("/media/fdetsch/data/bale")

## packages
# devtools::install_github("environmentalinformatics-marburg/reset")
# devtools::install_github("environmentalinformatics-marburg/Rsenal")
# devtools::install_github("italocegatta/rapidr")
# devtools::install_github("fdetsch/Orcs")
lib <- c("parallel", "rapidr", "Orcs", "reset", "Rsenal")
Orcs::loadPkgs(lib)

## functions
bfr <- "~/repo/bafire/"
jnk <- sapply(c("getInfo", "rapid_qc", "weightedAverage", "kea2tif", 
                "rapid_shadows", "findpeaks"), function(i) {
  source(paste0(bfr, "R/", i, ".R"))
})


# ### extraction -----
# 
# ## list available file containers
# ctn <- list.files("rapideye", pattern = "^getProduct.*streaming=True$",
#                   full.names = TRUE)
# 
# ## loop over containers and extract required files
# lst <- parLapply(cl, ctn, function(i) {
#   fls <- unzip(i, list = TRUE)$Name
#   ord <- unique(getOrder(fls))
#   ids <- sapply(c(paste0(c(ord, "udm"), ".tif$"), "metadata.xml"), 
#                 function(j) grep(j, fls))
#   unzip(i, files = fls[ids], exdir = dirname(i), overwrite = FALSE)
# })


### atmospheric correction (see 
### http://rsgislib.org/arcsi/scripts.html#arcsi-py) -----

## valid arcsi values for aot, water content
# aod_vld <- c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 0.95)
# wct_vld <- c(0.5, 1:6, 8:9)

## directions for adjacent pixels
adj <- matrix(rep(1, 25), ncol = 5); adj[3, 3] <- 0

## inputs (ie metadata files)
raw <- list.dirs("arcsidata/Raw", recursive = FALSE)
ord <- getOrder(raw)
inp <- paste0("arcsidata/Inputs/", basename(raw))
mtd <- sapply(raw, function(i) {
  list.files(i, pattern = "metadata.xml$", full.names = TRUE)
})
xml <- lapply(mtd, xml2::read_xml)
dts <- lapply(xml, function(i) rapid_date(i))
tms <- lapply(seq(xml), function(i) {
  as.POSIXct(paste(dts[[i]], "00:00:00")) + rapid_hour(xml[[i]]) * 3600
})
scl <- sapply(xml, function(i) 1 / round(unique(rapid_sf(i)), 2L))

## digital elevation models per rapideye tile
dem_utm <- raster("dem/dem_srtm_01_utm.tif")
cid <- sapply(strsplit(basename(raw), "_"), "[[", 1)

dms <- lapply(unique(cid), function(h) {
  dr <- raw[grep(h, raw)][1]
  fl <- list.files(dr, pattern = paste0(getOrder(dr), ".tif"), full.names = TRUE)
  nm <- paste0(gsub(".tif$", "_", attr(dem_utm@file, "name")), h, ".tif")
  
  if (file.exists(nm)) {
    raster(nm)
  } else {
    b1 <- raster(fl)
    resample(dem_utm, b1, filename = nm)
  }
}); names(dms) <- unique(cid)

## loop over scenes
for (h in 1:length(mtd)) {
  
  ## status message
  cat("Image", raw[h], "is in, start processing.\n")

  ## custom shadow mask
  bds <- list.files(raw[h], paste0(ord[h], ".tif$"), full.names = TRUE)
  udm <- raster(list.files(raw[h], pattern = "udm.tif$", full.names = TRUE))

  # if (any(udm[] != 0)) {
  #   fns <- gsub(".tif$", "_shadowfree.tif", bds)
  #   shw <- rapid_shadows(bds, adj, limit = 0.2, filename = fns)
  # }
  
  ## custom cloud mask (https://github.com/CONABIO/rapideye-cloud-detection)
  cmd <- paste("cd /home/fdetsch/repo/rapideye-cloud-detection/;", 
               "docker run -i -v", 
               paste0(getwd(), "/", raw[h], "/:/rapideye/"), 
               "-v $(pwd):/data rapideye-clouds", 
               "python main.py /rapideye/")

  system(cmd)

  # msk <- raster(list.files(raw[h], pattern = "cloud.tif$", full.names = TRUE))
  
  for (i in c("local.png$", "toa.tif$"))
    jnk <- file.remove(list.files(raw[h], pattern = i, full.names = TRUE))

  # fnc <- gsub("cloud", "cloudfree", attr(msk@file, "name"))
  # cld <- overlay(brick(bds), msk, fun = function(x, y) {
  #   x[y[] == 4] <- NA
  #   return(x)
  # }, filename = fnc, overwrite = TRUE)

  # ## built-in unusable data mask
  # if (!dir.exists(inp[h])) dir.create(inp[h])
  # fnu <- paste(inp[h], basename(bds), sep = "/")
  # qcl <- rapid_qc(cld, udm, directions = adj, filename = fnu, overwrite = TRUE)
  # rm(cld); jnk <- file.remove(fnc)
  
  # ## custom shadow mask (dx.doi.org/10.6029/smartcr.2014.01.003)
  # shw <- rgbShadowMask(cld[[3:1]], n = 1L)
  # 
  # fns <- paste(drs[h], basename(bds), sep = "/")
  # lgt <- overlay(cld, shw, fun = function(x, y) {
  #   x[y[] == 0] <- NA
  #   return(x)
  # }, filename = fns, overwrite = TRUE)
  # jnk <- file.remove(fnc); rm(cld)
  
  ## digital elevation model
  dem <- dms[[grep(cid[h], names(dms))]]

  ## aerosol optical depth (aod; which modis overpass is closer to rapideye?), 
  ## atmospheric water (wct) and ozone content (oct)
  prj <- projectExtent(dem, crs = "+init=epsg:4326")
  pry <- as(extent(prj), "SpatialPolygons")
  proj4string(pry) <- proj4string(dem)
  atm <- sapply(c("Aerosol_Optical_Depth", "Water_Vapor", "Total_Ozone"), 
                function(z) {

    mod_all <- list.files("modis", full.names = TRUE, pattern = paste0(z, ".tif$"), 
                          recursive = TRUE)
    isc <- sapply(mod_all, function(w) {
      rst <- raster(w) 
      spy <- as(extent(rst), "SpatialPolygons")
      proj4string(spy) <- proj4string(pry)
      rgeos::gIntersects(spy, pry)
    })
    mod_all <- mod_all[isc]
    
    mod_dts <- mod_all[grep(format(dts[[h]], "%Y%j"), mod_all)]
  
    
    val <- sapply(mod_dts, function(i) {
      tmp <- try(weightedAverage(i, prj, snap = "out"), silent = TRUE)
      if (inherits(tmp, "try-error")) return(NA) else return(tmp)
    })
    
    if (z == "Aerosol_Optical_Depth") {
      out <- mean(val, na.rm = TRUE)
    } else {
      dfs <- abs(difftime(tms[[h]], getSwathDateTime(mod_dts)))
      out <- NA
      for (y in val[order(dfs)]) {
        out <- y
        if (!is.na(out)) break
      }
    }
    
    ## if no value for the respective date is available, retrieve long-term mean
    if (is.na(out)) {
      out <- mean(sapply(mod_all, function(i) {
        weightedAverage(i, prj, snap = "out")
      }), na.rm = TRUE)
    }

    if (z == "Total_Ozone") out <- out / 1000
    return(round(out, digits = 2L))
  
    # aod <- if (is.na(aod)) .01 else aod_vld[which.min(abs(aod - aod_vld))]
    # wct <- if (is.na(wct)) .01 else wct_vld[which.min(abs(wct - wct_vld))]
  })
  
  ## apply atmospheric correction
  # input <- gsub("Raw", "Inputs", mtd[h]); jnk <- file.copy(mtd[h], input)
  cmd <- paste("arcsi.py -s rapideye -f KEA -p SREF",
               "--aeropro Continental --atmospro Tropical",
               "--atmoswater", atm[2], "--atmosozone", atm[3],
               "--aot", atm[1], "--dem", attr(dem@file, "name"),
               "-i", mtd[h], "-t arcsidata/Temp -o arcsidata/Outputs")

  system(cmd)

  # tfs <- kea2tif("arcsidata/Outputs", overwrite = FALSE, keep_kea = TRUE)
  # fno <- attr(tfs@layers[[1]]@file, "name")
  # 
  # val <- qcl[[1]][]
  # ids <- is.na(val); val[ids] <- 0; val[!ids] <- 1
  # tmp <- setValues(qcl[[1]], val)
  # tfs <- overlay(tfs, tmp, fun = function(x, y) {
  #   x[y[] == 0] <- NA
  #   return(x)
  # })
  # 
  # for (v in c(".kea$", ".xml$"))
  #   file.remove(list.files("arcsidata/Outputs", pattern = v, full.names = TRUE))
  # 
  # writeRaster(tfs, fno, overwrite = TRUE)
}

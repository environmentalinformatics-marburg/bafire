### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## packages
.libPaths(c("/media/sd19006/data/users/fdetsch/R-Server/R", .libPaths()))

lib <- c("RStoolbox", "rapidr", "Rsenal", "plotrix")
Orcs::loadPkgs(lib)

## functions
jnk <- sapply(c("R/kea2tif.R", "R/getInfo.R"), source)

## temporary raster folder
rasterOptions(tmpdir = "../../tmp")
jnk <- tmpDir()

## reset working directory
setwd("../../data/bale/DigitalGlobeFoundation/ImageryGrant")


### google maps -----

rgb <- brick("../../aerials/dsm/bale-0_utm.tif")
ref <- as(extent(rgb), "SpatialPolygons"); proj4string(ref) <- projection(rgb)
clusterExport(cl, c("rgb", "ref"))
              
drs <- dir("arcsidata/Raw", full.names = TRUE)
drs <- drs[grep("PAN$", drs)]

for (i in drs) {
  fls <- list.files(i, pattern = ".TIF$", full.names = TRUE)
  
  for (j in fls) {
    cat("File", j, "is in, start processing...\n")
    ptt <- substr(strsplit(basename(j), "-")[[1]][[2]], 1, 4)
    nms <- paste0("../../aerials/dsm/psh/", gsub(ptt, "PANS", basename(j)))
    nms <- gsub(".TIF$", ".tif", nms)

    if (file.exists(nms)) {
      brick(nms)
    } else {
      rst <- raster(j); rst[rst[] == 0] <- NA
      spy <- as(extent(rst), "SpatialPolygons")
      proj4string(spy) <- projection(rst)
      
      crp <- crop(rgb, rst, snap = "out")
      if (!rgeos::gCovers(ref, spy))
        rst <- crop(rst, crp, snap = "out")
      
      psh <- panSharpen(crp, rst, r = 1, g = 2, b = 3)
      writeRaster(psh, filename = nms, datatype = "INT2U")
    }
  }
}


### rapideye -----

## import rapideye tifs and tile extents
rpd_fls <- list.files("rapideye/Outputs", pattern = "srefdem.tif$", full.names = TRUE)
rpd_tfs <- kea2tif("rapideye/Outputs")
rpd_spy <- lapply(rpd_tfs, function(i) {
  spy <- as(extent(i), "SpatialPolygons")
  proj4string(spy) <- CRS("+init=epsg:32637")
  return(spy)
})

## extract rapideye dates
raw <- list.dirs("rapideye/Raw", recursive = FALSE)
mtd <- sapply(raw, function(i) {
  list.files(i, pattern = "metadata.xml$", full.names = TRUE)
})
xml <- lapply(mtd, xml2::read_xml)
rpd_dts <- do.call("c", lapply(xml, rapid_date))

## rearrange tifs (by date) according to raw scenes (by tile id)
nfo_raw <- do.call("rbind", lapply(1:length(raw), function(i) {
  data.frame(file = raw[i]
             , tid = sapply(strsplit(basename(raw[i]), "_"), "[[", 1)
             , oid = getOrder(raw[i])
             , date = rapid_date(xml[[i]]))
}))

nfo_tfs <- do.call("rbind", lapply(rpd_tfs, function(i) {
  fl <- sapply(strsplit(names(i)[1], "\\."), "[[", 1)
  data.frame(file = fl
             , tid = gsub("tid", "", sapply(strsplit(fl, "_"), "[[", 4))
             , oid = gsub("oid", "", sapply(strsplit(fl, "_"), "[[", 5))
             , date = as.Date(sapply(strsplit(fl, "_"), "[[", 2), "%Y%m%d"))
}))

srt <- sapply(1:nrow(nfo_raw), function(i) {
  which(nfo_tfs[, "date"] == nfo_raw[i, "date"] & 
          nfo_tfs[, "tid"] == nfo_raw[i, "tid"] & 
          nfo_tfs[, "oid"] == nfo_raw[i, "oid"])
})
rpd_tfs <- rpd_tfs[srt]; rpd_spy <- rpd_spy[srt]; rpd_fls <- rpd_fls[srt]

## set current locale to us standard
lcl <- Sys.getlocale(category = "LC_TIME")
gimms:::setLocale()

## import and loop over available worldview-1 scenes
drs <- list.dirs("arcsidata/Raw", recursive = FALSE)

for (g in drs) {
  
  ## import and loop over files per worldview-1 scene
  fls <- list.files(g, pattern = ".TIF$", full.names = TRUE)

  for (h in fls) {
    
    ## status message
    cat("File", basename(h), "is in, start processing...\n")
    
    # fls_out <- paste0("rapideye/panSharpen/", gsub("P2AS", "RE3A", basename(h)))
    # if (file.exists(gsub(".TIF$", ".tif", fls_out))) next
    
    ## extract worldview-1 dates
    wv1_dts <- as.Date(tolower(substr(basename(h), 1, 7)), format = "%y%b%d")
    
    ## identify rapideye tiles fully covering current worldview image
    rst <- raster(h); msk <- calc(rst, function(x) x == 0)
    rst <- overlay(rst, msk, fun = function(x, y) {
      x[y == 1] <- NA
      return(x)
    }); rst <- trim(rst)
    ext <- extent(rst)
    ext <- as(ext, "SpatialPolygons")
    proj4string(ext) <- CRS("+init=epsg:32637")
    
    ids_cov <- sapply(rpd_spy, function(i) rgeos::gCovers(i, ext))

    rpd_cdt <- if (all(!ids_cov)) {
      ids_int <- which(sapply(rpd_spy, function(i) rgeos::gIntersects(i, ext)))
      rpd_int <- split(ids_int, as.character(nfo_raw[ids_int, "tid"]))
      
      fls_out <- paste0("rapideye/panSharpen/", 
                        gsub("P2AS", "RE3A", Orcs::pureBasename(h)))
      fls_out <- paste0(paste(fls_out, seq(length(rpd_int)), sep = "-"), ".tif")
      
      rpd_rst <- lapply(rpd_int, function(i) {
        ids_cdt <- which.min(abs(wv1_dts - rpd_dts[i]))
        rpd_tfs[[match(names(ids_cdt), nfo_raw$file)]]
      })
      
      rpd_psh <- lapply(seq(length(rpd_rst)), function(i) {
        if (file.exists(fls_out[i])) {
          brick(fls_out[i])
        } else {
          rpd_crp <- crop(rpd_rst[[i]], ext, snap = "out")
          rst_crp <- crop(rst, rpd_crp, snap = "out")
          psh <- panSharpen(rpd_crp, rst_crp, r = 3, g = 2, b = 1)
          writeRaster(psh, fls_out[i], datatype = "INT2S")
        }
      })
    } else {
      
      ## select temporally closest rapideye image and perform pansharpening
      ids_cdt <- which.min(abs(wv1_dts - rpd_dts[ids_cov]))
      tfs <- rpd_tfs[[match(names(ids_cdt), nfo_raw$file)]]

      fls_out <- paste0("rapideye/panSharpen/", gsub("P2AS", "RE3A", basename(h)))
      fls_out <- gsub(".TIF$", ".tif", fls_out)
      if (file.exists(fls_out)) {
        brick(fls_out)
      } else {
        rpd_crp <- crop(tfs, ext, snap = "out")
        psh <- panSharpen(rpd_crp, rst, r = 3, g = 2, b = 1)
        writeRaster(psh, fls_out, datatype = "INT2S")
      }
    }
  }
}

gimms:::setLocale(reset = TRUE, locale = lcl)

reView <- function(cell, date, dsn_rpd = ".", dsn_mds = ".") {
  
  ## load required function
  source("R/getInfo.R")
  
  ## if required, convert 'date' object to character
  if (class(date) == "Date")
    date <- as.character(date)
  
  ## import fire information
  dat <- read.csv("inst/extdata/modis_fires__rapideye_files.csv", 
                  stringsAsFactors = FALSE)
  dat <- subset(dat, CellID == cell & Date == date)
  
  ## if no valid cell/date combination is provided, throw error
  if (nrow(dat) == 0)
    stop("Valid CellID/Date combination required.\n")
  
  rst_mds <- raster::raster(paste0(dsn_mds, "/", dat$File))
  rst_mds <- raster::projectRaster(rst_mds, crs = "+init=epsg:32637", method = "ngb")
  rst_mds[][-dat$CellID] <- NA
  spy_mds <- raster::rasterToPolygons(rst_mds)
  
  lst_rpd <- lapply(c("FileREbefore", "FileREafter"), function(i) {
    rst_rpd <- raster::stack(paste0(dsn_rpd, "/", dat[, i]))[[2:4]]
    raster::crop(rst_rpd, spy_mds, snap = "out")
  })

  lbl1 <- getAcquisitionDate(dat$FileREbefore)
  lbl2 <- getAcquisitionDate(dat$FileREafter)
  mapview::slideView(lst_rpd[[1]], lst_rpd[[2]], label1 = lbl1, label2 = lbl2)
}

dat <- read.csv("inst/extdata/modis_fires__rapideye_files.csv")
lapply(seq(610, 700, 10), function(i) {
reView(cell = dat$CellID[i], date = dat$Date[i], 
       dsn_rpd = "E:/work/projects/bale/rapideye/data/qc", 
       dsn_mds = "data/MCD14A1.006")
})
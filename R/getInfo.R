getInfo <- function(x) {
  
  ## remove file extension (usually one of '.tif', '.txt', .xml', or nothing)
  x <- strsplit(basename(x), "\\.")
  x <- sapply(x, "[[", 1)
  
  ## extract information from filename
  tiles <- getTileID(x)
  dates <- getAcquisitionDate(x)
  satellites <- getSatellite(x)
  levels <- getProcessingLevel(x)
  orders <- getOrder(x) 
  
  ## return list
  list("Tile ID" = tiles, 
       "Acquisition Date" = dates, 
       "Satellite" = satellites, 
       "Processing Level" = levels, 
       "Order No." = orders)
}

getTileID <- function(x) {
  x <- strsplit(basename(x), "_")
  sapply(x, "[[", 1)
}

getAcquisitionDate <- function(x) {
  x <- strsplit(basename(x), "_")
  sapply(x, "[[", 2)
}

getSatellite <- function(x) {
  x <- strsplit(basename(x), "_")
  sapply(x, "[[", 3)
}

getProcessingLevel <- function(x) {
  x <- strsplit(basename(x), "_")
  sapply(x, "[[", 4)
}

getOrder <- function(x) {
  x <- strsplit(basename(x), "_")
  gsub(".tif", "", sapply(x, "[[", 5))
}
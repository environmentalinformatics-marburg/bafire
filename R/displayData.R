## import yearly fires
library(raster)
fls <- list.files("data/MCD14A1.006/agg1yr", pattern = ".tif$", full.names = TRUE)
rst <- stack(fls)

## display fires, one layer per year
# devtools::install_github("environmentalinformatics-marburg/mapview", ref = "develop")
library(mapview)
cols <- RColorBrewer::brewer.pal(5, "YlOrRd")
mapview(rst, at = seq(0.5, 5.5, 1), col.regions = "red", legend = FALSE)

## display fires, one layer for all years
library(foreach)
rst_yrs <- rst
jnk <- foreach(i = 1:nlayers(rst), j = 2000:2016) %do% {
  rst_yrs[[i]][rst_yrs[[i]][] > 0] <- j
}

# devtools::install_github("environmentalinformatics-marburg/Rsenal")
library(Rsenal)
rst_agg <- overlay(rst_yrs, fun = function(...) max(..., na.rm = TRUE))
m1 <- mapview(rst_agg, col.regions = envinmrPalette(length(unique(rst_agg[])) - 1), 
              at = seq(1999.5, 2016.5, 1), map.types = "OpenTopoMap")

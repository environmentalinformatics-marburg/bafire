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

rgb <- kiliAerial(template = rst_agg, projection = "+init=epsg:4326")
tmp <- rst_agg; tmp[tmp[] == 0] <- NA
spy_agg <- rasterToPolygons(rst_agg)
p1 <- spplot(spy_agg, col = NA, alpha.regions = .6, scales = list(draw = TRUE), 
             sp.layout = rgb2spLayout(rgb, quantiles = c(0, 1), alpha = .6), 
             at = seq(1999.5, 2016.5, 1), main = "Year of Last Fire Incident")

library(grid)
tiff("../../work/projects/bale/events/workshop_oct2016/presentations/subproject/img/fires_modis.tiff", 
     width = 20, height = 18, units = "cm", res = 300, compression = "lzw")
grid.newpage()
print(p1, newpage = FALSE)
dev.off()


### highlight recurrent fires -----

## extract raster values 
mat <- as.matrix(rst)

## loop over available raster layers (i.e., years)
lst <- lapply(1:nlayers(rst), function(i) {
  
  # create base fire map
  m0 <- mapview(rst[[i]], map.types = "OpenTopoMap", 
                at = seq(.5, max(maxValue(rst) + .5)), col.regions = rainbow(5))
  
  # check if fire cells were registered during current year
  vls <- getValues(rst[[i]])
  cls <- which(vls > 0)
  
  # if so, check if in the detected fire cells, fires also occurred during the 
  # following years
  if (length(cls) > 0 & i != ncol(mat)) {
    ssq <- sapply(cls, function(j) {
      any(mat[j, (i+1):ncol(mat)] > 0)
    })
    
    # if so, add black margins around these particular cells
    cls <- cls[ssq]
    tmp <- rst[[i]]
    tmp[][-cls] <- NA
    shp <- rasterToPolygons(tmp)
    
    m1 <- mapview(shp, map.types = "OpenTopoMap", color = "black")
    m0 <- m1 + m0
  }
  
  return(m0)
})

## display data (e.g., from 2012; all cells that suffered from fire disturbance 
## during the following years have a black border)
lst[[13]]

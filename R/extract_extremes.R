setwd("/media/permanent/data/bale/")
library(raster)
path = "rapideye/3742217_2009-11-26_RE1_3A_627058/"
image_path = paste0(path, "3742217_2009-11-26_RE1_3A_627058.tif")
meta_path = paste0(path, "3742217_2009-11-26_RE1_3A_627058_metadata.xml")
original_info = brick(image_path)
source("../../repo/bafire/R/rapid_rad.R")
radiance = rapid_rad(original_info, meta_path)
top_of_atmosphere_data = rapid_ref(radiance, meta_path, digits = 2, dn = FALSE)

steps = 1000
center = 50
xtiles = ytiles = 5000 / steps
counter = 0; b = 0
global_quant = list()

## subtiles
ys <- seq(ymax(top_of_atmosphere_data), ymin(top_of_atmosphere_data), -5000)
xs <- seq(xmin(top_of_atmosphere_data), xmax(top_of_atmosphere_data), 5000)

for (ycount in 2:length(ys)) {
  for (xcount in 2:length(xs)) {
    ext <- extent(xs[xcount-1], xs[xcount], ys[ycount], ys[ycount-1])
    sub <- crop(top_of_atmosphere_data, ext)
    
    x = ncol(sub); y = nrow(sub); z = nlayers(sub)
    rgb = reclassify(sub, matrix(c(0, 100, 0), ncol = 3), include.lowest = TRUE)
    for (i in 1:z) {
      
    }
  }
}

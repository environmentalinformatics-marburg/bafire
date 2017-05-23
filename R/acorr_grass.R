### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## working directory
setwd("/media/XChange/bale/DigitalGlobeFoundation/ImageryGrant")

## packages
# devtools::install_github("environmentalinformatics-marburg/reset")
# devtools::install_github("italocegatta/rapidr")
# devtools::install_github("fdetsch/Orcs")
lib <- c("parallel", "reset", "rapidr", "rgrass7", "Orcs")
Orcs::loadPkgs(lib)

## functions
bfr <- "/home/fdetsch/repo/bafire/"
jnk <- sapply(c("getInfo", "rapid_qc", "rapid_rad", "rapid_ref", 
                "weightedAverage"), function(i) {
  source(paste0(bfr, "R/", i, ".R"))
})

## raster options, write format used for temporary files and readable from GRASS
jnk <- capture.output(opt <- rasterOptions())
rasterOptions(format = "HFA")

## grass-gis initialization parameters
os <- Sys.info()[["sysname"]]
gisBase <- system("grass72 --config path", intern = TRUE) # works on windows?
gisDbase <- "/media/fdetsch/data/bale/grassdata/"

# ## parallelization
# cl <- makePSOCKcluster(detectCores() * .75)
# clusterExport(cl, "bfr")
# jnk <- clusterEvalQ(cl, source(paste0(bfr, "R/", i, ".R")))


### extraction -----

## list available file containers
ctn <- list.files("rapideye", pattern = "^getProduct.*streaming=True$",
                  full.names = TRUE)

## loop over containers and extract required files
lst <- parLapply(cl, ctn, function(i) {
  fls <- unzip(i, list = TRUE)$Name
  ord <- unique(getOrder(fls))
  ids <- sapply(c(paste0(c(ord, "udm"), ".tif$"), "metadata.xml"),
                function(j) grep(j, fls))
  unzip(i, files = fls[ids], exdir = dirname(i), overwrite = FALSE)
})

drs <- list.dirs("/media/fdetsch/data/bale/arcsidata/Raw", recursive = FALSE)
lst <- lapply(drs, function(i) {
  fls <- list.files(i, full.names = TRUE)
  ord <- unique(getOrder(fls))
  ids <- sapply(c(paste0(c(ord, "udm"), ".tif$"), "metadata.xml"),
                function(j) grep(j, fls))
  fls[ids]
})


### initialize grass session -----

## initialize new location and baseline mapset with required epsg code
initGRASS(gisBase = gisBase, home = raster::tmpDir(), 
          gisDbase = gisDbase, location = "bale_utm", 
          mapset = "PERMANENT", override = TRUE)

execGRASS("g.proj", flags = "c", epsg = 32637)


### quality control -----

out <- vector("list", length(lst)); n <- 1L
for (h in lst) {
  
  ## status message
  cat("Image", n, "of", length(lst), paste0("(", pureBasename(h[1]), ")"), 
      "is in, start processing.\n")
  
  ## import rapideye bands, unusable data mask, and metadata (file order in each 
  ## list entry results from extraction above)
  bds <- brick(h[1])
  qty <- raster(h[2])
  mtd <- xml2::read_xml(h[3])
  
  # ## perform quality control with 2-pixel buffer around each unusable cell
  # mat <- matrix(rep(1, 25), ncol = 5); mat[3, 3] <- 0
  # qcl <- rapid_qc(bds, qty, directions = mat)
  
  
  ### top-of-atmosphere (toa) reflectances -----

  rfl <- rapid_ref(bds, mtd)

  
  ### atmospheric correction (see 
  ### https://grasswiki.osgeo.org/wiki/Atmospheric_correction) -----
  
  ## geometrical conditions
  re_dt <- rapid_date(mtd); re_hr <- rapid_hour(mtd); re_ll <- rapid_latlon(mtd)
  
  geo <- paste(format(re_dt, "%m %d")                         # month and day
               , round(re_hr, 2)                              # decimal hours
               , paste(rev(round(re_ll, 3)), collapse = " ")) # coordinates
  
  ## mean elevation (tile-based information could be stored in separate images)
  dem <- list.files("/media/fdetsch/data/bale/dem", full.names = TRUE,
                    pattern = sapply(strsplit(basename(h[1]), "_"), "[[", 1))
  dem <- raster(dem)
  ele <- round(mean(dem[]) / 1000 * (-1), digits = 3)
  
  ## aerosol optical depth (aod)
  prj <- projectExtent(dem, crs = "+init=epsg:4326")
  pry <- as(extent(prj), "SpatialPolygons")
  proj4string(pry) <- proj4string(dem)
  mod_all <- list.files("/media/fdetsch/data/bale/modis", full.names = TRUE, 
                        recursive = TRUE,
                        pattern = "Aerosol_Optical_Depth_Land_Ocean.tif$")
  
  isc <- sapply(mod_all, function(w) {
    rst <- raster(w) 
    spy <- as(extent(rst), "SpatialPolygons")
    proj4string(spy) <- proj4string(pry)
    rgeos::gIntersects(spy, pry)
  })
  mod_all <- mod_all[isc]
                  
  mod_dts <- mod_all[grep(format(re_dt, "%Y%j"), mod_all)]
                
  val <- sapply(mod_dts, function(i) {
    tmp <- try(weightedAverage(i, prj, snap = "out"), silent = TRUE)
    if (inherits(tmp, "try-error")) return(NA) else return(tmp)
  })
                  
  out <- mean(val, na.rm = TRUE)

  ## if no value for the respective date is available, retrieve long-term mean
  if (is.na(out)) {
    out <- mean(sapply(mod_all, function(i) {
      weightedAverage(i, prj, snap = "out")
    }), na.rm = TRUE)
  }
                  
  aod <- round(out, digits = 2L)

  ## loop over single bands
  atc <- vector("list", nlayers(bds))
  for (i in 1:nlayers(rfl)) {
    
    ## band code
    bnd <- switch(as.character(i), "1" = 88, "2" = 89, "3" = 90, "4" = 91, "5" = 92)
    
    ## create parameter file
    prm <- c(13      # sensor, 13 = "rapideye"
             , geo   # geometrical conditions 
             , 1     # atmospheric model, 1 = "tropical"
             , 1     # aerosols model, 1 = "continental"
             , 0     # visibility, 0 = use aod (next line)
             , aod   # aod
             , ele   # mean elevation
             , -1000 # sensor height, -1000 = sensor onboard a satellite
             , bnd   # satellite band, 88 (:92) = rapideye blue (to nir) band
    )
    
    fls_prm <- gsub(".tif$", "_param.txt", h[1])
    writeLines(prm, fls_prm)
    
    ## during first run, initialize new mapset inheriting projection info
    if (i == 1) {
      execGRASS("g.mapset", flags = "c", mapset = pureBasename(h[1]))
      
      ## assign spatial settings (extent, number of rows and columns, resolution)  
      ## from resampled digital elevation model to current geographic region
      execGRASS("r.external", flags = "overwrite",
                parameters = list(input = attr(dem@file, "name"), 
                                  output = "dem"))
      
      execGRASS("g.region", raster = "dem")
    }
    
    ## import current rapideye band and perform atmospheric correction
    execGRASS("r.external", flags = "overwrite", 
              parameters = list(input = attr(bds@file, "name"), 
                                band = i, output = paste0("bnd-", i)))
    
    execGRASS("i.atcorr", flags = c("r", "overwrite"), 
              parameters = list(input = paste0("bnd-", i), elevation = "dem", 
                                parameters = fls_prm, 
                                # range = c(0, 1), rescale = c(0, 1), 
                                output = paste0("atc-", i)))
    jnk <- file.remove(fls_prm)
    
    ## write image to disk  
    atc[[i]] <- raster(readRAST(paste0("atc-", i)))
    # fls_atc <- gsub(".tif$", paste0("_ATC-", i, ".tif"), h[1])
    # execGRASS("r.out.gdal", flags = "overwrite", 
    #           parameters = list(input = "atc", output = fls_atc, 
    #                             format = "GTiff"))
    # 
    # atc[[i]] <- raster(fls_atc)
  }
  
  out[[n]] <- stack(atc); n <- n + 1L
}

## deregister parallel backend
stopCluster(cl)
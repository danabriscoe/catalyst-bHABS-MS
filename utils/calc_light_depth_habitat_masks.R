## calc_light_depth_habitat_masks.R


## ============================================================
## Gambierdiscus polynesiensis: light-limited habitat mask
##
## Inputs:
##   1. Bio-ORACLE KdPAR (historical)
##   2. Bio-ORACLE surface PAR (historical)
##   3. GEBCO bathymetry (western Pacific)
##   4. GEBCO bathymetry (eastern Pacific)
##
## Outputs:
##   - Maximum light-supported depth
##   - Fraction shallow benthic habitat
##   - Fraction light-suitable benthic habitat
##   - Final habitat mask
##
## Longitude convention: 0-360
## ============================================================


## Libraries ----
library(terra)
library(here)
library(glue)
library(ggplot2)

terraOptions(
  memfrac = 0.5,
  progress = 1
)


## Parameters ----
params <- list(
  
  ## Bathymetry
  max_depth = 200,
  
  ## Minimum light supporting growth
  min_irradiance = 10,   # umol photons m-2 s-1
  
  ## Assumed daily daylight duration
  daylight_hours = 12,
  
  ## Minimum suitable habitat fraction for display
  min_habitat_fraction = 0.001,
  
  ## Geographic extent
  xmin = 120,
  xmax = 260,
  ymin = -60,
  ymax = 60,
  
  ## Processing tile width/height (degrees)
  tile_size = 10
  
)


## Input file paths ----
# UPDATE THESE FILE PATHS

kd_file <- "~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/biooracle_kdpar/kdpar_mean_baseline_2000_2020_lon360.nc"

par_file <- "~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/biooracle_par/par_mean_baseline_2000_2020_lon360.nc"

gebco_west_file <- "~/Downloads/GEBCO_16_Nov_2026_pac/gebco_2026_n60.0_s-60.0_w179.0_e260.0.nc"

gebco_east_file <- "~/Downloads/GEBCO_16_Nov_2026_pac/gebco_2026_n60.0_s-60.0_w120.0_e179.0.nc"


## Output paths ----
out_dir <- here(
  "data",
  "habitat_masks",
  "light_depth"
)

tile_dir <- file.path(out_dir, "interim_tiles")

dir.create(
  tile_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


## Basic input checks ----
input_files <- c(
  kd_file,
  par_file,
  gebco_west_file,
  gebco_east_file
)

if (!all(file.exists(input_files))) {
  stop(
    "One or more input NetCDF files do not exist:\n",
    paste(input_files[!file.exists(input_files)],
          collapse = "\n")
  )
}


## Helper functions ----

## Convert negative longitude rasters to 0-360
to_lon360 <- function(r) {
  
  if (xmin(r) < 0) {
    r <- rotate(r)
  }
  
  return(r)
  
}


## Load a named NetCDF variable
load_nc_var <- function(file, var_name) {
  
  r <- rast(
    file,
    subds = var_name
  )
  
  message(
    glue(
      "Loaded {var_name}: {nlyr(r)} layer(s)"
    )
  )
  
  return(r)
  
}


## Load Bio-ORACLE light data ----

## Diffuse attenuation
kd <- load_nc_var(
  kd_file,
  "kdpar_mean_mean"
)

## Surface PAR
par <- load_nc_var(
  par_file,
  "par_mean_mean"
)


## Average historical time slices ----
# Bio-ORACLE baseline contains two decadal layers.
# Calculate the mean to create one historical climatology.

kd <- mean(
  kd,
  na.rm = TRUE
)

par <- mean(
  par,
  na.rm = TRUE
)

names(kd) <- "kdpar_hist"
names(par) <- "par_hist"


## Convert longitude ----
kd <- to_lon360(kd)

par <- to_lon360(par)


## Crop to study extent ----
bbox <- ext(
  params$xmin,
  params$xmax,
  params$ymin,
  params$ymax
)

kd <- crop(kd, bbox)

par <- crop(par, bbox)


## Align PAR to KdPAR grid ----
if (!compareGeom(
  kd,
  par,
  stopOnError = FALSE
)) {
  
  par <- resample(
    par,
    kd,
    method = "bilinear"
  )
  
}


## Remove invalid values ----
kd <- ifel(
  kd > 0,
  kd,
  NA
)

par <- ifel(
  par > 0,
  par,
  NA
)


## Calculate surface irradiance ----
# Bio-ORACLE PAR:
# mol photons m-2 day-1
#
# Convert to mean daylight irradiance:
# umol photons m-2 s-1
#
# NOTE:
# daylight_hours = 12 is an approximation.
# This should be tested with alternative durations.

daylight_seconds <- params$daylight_hours * 3600

par_surface <- (
  par * 1e6
) / daylight_seconds

names(par_surface) <- "par_surface_umol"


## Calculate maximum light-supported depth ----
# Beer-Lambert:
#
# I(z) = I(0) * exp(-Kd * z)
#
# Dmax = ln(I(0) / Ic) / Kd
#
# Ic = minimum growth irradiance

z_light <- log(
  par_surface / params$min_irradiance
) / kd

## Do not allow negative light depths
z_light <- ifel(
  z_light > 0,
  z_light,
  0
)

names(z_light) <- "max_light_depth"


## Save light-depth rasters ----
writeRaster(
  kd,
  file.path(out_dir, "kdpar_historical.nc"),
  overwrite = TRUE
)

writeRaster(
  par_surface,
  file.path(out_dir, "par_surface_umol.nc"),
  overwrite = TRUE
)

writeRaster(
  z_light,
  file.path(out_dir, "max_light_depth.nc"),
  overwrite = TRUE
)


## Load GEBCO bathymetry ----
# GEBCO files already cover:
# 120-179 E and 179-260 E.
#
# # Create a virtual mosaic without writing a huge
# # merged bathymetry file.
# 
# gebco_files <- c(
#   gebco_west_file,
#   gebco_east_file
# )
# 
# bathy_vrt_file <- file.path(
#   out_dir,
#   "gebco_pacific.vrt"
# )
# 
# vrt(
#   gebco_files,
#   filename = bathy_vrt_file,
#   overwrite = TRUE
# )
# 
# bathy <- rast(bathy_vrt_file)
# 
# ## Confirm extent and coordinate system
# print(bathy)
# print(ext(bathy))
# print(crs(bathy))


## Load GEBCO bathymetry ----

gebco_files <- normalizePath(
  path.expand(c(
    gebco_west_file,
    gebco_east_file
  )),
  mustWork = TRUE
)

## Explicitly specify NetCDF elevation variable ----
gebco_sources <- paste0(
  'NETCDF:"',
  gebco_files,
  '":elevation'
)

## Output VRT ----
dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

bathy_vrt_file <- file.path(
  out_dir,
  "gebco_pacific.vrt"
)

## Create virtual mosaic ----
bathy <- terra::vrt(
  gebco_sources,
  filename = bathy_vrt_file,
  overwrite = TRUE
)

## Verify ----
ext(bathy)
res(bathy)
names(bathy)

## GEBCO elevation is negative below sea level.
## We will convert it to positive ocean depth in
## each processing tile.


## Build habitat masks in geographic tiles ----
# Processing at GEBCO native resolution preserves
# small areas of shallow seabed.
#
# Each tile is aggregated to the Bio-ORACLE grid.
#
# Output layers:
#   1. Fraction shallow habitat (0-max_depth)
#   2. Fraction illuminated habitat
#   3. Fraction ocean area


## Tile boundaries ----
lon_breaks <- seq(
  params$xmin,
  params$xmax,
  by = params$tile_size
)

lat_breaks <- seq(
  params$ymin,
  params$ymax,
  by = params$tile_size
)


## Process one tile ----
process_habitat_tile <- function(
    lon1,
    lon2,
    lat1,
    lat2,
    tile_id
) {
  
  message(
    glue(
      "Processing tile {tile_id}: ",
      "{lon1}-{lon2} E, {lat1}-{lat2} N"
    )
  )
  
  tile_ext <- ext(
    lon1,
    lon2,
    lat1,
    lat2
  )
  
  ## Crop high-resolution bathymetry
  b <- crop(
    bathy,
    tile_ext,
    snap = "near"
  )
  
  ## Ocean depth (positive metres)
  depth <- ifel(
    b < 0,
    -b,
    NA
  )
  
  ## Crop light penetration raster
  z <- crop(
    z_light,
    tile_ext,
    snap = "near"
  )
  
  ## Resample light depth to bathymetry grid
  # Nearest neighbour avoids inventing subgrid
  # differences in oceanographic light conditions.
  
  z_hi <- resample(
    z,
    depth,
    method = "near"
  )
  
  
  ## Ocean indicator ----
  ocean <- ifel(
    b < 0,
    1,
    0
  )
  
  
  ## Shallow habitat indicator ----
  shallow <- ifel(
    !is.na(depth) &
      depth <= params$max_depth,
    1,
    0
  )
  
  
  ## Light-limited benthic habitat indicator ----
  # NA where light data are missing; distinguish
  # missing information from unsuitable habitat.
  
  illuminated <- ifel(
    is.na(z_hi),
    NA,
    ifel(
      !is.na(depth) &
        depth <= params$max_depth &
        depth <= z_hi,
      1,
      0
    )
  )
  
  
  ## Target Bio-ORACLE grid for this tile ----
  target <- crop(
    z_light,
    tile_ext,
    snap = "near"
  )
  
  
  ## Aggregate high-resolution indicators ----
  # average of binary values = area fraction
  # (approximately, for regular geographic cells).
  
  shallow_fraction_all <- resample(
    shallow,
    target,
    method = "average"
  )
  
  illuminated_fraction_all <- resample(
    illuminated,
    target,
    method = "average"
  )
  
  ocean_fraction <- resample(
    ocean,
    target,
    method = "average"
  )
  
  
  ## Fraction relative to ocean area ----
  # Avoid land area influencing the denominator.
  
  shallow_fraction <- ifel(
    ocean_fraction > 0,
    shallow_fraction_all / ocean_fraction,
    NA
  )
  
  illuminated_fraction <- ifel(
    ocean_fraction > 0,
    illuminated_fraction_all / ocean_fraction,
    NA
  )
  
  ## Limit floating-point rounding
  shallow_fraction <- clamp(
    shallow_fraction,
    0,
    1
  )
  
  illuminated_fraction <- clamp(
    illuminated_fraction,
    0,
    1
  )
  
  
  ## Combine tile outputs ----
  out <- c(
    shallow_fraction,
    illuminated_fraction,
    ocean_fraction
  )
  
  names(out) <- c(
    "shallow_fraction",
    "illuminated_fraction",
    "ocean_fraction"
  )
  
  
  ## Save tile ----
  tile_file <- file.path(
    tile_dir,
    glue("habitat_tile_{tile_id}.tif")
  )
  
  writeRaster(
    out,
    tile_file,
    overwrite = TRUE,
    datatype = "FLT4S"
  )
  
  return(tile_file)
  
}


## Run tile processing ----
tile_files <- character(0)

tile_i <- 0

for (i in seq_len(length(lon_breaks) - 1)) {
  
  for (j in seq_len(length(lat_breaks) - 1)) {
    
    tile_i <- tile_i + 1
    
    lon1 <- lon_breaks[i]
    lon2 <- lon_breaks[i + 1]
    
    lat1 <- lat_breaks[j]
    lat2 <- lat_breaks[j + 1]
    
    tile_id <- sprintf("%03d", tile_i)
    
    tile_file <- process_habitat_tile(
      lon1,
      lon2,
      lat1,
      lat2,
      tile_id
    )
    
    tile_files <- c(
      tile_files,
      tile_file
    )
    
  }
  
}


## Merge processed tiles ----
habitat <- merge(
  sprc(
    lapply(tile_files, rast)
  )
)

names(habitat) <- c(
  "shallow_fraction",
  "illuminated_fraction",
  "ocean_fraction"
)


## Extract habitat layers ----
shallow_fraction <- habitat[["shallow_fraction"]]

illuminated_fraction <- habitat[["illuminated_fraction"]]

ocean_fraction <- habitat[["ocean_fraction"]]


## Final binary habitat mask ----
# Minimum proportion of ocean area within each
# grid cell that is both shallow and illuminated.

habitat_mask <- ifel(
  illuminated_fraction >=
    params$min_habitat_fraction,
  1,
  NA
)

names(habitat_mask) <- "habitat_mask"


## Save final rasters ----
writeRaster(
  habitat,
  file.path(
    out_dir,
    "habitat_fractions_historical.nc"
  ),
  overwrite = TRUE
)
## upd - save as .tif not ncdf --
writeRaster(
  habitat,
  file.path(out_dir, "habitat_fractions_historical.tif"),
  overwrite = TRUE
)


writeRaster(
  habitat_mask,
  file.path(
    out_dir,
    "habitat_mask_historical.nc"
  ),
  overwrite = TRUE
)


## Quick diagnostic plots ----

## Maximum light-supported depth
plot(
  z_light,
  main = "Maximum light-supported depth (m)"
)

## Shallow habitat fraction
plot(
  shallow_fraction,
  main = "Fraction of shallow benthic habitat"
)

## Illuminated habitat fraction
plot(
  illuminated_fraction,
  main = "Fraction of illuminated benthic habitat"
)

## Final binary mask
# ## Final binary mask ----
# plot(
#   habitat_mask,
#   # col = "#E16462",    # Coral red
#   col = "#00BFC4",
#   colNA = "white",
#   legend = TRUE,
#   main = "Light-limited benthic habitat mask"
# )

library(viridisLite)

plot(
  illuminated_fraction,
  col = viridis(100, option = "viridis"),
  range = c(0, 1),
  colNA = "gray90",
  main = "Fraction of illuminated benthic habitat"
)

## Save diagnostic PNG ----
png(
  file.path(
    out_dir,
    "habitat_mask_historical.png"
  ),
  width = 1800,
  height = 1200,
  res = 180
)

plot(
  habitat_mask,
  main = "Potential light-limited benthic habitat"
)

dev.off()


## Summary ----
message("Habitat mask processing complete.")
message(glue("Outputs saved to: {out_dir}"))










# library(terra)
# 
# ## Load downloaded files ----
# west <- rast("~/Downloads/kdpar_mean_baseline_2000_2020_depthsurf_west.nc")
# east <- rast("~/Downloads/kdpar_mean_baseline_2000_2020_depthsurf_east.nc")
# 
# ## Shift eastern Pacific longitude to 0–360 ----
# east_360 <- terra::shift(east, dx = 360)
# 
# ## Merge both sections ----
# kdpar_360 <- terra::merge(west, east_360)
# 
# names(kdpar_360) <- "kdpar_mean"
# 
# ## Check extent ----
# ext(kdpar_360)
# res(kdpar_360)
# 
# ## Plot ----
# plot(kdpar_360)
# 
# ## Save merged NetCDF ----
# writeCDF(
#   kdpar_360,
#   filename = "kdpar_mean_baseline_2000_2020_lon360.nc",
#   varname = "kdpar_mean",
#   unit = "m-1",
#   overwrite = TRUE
# )
# 
# 
# 
# 
# 
# 
# 
# ###### ------------------------
# 
# library(terra)
# library(ncdf4)
# library(glue)
# 
# options(timeout = 3600)
# 
# ## Configuration ----
# dataset_id <- "kdpar_mean_baseline_2000_2020_depthsurf"
# variable   <- "kdpar_mean_mean"
# target_date <- "2010-01-01T00:00:00Z"
# 
# ## Output directory ----
# out_dir <- path.expand("~/Downloads/biooracle_kdpar")
# dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# 
# ## ERDDAP URL ----
# base_url <- glue(
#   "https://erddap.bio-oracle.org/erddap/griddap/{dataset_id}.nc"
# )
# 
# ## Download regions ----
# # Longitude values are cell centres.
# # The two sections meet at the 180-degree cell boundary.
# 
# regions <- list(
#   west = list(
#     lon1 = 120.025,
#     lon2 = 179.975,
#     xmin = 120,
#     xmax = 180,
#     ncol = 1200
#   ),
#   east = list(
#     lon1 = -179.975,
#     lon2 = -100.025,
#     xmin = -180,
#     xmax = -100,
#     ncol = 1600
#   )
# )
# 
# ## Check downloaded NetCDF coordinates ----
# check_ncdf <- function(file, region) {
#   
#   if (!file.exists(file)) return(FALSE)
#   
#   nc <- try(ncdf4::nc_open(file), silent = TRUE)
#   
#   if (inherits(nc, "try-error")) return(FALSE)
#   
#   on.exit(ncdf4::nc_close(nc))
#   
#   if (!variable %in% names(nc$var)) return(FALSE)
#   
#   lon <- ncdf4::ncvar_get(nc, "longitude")
#   lat <- ncdf4::ncvar_get(nc, "latitude")
#   
#   isTRUE(
#     length(lon) == region$ncol &&
#       length(lat) == 2400 &&
#       abs(min(lon) - region$lon1) < 0.001 &&
#       abs(max(lon) - region$lon2) < 0.001 &&
#       abs(min(lat) + 59.975) < 0.001 &&
#       abs(max(lat) - 59.975) < 0.001
#   )
# }
# 
# ## Download function ----
# download_region <- function(region_name) {
#   
#   region <- regions[[region_name]]
#   
#   file <- file.path(
#     out_dir,
#     glue("{variable}_{region_name}.nc")
#   )
#   
#   ## Retain valid existing downloads ----
#   if (check_ncdf(file, region)) {
#     message("Using existing file: ", file)
#     return(file)
#   }
#   
#   ## Remove incomplete or incorrect downloads ----
#   if (file.exists(file)) {
#     message("Existing file has incorrect extent. Re-downloading.")
#     unlink(file)
#   }
#   
#   ## Build ERDDAP query ----
#   query <- glue(
#     "{variable}",
#     "[({target_date})]",
#     "[(-59.975):1:(59.975)]",
#     "[({region$lon1}):1:({region$lon2})]"
#   )
#   
#   url <- paste0(
#     base_url,
#     "?",
#     utils::URLencode(query, reserved = TRUE)
#   )
#   
#   message("Downloading ", region_name, " section...")
#   
#   download.file(
#     url,
#     destfile = file,
#     mode = "wb",
#     method = "libcurl",
#     quiet = FALSE
#   )
#   
#   ## Verify coordinates ----
#   if (!check_ncdf(file, region)) {
#     stop("Downloaded file has incorrect dimensions or coordinates: ", file)
#   }
#   
#   message("Download verified: ", file)
#   
#   return(file)
# }
# 
# 
# ## Download both sections ----
# west_file <- download_region("west")
# east_file <- download_region("east")
# 
# 
# ## Load NetCDF rasters ----
# west <- rast(west_file, subds = variable)
# east <- rast(east_file, subds = variable)
# 
# ## Check dimensions ----
# stopifnot(
#   ncol(west) == 1200,
#   ncol(east) == 1600,
#   nrow(west) == 2400,
#   nrow(east) == 2400,
#   nlyr(west) == 1,
#   nlyr(east) == 1
# )
# 
# ## Set exact geographic extents ----
# # NetCDF longitude and latitude coordinates were
# # verified above. This avoids floating-point
# # and GDAL extent interpretation issues.
# 
# ext(west) <- ext(120, 180, -60, 60)
# ext(east) <- ext(-180, -100, -60, 60)
# 
# ## Shift eastern Pacific to 0–360 ----
# east_360 <- terra::shift(east, dx = 360)
# 
# ## Check individual extents ----
# print(ext(west))
# print(ext(east_360))
# 
# 
# ## Merge across the dateline ----
# # Temporarily remove CRS so that terra does not
# # attempt to wrap geographic longitudes.
# 
# original_crs <- crs(west)
# 
# crs(west) <- ""
# crs(east_360) <- ""
# 
# kdpar_360 <- terra::merge(
#   west,
#   east_360,
#   first = TRUE
# )
# 
# ## Restore CRS ----
# crs(kdpar_360) <- original_crs
# 
# names(kdpar_360) <- "kdpar_mean"
# 
# 
# ## Verify merged raster ----
# print(ext(kdpar_360))
# print(dim(kdpar_360))
# print(res(kdpar_360))
# 
# stopifnot(
#   ncol(kdpar_360) == 2800,
#   nrow(kdpar_360) == 2400,
#   abs(xmin(kdpar_360) - 120) < 0.001,
#   abs(xmax(kdpar_360) - 260) < 0.001,
#   abs(ymin(kdpar_360) + 60) < 0.001,
#   abs(ymax(kdpar_360) - 60) < 0.001
# )
# 
# 
# ## Plot ----
# plot(
#   kdpar_360,
#   main = "Bio-ORACLE KdPAR (2000–2020)",
#   col = hcl.colors(100, "Viridis")
# )
# 
# 
# ## Save merged NetCDF ----
# out_file <- file.path(
#   out_dir,
#   "kdpar_mean_baseline_2000_2020_lon360.nc"
# )
# 
# terra::writeCDF(
#   kdpar_360,
#   filename = out_file,
#   varname = "kdpar_mean",
#   longname = "Mean diffuse attenuation coefficient of PAR",
#   unit = "m-1",
#   overwrite = TRUE
# )
# 
# message("Saved: ", out_file)
# 
# 
# ## Verify saved file ----
# kdpar_check <- rast(out_file)
# 
# print(ext(kdpar_check))
# print(dim(kdpar_check))

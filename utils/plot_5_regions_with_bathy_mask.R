# plot_5_regions_with_bathy_mask.R


## ============================================================
## Gambierdiscus polynesiensis
## Bathymetry-masked CTX predictions - Five regions
##
## Scenarios:
##   Historical GLORYS
##   SSP126
##   SSP585
##
## Regions:
##   1. Southern New Caledonia
##   2. GBR and southern Queensland
##   3. Southeastern Queensland / NSW
##   4. Northern New Zealand
##   5. Southern Japan / East China Sea
##
## Habitat:
##   GEBCO bathymetry, 0-200 m
##   No light constraint
##
## Prediction resolution: ~0.083 degrees
## Habitat resolution: 0.05 degrees
## Longitude convention: 0-360
##
## Plotting:
##   Optional bilinear upsampling for display
##   Sharp bathymetry mask boundaries
##   No ggplot raster interpolation
##
## ============================================================


## Libraries ----
library(terra)
library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(patchwork)
library(khroma)
library(dplyr)
library(here)
library(glue)


## ============================================================
## 1. Parameters
## ============================================================

params <- list(
  
  ## Prediction model ----
  pred_model = "CTX_rate",
  stat = "all_time_mean",
  
  ## Bathymetric habitat threshold ----
  ## Fraction of ocean area with seabed <= 200 m
  min_shallow_fraction = 0.001,
  
  ## CTX color scale ----
  ctx_limits = c(0, 0.62),
  
  ctx_label = "Total CTX production rate (ng/ml/day)",
  
  ## Plot smoothing ----
  ## TRUE: bilinear upsampling before masking
  ## FALSE: original prediction resolution
  
  plot_smooth = TRUE,
  
  ## Try 2 or 3
  smooth_factor = 3,
  
  ## Optional sampling points ----
  show_sites = TRUE,
  
  ## Figure settings ----
  figure_width = 15,
  figure_height = 20,
  figure_dpi = 300
  
)


## ============================================================
## 2. Define regions
## ============================================================

regions <- list(
  
  ## 1. Southern New Caledonia ----
  new_caledonia = list(
    label = "Southern New Caledonia",
    xmin = 164,
    xmax = 168.5,
    ymin = -24,
    ymax = -20
  ),
  
  ## 2. GBR and southern Queensland ----
  great_barrier_reef = list(
    label = "GBR and southern QLD",
    xmin = 145,
    xmax = 155,
    ymin = -30,
    ymax = -16
  ),
  
  ## 3. Southeastern Queensland / NSW ----
  nsw = list(
    label = "Southeastern QLD and NSW",
    xmin = 150,
    xmax = 155,
    ymin = -36,
    ymax = -27
  ),
  
  ## 4. Northern New Zealand ----
  north_nz = list(
    label = "Northern New Zealand",
    xmin = 172,
    xmax = 180,
    ymin = -39,
    ymax = -33
  ),
  
  ## 5. Southern Japan / East China Sea ----
  southern_japan = list(
    label = "Southern Japan Region",
    xmin = 125,
    xmax = 142,
    ymin = 27,
    ymax = 38
  )
  
)


## Scenarios ----
scenarios <- c(
  "histclim",
  "ssp126",
  "ssp585"
)

scenario_labels <- c(
  histclim = "Historical",
  ssp126 = "SSP126",
  ssp585 = "SSP585"
)


## Sampling sites ----
sites <- data.frame(
  
  region = c(
    "new_caledonia",
    "great_barrier_reef",
    "great_barrier_reef"
  ),
  
  site = c(
    "Noumea",
    "Orpheus Island",
    "Beaver Reef"
  ),
  
  lon = c(
    166.435806,
    146.500,
    146.479
  ),
  
  lat = c(
    -22.304028,
    -18.621,
    -17.853
  )
  
)


## ============================================================
## 3. File paths
## ============================================================

pred_dir <- here(
  "data",
  "preds",
  "expanded_lon"
)

habitat_file <- here(
  "data",
  "habitat_masks",
  "light_depth",
  "habitat_fractions_historical.nc"
)

out_dir <- here(
  "figures",
  "supplementary",
  "bathy_mask_5regions_smooth"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


## ============================================================
## 4. Load prediction rasters
## ============================================================

load_pred_ras <- function(
    scenario,
    pred_model = "CTX_rate",
    stat = "all_time_mean"
) {
  
  ## Filename ----
  if (scenario == "histclim") {
    
    filename <- glue(
      "preds_df_list_{pred_model}_histclim_glorys_{stat}_expanded_lon.rdata"
    )
    
  } else {
    
    filename <- glue(
      "preds_df_list_{pred_model}_{scenario}_gcm.rdata"
    )
    
  }
  
  
  fpath <- file.path(pred_dir, filename)
  
  if (!file.exists(fpath)) {
    stop(glue("Missing prediction file: {fpath}"))
  }
  
  
  ## Load into temporary environment ----
  env <- new.env(parent = globalenv())
  
  object_names <- load(
    fpath,
    envir = env
  )
  
  
  if (length(object_names) != 1) {
    stop(
      glue("Expected one object in {filename}.")
    )
  }
  
  
  x <- get(
    object_names[1],
    envir = env
  )
  
  
  ## Convert to terra raster ----
  if (inherits(x, "SpatRaster")) {
    
    r <- x
    
  } else {
    
    r <- terra::rast(
      raster::stack(x)
    )
    
  }
  
  
  ## Require one prediction layer ----
  if (terra::nlyr(r) != 1) {
    stop(
      glue(
        "{filename} contains {terra::nlyr(r)} layers."
      )
    )
  }
  
  
  ## Convert conventional -180/180 rasters
  ## to 0-360 longitude if necessary ----
  if (
    terra::xmin(r) < 0 &&
    terra::xmax(r) <= 180
  ) {
    
    r <- terra::rotate(r)
    
  }
  
  
  names(r) <- scenario
  
  message(glue("Loaded: {filename}"))
  
  return(r)
  
}


## Load historical and future predictions ----
preds <- list(
  
  histclim = load_pred_ras(
    "histclim",
    params$pred_model,
    params$stat
  ),
  
  ssp126 = load_pred_ras(
    "ssp126",
    params$pred_model
  ),
  
  ssp585 = load_pred_ras(
    "ssp585",
    params$pred_model
  )
  
)


## Check prediction geometry ----
lapply(preds, function(r) {
  
  list(
    extent = terra::ext(r),
    resolution = terra::res(r),
    names = names(r)
  )
  
})


## ============================================================
## 5. Load GEBCO-derived habitat fractions
## ============================================================

habitat <- terra::rast(habitat_file)

stopifnot(terra::nlyr(habitat) == 3)


## Restore names ----
names(habitat) <- c(
  "shallow_fraction",
  "illuminated_fraction",
  "ocean_fraction"
)


## Bathymetry only ----
shallow_fraction <- habitat[["shallow_fraction"]]

ocean_fraction <- habitat[["ocean_fraction"]]


## NOTE:
## illuminated_fraction is NOT used in this analysis.


## Check CRS ----
for (scenario in scenarios) {
  
  if (!terra::same.crs(
    preds[[scenario]],
    habitat
  )) {
    
    stop(glue("CRS mismatch: {scenario}"))
    
  }
  
}


## ============================================================
## 6. Bathymetry mask function
## ============================================================

get_bathy_mask <- function(
    target,
    shallow_fraction,
    ocean_fraction,
    min_fraction = 0.001
) {
  
  ## Crop source layers around target grid ----
  e <- terra::ext(target)
  
  source_ext <- terra::ext(
    terra::xmin(e) - 0.3,
    terra::xmax(e) + 0.3,
    terra::ymin(e) - 0.3,
    terra::ymax(e) + 0.3
  )
  
  
  shallow_src <- terra::crop(
    shallow_fraction,
    source_ext
  )
  
  ocean_src <- terra::crop(
    ocean_fraction,
    source_ext
  )
  
  
  ## Ocean fraction ----
  ocean_valid <- terra::ifel(
    is.na(ocean_src),
    0,
    ocean_src
  )
  
  
  ## Fraction of entire source cell containing
  ## shallow seabed ----
  ## Assumes shallow_fraction is relative to
  ## the ocean portion of the cell.
  
  shallow_area_src <- terra::ifel(
    !is.na(shallow_src) &
      !is.na(ocean_src),
    shallow_src * ocean_valid,
    0
  )
  
  
  ## Resample to prediction grid ----
  ocean_coarse <- terra::resample(
    ocean_valid,
    target,
    method = "average"
  )
  
  shallow_coarse <- terra::resample(
    shallow_area_src,
    target,
    method = "average"
  )
  
  
  ## Fraction of ocean area containing
  ## seabed depths <= 200 m ----
  shallow_prop <- terra::ifel(
    ocean_coarse > 0,
    shallow_coarse / ocean_coarse,
    NA
  )
  
  
  shallow_prop <- terra::clamp(
    shallow_prop,
    lower = 0,
    upper = 1
  )
  
  
  ## Binary bathymetry mask ----
  bathy_mask <- terra::ifel(
    !is.na(shallow_prop) &
      shallow_prop >= min_fraction,
    1,
    NA
  )
  
  
  ## Rename ----
  names(shallow_prop) <- "shallow_fraction"
  
  names(bathy_mask) <- "bathy_mask"
  
  
  return(
    list(
      fraction = shallow_prop,
      mask = bathy_mask
    )
  )
  
}


## ============================================================
## 7. Prepare regional predictions and masks
## ============================================================

original_preds <- list()

masked_preds <- list()

bathy_results <- list()


for (region_name in names(regions)) {
  
  region <- regions[[region_name]]
  
  
  ## Regional extent ----
  bbox <- terra::ext(
    region$xmin,
    region$xmax,
    region$ymin,
    region$ymax
  )
  
  
  ## Historical prediction reference grid ----
  r_hist <- terra::crop(
    preds[["histclim"]],
    bbox,
    snap = "near"
  )
  
  
  if (is.null(r_hist)) {
    stop(glue("No historical overlap: {region_name}"))
  }
  
  
  ## Calculate fixed bathymetry mask ----
  h <- get_bathy_mask(
    target = r_hist,
    shallow_fraction = shallow_fraction,
    ocean_fraction = ocean_fraction,
    min_fraction = params$min_shallow_fraction
  )
  
  
  bathy_results[[region_name]] <- h
  
  
  ## Save original-resolution bathymetry mask ----
  terra::writeRaster(
    h$mask,
    file.path(
      out_dir,
      glue("bathy_mask_{region_name}.tif")
    ),
    overwrite = TRUE
  )
  
  
  ## Save shallow habitat fraction ----
  terra::writeRaster(
    h$fraction,
    file.path(
      out_dir,
      glue("shallow_fraction_{region_name}.tif")
    ),
    overwrite = TRUE
  )
  
  
  ## Initialize prediction lists ----
  original_preds[[region_name]] <- list()
  
  masked_preds[[region_name]] <- list()
  
  
  for (scenario in scenarios) {
    
    message(
      glue("Processing {region_name}: {scenario}")
    )
    
    
    ## Historical raster ----
    if (scenario == "histclim") {
      
      r <- r_hist
      
    } else {
      
      ## Crop future predictions with margin ----
      r_future <- terra::crop(
        preds[[scenario]],
        terra::ext(
          region$xmin - 0.3,
          region$xmax + 0.3,
          region$ymin - 0.3,
          region$ymax + 0.3
        ),
        snap = "out"
      )
      
      
      if (is.null(r_future)) {
        stop(
          glue(
            "No future overlap: {region_name}, {scenario}"
          )
        )
      }
      
      
      ## Align future prediction with historical grid ----
      if (!terra::compareGeom(
        r_future,
        r_hist,
        stopOnError = FALSE
      )) {
        
        r <- terra::resample(
          r_future,
          r_hist,
          method = "bilinear"
        )
        
      } else {
        
        r <- r_future
        
      }
      
    }
    
    
    ## Apply bathymetry mask ----
    r_masked <- terra::mask(
      r,
      h$mask
    )
    
    
    ## Store original-resolution rasters ----
    original_preds[[region_name]][[scenario]] <- r
    
    masked_preds[[region_name]][[scenario]] <- r_masked
    
    
    ## Save ORIGINAL-RESOLUTION masked prediction ----
    terra::writeRaster(
      r_masked,
      file.path(
        out_dir,
        glue(
          "CTX_{region_name}_{scenario}_masked.tif"
        )
      ),
      overwrite = TRUE
    )
    
  }
  
}


## ============================================================
## 8. Summarize retained prediction cells
## ============================================================

count_valid <- function(r) {
  
  terra::global(
    !is.na(r),
    fun = "sum",
    na.rm = TRUE
  )[1, 1]
  
}


retention_results <- list()


for (region_name in names(regions)) {
  
  for (scenario in scenarios) {
    
    n_original <- count_valid(
      original_preds[[region_name]][[scenario]]
    )
    
    n_masked <- count_valid(
      masked_preds[[region_name]][[scenario]]
    )
    
    
    key <- glue("{region_name}_{scenario}")
    
    retention_results[[key]] <- data.frame(
      region = region_name,
      scenario = scenario,
      n_original = n_original,
      n_masked = n_masked,
      pct_retained = if (n_original > 0) {
        100 * n_masked / n_original
      } else {
        NA_real_
      }
    )
    
  }
  
}


retention_summary <- dplyr::bind_rows(
  retention_results
)


## Print and save ----
print(retention_summary)

write.csv(
  retention_summary,
  file.path(
    out_dir,
    "CTX_retention_summary_5regions.csv"
  ),
  row.names = FALSE
)


## ============================================================
## 9. Future minus historical changes
## ============================================================

## Use the original-resolution masked predictions.
## Do NOT use the smoothed display rasters.

change_results <- list()

change_rasters <- list()


for (region_name in names(regions)) {
  
  change_rasters[[region_name]] <- list()
  
  hist_r <- masked_preds[[region_name]][["histclim"]]
  
  
  for (scenario in c("ssp126", "ssp585")) {
    
    future_r <- masked_preds[[region_name]][[scenario]]
    
    
    ## Check geometry ----
    stopifnot(
      terra::compareGeom(
        hist_r,
        future_r,
        stopOnError = FALSE
      )
    )
    
    
    ## Future minus historical ----
    diff_r <- future_r - hist_r
    
    names(diff_r) <- glue(
      "CTX_change_{scenario}"
    )
    
    
    ## Store change raster ----
    change_rasters[[region_name]][[scenario]] <- diff_r
    
    
    ## Save change raster ----
    terra::writeRaster(
      diff_r,
      file.path(
        out_dir,
        glue(
          "CTX_change_{region_name}_{scenario}.tif"
        )
      ),
      overwrite = TRUE
    )
    
    
    ## Summary ----
    n_valid <- count_valid(diff_r)
    
    
    if (n_valid > 0) {
      
      mean_change <- terra::global(
        diff_r,
        "mean",
        na.rm = TRUE
      )[1, 1]
      
      n_increase <- terra::global(
        diff_r > 0,
        "sum",
        na.rm = TRUE
      )[1, 1]
      
      n_decrease <- terra::global(
        diff_r < 0,
        "sum",
        na.rm = TRUE
      )[1, 1]
      
      pct_increase <- 100 * n_increase / n_valid
      
    } else {
      
      mean_change <- NA_real_
      
      n_increase <- 0
      
      n_decrease <- 0
      
      pct_increase <- NA_real_
      
    }
    
    
    ## Store result ----
    key <- glue("{region_name}_{scenario}")
    
    change_results[[key]] <- data.frame(
      region = region_name,
      scenario = scenario,
      mean_change = mean_change,
      n_compared = n_valid,
      n_increase = n_increase,
      n_decrease = n_decrease,
      pct_increase = pct_increase
    )
    
  }
  
}


change_summary <- dplyr::bind_rows(
  change_results
)


## Print and save ----
print(change_summary)

write.csv(
  change_summary,
  file.path(
    out_dir,
    "CTX_future_minus_historical_5regions.csv"
  ),
  row.names = FALSE
)


## ============================================================
## 10. Create smoothed display rasters
## ============================================================
##
## IMPORTANT:
##
## Smoothing is for plotting ONLY.
##
## Original prediction:
##   ~0.083 degrees
##
## Upsampled display (factor = 3):
##   ~0.0278 degrees
##
## Method:
##   1. Start with UNMASKED prediction
##   2. Create finer display grid
##   3. Bilinearly interpolate predicted CTX
##   4. Transfer binary mask using nearest neighbor
##   5. Apply mask AFTER interpolation
##
## This avoids interpolating NA values outside
## the bathymetry mask.
## ============================================================


## Smoothing function ----
smooth_for_plot <- function(
    r,
    mask,
    factor = 3
) {
  
  ## Original prediction and mask must align ----
  stopifnot(
    terra::compareGeom(
      r,
      mask,
      stopOnError = FALSE
    )
  )
  
  
  ## Create finer display grid ----
  display_grid <- terra::disagg(
    r,
    fact = factor
  )
  
  
  ## Bilinearly interpolate prediction ----
  r_smooth <- terra::resample(
    r,
    display_grid,
    method = "bilinear"
  )
  
  
  ## Transfer binary mask using nearest neighbor ----
  mask_display <- terra::resample(
    mask,
    display_grid,
    method = "near"
  )
  
  
  ## Apply mask AFTER interpolation ----
  r_smooth <- terra::mask(
    r_smooth,
    mask_display
  )
  
  
  return(r_smooth)
  
}


## Prepare separate display rasters ----
display_preds <- list()

display_original <- list()


for (region_name in names(regions)) {
  
  display_preds[[region_name]] <- list()
  
  display_original[[region_name]] <- list()
  
  
  ## Fixed original-resolution mask ----
  mask <- bathy_results[[region_name]]$mask
  
  
  for (scenario in scenarios) {
    
    ## Original unrestricted prediction ----
    r <- original_preds[[region_name]][[scenario]]
    
    
    if (params$plot_smooth) {
      
      ## Create lightly smoothed masked raster ----
      r_display <- smooth_for_plot(
        r = r,
        mask = mask,
        factor = params$smooth_factor
      )
      
      
      ## Create smoothed unrestricted raster ----
      display_grid <- terra::disagg(
        r,
        fact = params$smooth_factor
      )
      
      r_original_display <- terra::resample(
        r,
        display_grid,
        method = "bilinear"
      )
      
    } else {
      
      ## No smoothing ----
      r_display <- masked_preds[[region_name]][[scenario]]
      
      r_original_display <- r
      
    }
    
    
    ## Store display rasters ----
    display_preds[[region_name]][[scenario]] <- r_display
    
    display_original[[region_name]][[scenario]] <-
      r_original_display
    
  }
  
}


## ============================================================
## 11. CTX color scale
## ============================================================

ctx_limits <- params$ctx_limits

stopifnot(
  length(ctx_limits) == 2,
  all(is.finite(ctx_limits)),
  ctx_limits[1] < ctx_limits[2]
)


## Pacific-wide smooth-rainbow palette ----
smooth_rainbow <- khroma::colour("smooth rainbow")

cpal <- c(
  smooth_rainbow(
    length(seq(0, 0.25, length = 100)),
    range = c(0, 0.9)
  ),
  "#9e2a2b",
  "firebrick4"
)


## DO NOT REVERSE
## Low CTX  = first color
## High CTX = final dark-red color


## ============================================================
## 12. Load land polygons
## ============================================================

world_sf <- rnaturalearth::ne_countries(
  scale = "large",
  returnclass = "sf"
) |>
  sf::st_make_valid()


## Regional land polygons ----
world_regions <- list()


for (region_name in names(regions)) {
  
  region <- regions[[region_name]]
  
  
  world_regions[[region_name]] <- suppressWarnings(
    sf::st_crop(
      world_sf,
      xmin = region$xmin,
      xmax = region$xmax,
      ymin = region$ymin,
      ymax = region$ymax
    )
  )
  
}


## ============================================================
## 13. Plotting helpers
## ============================================================

## Raster to dataframe ----
raster_to_df <- function(r) {
  
  df <- as.data.frame(
    r,
    xy = TRUE,
    na.rm = FALSE
  )
  
  names(df)[3] <- "value"
  
  return(df)
  
}


## Longitude labels ----
lon_labels <- function(x) {
  
  ifelse(
    x < 180,
    paste0(x, "°E"),
    ifelse(
      x > 180,
      paste0(360 - x, "°W"),
      "180°"
    )
  )
  
}


## Latitude labels ----
lat_labels <- function(y) {
  
  ifelse(
    y < 0,
    paste0(abs(y), "°S"),
    paste0(y, "°N")
  )
  
}


## ============================================================
## 14. CTX plotting function
## ============================================================

plot_ctx_region <- function(
    r,
    region_name,
    plot_title,
    show_sites = params$show_sites
) {
  
  region <- regions[[region_name]]
  
  df <- raster_to_df(r)
  
  
  ## Sampling sites ----
  site_df <- sites %>%
    dplyr::filter(
      region == region_name
    )
  
  
  p <- ggplot() +
    
    ## Ocean background ----
  geom_rect(
    xmin = region$xmin,
    xmax = region$xmax,
    ymin = region$ymin,
    ymax = region$ymax,
    fill = "gray94",
    colour = NA
  ) +
    
    ## CTX predictions ----
  ## IMPORTANT:
  ## interpolate = FALSE because values have
  ## already been interpolated for display.
  
  geom_raster(
    data = df,
    aes(
      x = x,
      y = y,
      fill = value
    ),
    interpolate = FALSE,
    na.rm = TRUE
  ) +
    
    ## Land polygons ----
  geom_sf(
    data = world_regions[[region_name]],
    fill = "black",
    colour = "black",
    linewidth = 0.15,
    inherit.aes = FALSE
  ) +
    
    ## Original CTX palette ----
  scale_fill_gradientn(
    colours = cpal,
    limits = ctx_limits,
    oob = scales::squish,
    na.value = "transparent",
    name = params$ctx_label
  ) +
    
    ## Longitude axis ----
  scale_x_continuous(
    breaks = pretty(
      c(region$xmin, region$xmax),
      n = 4
    ),
    labels = lon_labels
  ) +
    
    ## Latitude axis ----
  scale_y_continuous(
    breaks = pretty(
      c(region$ymin, region$ymax),
      n = 4
    ),
    labels = lat_labels
  ) +
    
    ## Map extent ----
  coord_sf(
    xlim = c(region$xmin, region$xmax),
    ylim = c(region$ymin, region$ymax),
    expand = FALSE
  ) +
    
    ## Labels ----
  labs(
    title = plot_title,
    x = NULL,
    y = NULL
  ) +
    
    ## Theme ----
  theme_bw(base_size = 11) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11
      ),
      
      panel.background = element_rect(
        fill = "gray94",
        colour = NA
      ),
      
      panel.grid = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        size = 10
      ),
      
      legend.text = element_text(
        size = 9
      ),
      
      axis.text = element_text(
        size = 9
      )
    )
  
  
  ## Sampling locations ----
  if (show_sites && nrow(site_df) > 0) {
    
    p <- p +
      
      geom_point(
        data = site_df,
        aes(
          x = lon,
          y = lat
        ),
        shape = 21,
        size = 2.5,
        stroke = 0.8,
        fill = "white",
        colour = "black",
        inherit.aes = FALSE
      )
    
  }
  
  
  return(p)
  
}


## ============================================================
## 15. Multi-panel figure function
## ============================================================

make_supp_figure <- function(
    pred_list,
    selected_regions = names(regions)
) {
  
  plots <- list()
  
  panel_i <- 0
  
  
  for (region_name in selected_regions) {
    
    for (scenario in scenarios) {
      
      panel_i <- panel_i + 1
      
      region <- regions[[region_name]]
      
      
      ## Panel title ----
      if (scenario == "histclim") {
        
        panel_title <- glue(
          "{LETTERS[panel_i]}. Historical - {region$label}"
        )
        
      } else {
        
        panel_title <- glue(
          "{LETTERS[panel_i]}. ",
          "{scenario_labels[[scenario]]}"
        )
        
      }
      
      
      ## Create panel ----
      plots[[panel_i]] <- plot_ctx_region(
        r = pred_list[[region_name]][[scenario]],
        region_name = region_name,
        plot_title = panel_title
      )
      
    }
    
  }
  
  
  ## Three columns: Historical, SSP126, SSP585 ----
  p <- patchwork::wrap_plots(
    plots,
    ncol = 3,
    byrow = TRUE,
    guides = "collect"
  ) &
    theme(
      legend.position = "bottom"
    )
  
  
  return(p)
  
}


## ============================================================
## 16. Figure 1: All five regions, bathymetry-masked
## ============================================================

p_supp_ctx_all <- make_supp_figure(
  pred_list = display_preds
)


## Display ----
print(p_supp_ctx_all)


## Save PNG ----
ggsave(
  filename = file.path(
    out_dir,
    "CTX_bathy_masked_5regions_15panel_smooth.png"
  ),
  plot = p_supp_ctx_all,
  width = params$figure_width,
  height = params$figure_height,
  dpi = params$figure_dpi,
  bg = "white"
)


## Save PDF ----
ggsave(
  filename = file.path(
    out_dir,
    "CTX_bathy_masked_5regions_15panel_smooth.pdf"
  ),
  plot = p_supp_ctx_all,
  width = params$figure_width,
  height = params$figure_height,
  bg = "white"
)


## ============================================================
## 17. Figure 2: New Caledonia + GBR
## ============================================================

p_supp_ctx_warm <- make_supp_figure(
  pred_list = display_preds,
  selected_regions = c(
    "new_caledonia",
    "great_barrier_reef"
  )
)


print(p_supp_ctx_warm)


ggsave(
  filename = file.path(
    out_dir,
    "CTX_bathy_masked_NC_GBR_6panel_smooth.png"
  ),
  plot = p_supp_ctx_warm,
  width = 15,
  height = 9,
  dpi = params$figure_dpi,
  bg = "white"
)


## ============================================================
## 18. Figure 3: NSW + Northern NZ
## ============================================================

p_supp_ctx_south <- make_supp_figure(
  pred_list = display_preds,
  selected_regions = c(
    "nsw",
    "north_nz"
  )
)


print(p_supp_ctx_south)


ggsave(
  filename = file.path(
    out_dir,
    "CTX_bathy_masked_NSW_NZ_6panel_smooth.png"
  ),
  plot = p_supp_ctx_south,
  width = 15,
  height = 9,
  dpi = params$figure_dpi,
  bg = "white"
)


## ============================================================
## 19. Figure 4: Southern Japan
## ============================================================

p_supp_ctx_japan <- make_supp_figure(
  pred_list = display_preds,
  selected_regions = "southern_japan"
)


print(p_supp_ctx_japan)


ggsave(
  filename = file.path(
    out_dir,
    "CTX_bathy_masked_southern_japan_3panel_smooth.png"
  ),
  plot = p_supp_ctx_japan,
  width = 15,
  height = 5.5,
  dpi = params$figure_dpi,
  bg = "white"
)


## ============================================================
## 20. Figure 5: Unrestricted predictions
## ============================================================

p_supp_ctx_unrestricted <- make_supp_figure(
  pred_list = display_original
)


print(p_supp_ctx_unrestricted)


ggsave(
  filename = file.path(
    out_dir,
    "CTX_unrestricted_5regions_15panel_smooth.png"
  ),
  plot = p_supp_ctx_unrestricted,
  width = params$figure_width,
  height = params$figure_height,
  dpi = params$figure_dpi,
  bg = "white"
)


## ============================================================
## 21. Compare original vs smoothed display
## ============================================================

## Example: Southern Japan, SSP585 ----

p_original_pixels <- plot_ctx_region(
  r = masked_preds[["southern_japan"]][["ssp585"]],
  region_name = "southern_japan",
  plot_title = "A. Original resolution (0.083 degrees)"
)


p_smoothed_pixels <- plot_ctx_region(
  r = display_preds[["southern_japan"]][["ssp585"]],
  region_name = "southern_japan",
  plot_title = "B. Bilinear display interpolation"
)


## Side-by-side comparison ----
p_smooth_comparison <- (
  p_original_pixels | p_smoothed_pixels
) +
  patchwork::plot_layout(
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )


print(p_smooth_comparison)


ggsave(
  filename = file.path(
    out_dir,
    "CTX_southern_japan_smoothing_comparison.png"
  ),
  plot = p_smooth_comparison,
  width = 12,
  height = 6,
  dpi = params$figure_dpi,
  bg = "white"
)


## ============================================================
## 22. Final summaries
## ============================================================

## Retained prediction cells ----
print(retention_summary)


## Future-minus-historical CTX changes ----
print(change_summary)


## Increasing CTX regions ----
increasing_regions <- change_summary %>%
  dplyr::filter(
    is.finite(mean_change),
    mean_change > 0
  ) %>%
  dplyr::arrange(
    scenario,
    dplyr::desc(mean_change)
  )


print(increasing_regions)


message("Five-region bathymetry analysis complete.")

message(
  glue("Outputs saved to: {out_dir}")
)




### PARTITION FIGS INTO DECREASING CTX VS INCREASING CTX:

## ============================================================
## Additional figures: Panels A-F and G-O
##
## Figure 1: A-F (2 rows x 3 columns)
## Figure 2: G-O (3 rows x 3 columns)
## ============================================================


## Updated multi-panel function ----
make_supp_figure <- function(
    pred_list,
    selected_regions = names(regions)
) {
  
  plots <- list()
  
  panel_i <- 0
  
  for (region_name in selected_regions) {
    
    for (scenario in scenarios) {
      
      panel_i <- panel_i + 1
      
      region <- regions[[region_name]]
      
      
      ## Preserve original A-O panel labels ----
      global_panel_i <- (
        match(region_name, names(regions)) - 1
      ) * length(scenarios) +
        match(scenario, scenarios)
      
      panel_letter <- LETTERS[global_panel_i]
      
      
      ## Panel title ----
      if (scenario == "histclim") {
        
        panel_title <- glue(
          "{panel_letter}. Historical - {region$label}"
        )
        
      } else {
        
        panel_title <- glue(
          "{panel_letter}. {scenario_labels[[scenario]]}"
        )
        
      }
      
      
      ## Create panel ----
      plots[[panel_i]] <- plot_ctx_region(
        r = pred_list[[region_name]][[scenario]],
        region_name = region_name,
        plot_title = panel_title
      )
      
    }
    
  }
  
  
  ## Combine panels ----
  p <- patchwork::wrap_plots(
    plots,
    ncol = 3,
    byrow = TRUE,
    guides = "collect"
  ) &
    theme(
      legend.position = "bottom"
    )
  
  return(p)
  
}


## ============================================================
## Figure 1: Panels A-F
## Southern New Caledonia + GBR
## ============================================================

p_supp_ctx_AF <- make_supp_figure(
  pred_list = display_preds,
  selected_regions = c(
    "new_caledonia",
    "great_barrier_reef"
  )
)

print(p_supp_ctx_AF)


## Save PNG ----
ggsave(
  filename = file.path(
    out_dir,
    "CTX_bathy_masked_panels_A-F.png"
  ),
  plot = p_supp_ctx_AF,
  width = 15,
  height = 9,
  dpi = params$figure_dpi,
  bg = "white"
)


## Save PDF ----
ggsave(
  filename = file.path(
    out_dir,
    "CTX_bathy_masked_panels_A-F.pdf"
  ),
  plot = p_supp_ctx_AF,
  width = 15,
  height = 9,
  bg = "white"
)


## ============================================================
## Figure 2: Panels G-O
## NSW + Northern NZ + Southern Japan
## ============================================================

p_supp_ctx_GO <- make_supp_figure(
  pred_list = display_preds,
  selected_regions = c(
    "nsw",
    "north_nz",
    "southern_japan"
  )
)

print(p_supp_ctx_GO)


## Save PNG ----
ggsave(
  filename = file.path(
    out_dir,
    "CTX_bathy_masked_panels_G-O.png"
  ),
  plot = p_supp_ctx_GO,
  width = 15,
  height = 13,
  dpi = params$figure_dpi,
  bg = "white"
)


## Save PDF ----
ggsave(
  filename = file.path(
    out_dir,
    "CTX_bathy_masked_panels_G-O.pdf"
  ),
  plot = p_supp_ctx_GO,
  width = 15,
  height = 13,
  bg = "white"
)








### ADD EXTRAS - TONGA, FP, RARO ----------------


## ============================================================
## ADD-ON: Bathymetry-masked CTX predictions
##
## Additional regions:
##   1. Tonga
##   2. French Polynesia
##   3. Rarotonga, Cook Islands
##
## Scenarios:
##   Historical GLORYS
##   SSP126
##   SSP585
##
## Bathymetry: GEBCO 0-200 m
## Prediction resolution: ~0.083 degrees
## Habitat resolution: 0.05 degrees
## Longitude convention: 0-360
##
## Uses existing objects from five-region script:
##   preds
##   shallow_fraction
##   ocean_fraction
##   params
##   scenarios
##   scenario_labels
##   cpal
##   ctx_limits
##   get_bathy_mask()
##   smooth_for_plot()
##
## Outputs:
##   - Combined 9-panel figure
##   - Three individual 3-panel figures
##   - Unrestricted 9-panel comparison
##   - Bathymetry-masked prediction rasters
##   - Cell-retention summary
## ============================================================


## Libraries ----
library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(patchwork)
library(dplyr)
library(glue)
library(here)


## ============================================================
## 1. Parameters
## ============================================================

## Reuse original five-region parameters ----
## These settings only affect these additional figures.

extra_params <- list(
  
  ## Figure settings
  figure_width = 15,
  figure_height = 14,
  figure_dpi = 300,
  
  ## Use existing bathymetric threshold
  min_shallow_fraction = params$min_shallow_fraction,
  
  ## Use original display smoothing
  plot_smooth = params$plot_smooth,
  smooth_factor = params$smooth_factor,
  
  ## Plot sampling locations
  show_sites = TRUE,
  
  ## Show site labels?
  show_site_labels = FALSE
  
)


## Output directory ----
extra_out_dir <- here(
  "figures",
  "supplementary",
  "bathy_mask_extra_regions"
)

dir.create(
  extra_out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


## ============================================================
## 2. Define additional regions
## ============================================================

extra_regions <- list(
  
  ## Tonga ----
  ## Includes Vava'u and Ha'apai
  tonga = list(
    label = "Tonga",
    xmin = 184.8,
    xmax = 187.2,
    ymin = -21.2,
    ymax = -17.2
  ),
  
  ## French Polynesia ----
  ## Includes Marquesas, Tuamotu, Society Islands,
  ## Austral Islands and Gambier Islands
  french_polynesia = list(
    label = "French Polynesia",
    xmin = 207,
    xmax = 226.5,
    ymin = -26,
    ymax = -7
  ),
  
  ## Rarotonga / Cook Islands ----
  rarotonga = list(
    label = "Rarotonga, Cook Islands",
    xmin = 199,
    xmax = 201.6,
    ymin = -22.4,
    ymax = -20
  )
  
)


## Region plotting order ----
extra_region_order <- c(
  "tonga",
  "french_polynesia",
  "rarotonga"
)


## ============================================================
## 3. Define sampling locations
## ============================================================

## Convert longitude to 0-360 ----
make360 <- function(x) {
  ifelse(x < 0, x + 360, x)
}


## Sampling locations ----
extra_sites <- data.frame(
  
  region = c(
    "tonga",
    "tonga",
    "french_polynesia",
    "french_polynesia",
    "french_polynesia",
    "french_polynesia",
    "french_polynesia",
    "french_polynesia",
    "french_polynesia",
    "french_polynesia",
    "rarotonga"
  ),
  
  site = c(
    "Vava'u",
    "Ha'apai",
    "Nuku Hiva",
    "Rangiroa",
    "Gambier Islands",
    "Raivavae",
    "Tubuai",
    "Bora Bora",
    "Moorea",
    "Tahiti",
    "Rarotonga"
  ),
  
  lat = c(
    -18.606,
    -19.751,
    -8.887,
    -15.126,
    -23.128,
    -23.869,
    -23.000,
    -16.501,
    -17.542,
    -17.612,
    -21.237
  ),
  
  lon = c(
    -173.967,
    -174.372,
    -140.144,
    -147.637,
    -134.979,
    -147.663,
    -150.000,
    -151.735,
    -149.831,
    -149.514,
    -159.781
  ),
  
  toxin_data = c(
    "Not tested",
    "Not tested",
    "4.61",
    "2.23",
    "4.2",
    "3.62",
    "7.65",
    "Not tested",
    "Not tested",
    "18.96",
    "0.44"
  )
  
) %>%
  dplyr::mutate(
    lon360 = make360(lon)
  )


## Check sampling points ----
print(extra_sites)


# ## ============================================================
# ## 4. Load land polygons in 0-360 longitude
# ## ============================================================
# 
# ## Load land ----
# extra_world_sf <- rnaturalearth::ne_countries(
#   scale = "large",
#   returnclass = "sf"
# ) %>%
#   sf::st_make_valid()
# 
# 
# ## Convert to 0-360 ----
# extra_world_sf <- sf::st_shift_longitude(
#   extra_world_sf
# )
# 
# 
# ## Convert land polygons to ggplot data frames ----
# ## This avoids the coord_sf() degree-label error.
# 
# 
# get_land_df <- function(
#     world,
#     region
# ) {
#   
#   ## Crop land to region ----
#   land_crop <- suppressWarnings(
#     sf::st_crop(
#       world,
#       xmin = region$xmin,
#       xmax = region$xmax,
#       ymin = region$ymin,
#       ymax = region$ymax
#     )
#   )
#   
#   
#   ## Return empty dataframe if no land ----
#   if (nrow(land_crop) == 0) {
#     
#     return(
#       data.frame(
#         long = numeric(),
#         lat = numeric(),
#         group = character()
#       )
#     )
#     
#   }
#   
#   
#   ## Extract polygon geometries ----
#   land_polygons <- suppressWarnings(
#     sf::st_cast(
#       sf::st_geometry(land_crop),
#       "POLYGON"
#     )
#   )
#   
#   
#   ## Convert each polygon ring to dataframe ----
#   land_list <- list()
#   
#   for (i in seq_along(land_polygons)) {
#     
#     coords <- sf::st_coordinates(
#       land_polygons[i]
#     )
#     
#     ## Identify polygon/ring indices
#     group_cols <- grep(
#       "^L[0-9]+$",
#       colnames(coords),
#       value = TRUE
#     )
#     
#     ## Group each ring independently
#     if (length(group_cols) > 0) {
#       
#       ring_group <- do.call(
#         interaction,
#         c(
#           as.data.frame(
#             coords[, group_cols, drop = FALSE]
#           ),
#           list(drop = TRUE)
#         )
#       )
#       
#     } else {
#       
#       ring_group <- rep(
#         "1",
#         nrow(coords)
#       )
#       
#     }
#     
#     
#     land_list[[i]] <- data.frame(
#       long = coords[, "X"],
#       lat = coords[, "Y"],
#       group = paste0(
#         i,
#         "_",
#         ring_group
#       )
#     )
#     
#   }
#   
#   
#   ## Combine polygons ----
#   land_df <- dplyr::bind_rows(
#     land_list
#   )
#   
#   return(land_df)
#   
# }
# 
# 
# ## Prepare regional land data ----
# extra_world_regions <- list()
# 
# for (region_name in extra_region_order) {
#   
#   region <- extra_regions[[region_name]]
#   
#   extra_world_regions[[region_name]] <- get_land_df(
#     world = extra_world_sf,
#     region = region
#   )
#   
# }

## ============================================================
## 4. Load higher-detail land polygons
## ============================================================

## Use Natural Earth physical land polygons
## rather than administrative country polygons.

extra_world_sf <- rnaturalearth::ne_download(
  scale = 10,
  type = "land",
  category = "physical",
  returnclass = "sf"
)


## Ensure valid geometries ----
extra_world_sf <- sf::st_make_valid(
  extra_world_sf
)


## Convert to 0-360 longitude ----
extra_world_sf <- sf::st_shift_longitude(
  extra_world_sf
)


## Function to convert sf polygons to ggplot dataframe ----
get_land_df <- function(world, region) {
  
  ## Crop to regional extent ----
  land_crop <- suppressWarnings(
    sf::st_crop(
      world,
      xmin = region$xmin,
      xmax = region$xmax,
      ymin = region$ymin,
      ymax = region$ymax
    )
  )
  
  if (nrow(land_crop) == 0) {
    
    warning(
      glue::glue(
        "No land polygons found for {region$label}"
      )
    )
    
    return(
      data.frame(
        long = numeric(),
        lat = numeric(),
        group = character()
      )
    )
    
  }
  
  
  ## Convert to polygon geometries ----
  land_poly <- suppressWarnings(
    sf::st_cast(
      sf::st_geometry(land_crop),
      "POLYGON"
    )
  )
  
  
  ## Extract coordinates ----
  land_list <- lapply(
    seq_along(land_poly),
    function(i) {
      
      coords <- sf::st_coordinates(
        land_poly[i]
      )
      
      ## Distinguish exterior and interior rings
      ring_id <- if ("L1" %in% colnames(coords)) {
        coords[, "L1"]
      } else {
        rep(1, nrow(coords))
      }
      
      data.frame(
        long = coords[, "X"],
        lat = coords[, "Y"],
        group = paste(i, ring_id, sep = "_")
      )
      
    }
  )
  
  
  dplyr::bind_rows(land_list)
  
}


## Prepare land polygons for each region ----
extra_world_regions <- list()

for (region_name in extra_region_order) {
  
  region <- extra_regions[[region_name]]
  
  extra_world_regions[[region_name]] <- get_land_df(
    world = extra_world_sf,
    region = region
  )
  
}


## Check polygon availability ----
sapply(
  extra_world_regions,
  nrow
)

## ============================================================
## 5. Prepare bathymetry masks and predictions
## ============================================================

extra_original_preds <- list()
extra_masked_preds <- list()

extra_display_preds <- list()
extra_display_original <- list()

extra_bathy_results <- list()


for (region_name in extra_region_order) {
  
  region <- extra_regions[[region_name]]
  
  
  ## Regional extent ----
  bbox <- terra::ext(
    region$xmin,
    region$xmax,
    region$ymin,
    region$ymax
  )
  
  
  ## Historical prediction reference grid ----
  r_hist <- terra::crop(
    preds[["histclim"]],
    bbox,
    snap = "near"
  )
  
  
  if (is.null(r_hist)) {
    
    stop(
      glue::glue(
        "No historical prediction overlap: {region_name}"
      )
    )
    
  }
  
  
  ## Create bathymetry-only mask ----
  h <- get_bathy_mask(
    target = r_hist,
    shallow_fraction = shallow_fraction,
    ocean_fraction = ocean_fraction,
    min_fraction = extra_params$min_shallow_fraction
  )
  
  
  ## Store habitat results ----
  extra_bathy_results[[region_name]] <- h
  
  
  ## Save mask ----
  terra::writeRaster(
    h$mask,
    file.path(
      extra_out_dir,
      glue::glue(
        "bathy_mask_{region_name}.tif"
      )
    ),
    overwrite = TRUE
  )
  
  
  ## Save shallow fraction ----
  terra::writeRaster(
    h$fraction,
    file.path(
      extra_out_dir,
      glue::glue(
        "shallow_fraction_{region_name}.tif"
      )
    ),
    overwrite = TRUE
  )
  
  
  ## Initialize regional lists ----
  extra_original_preds[[region_name]] <- list()
  
  extra_masked_preds[[region_name]] <- list()
  
  extra_display_preds[[region_name]] <- list()
  
  extra_display_original[[region_name]] <- list()
  
  
  ## Process scenarios ----
  for (scenario in scenarios) {
    
    message(
      glue::glue(
        "Processing {region_name}: {scenario}"
      )
    )
    
    
    ## Historical prediction ----
    if (scenario == "histclim") {
      
      r <- r_hist
      
    } else {
      
      ## Crop future prediction with margin ----
      r_future <- terra::crop(
        preds[[scenario]],
        terra::ext(
          region$xmin - 0.3,
          region$xmax + 0.3,
          region$ymin - 0.3,
          region$ymax + 0.3
        ),
        snap = "out"
      )
      
      
      if (is.null(r_future)) {
        
        stop(
          glue::glue(
            "No future prediction overlap: ",
            "{region_name}, {scenario}"
          )
        )
        
      }
      
      
      ## Align future prediction to historical grid ----
      if (!terra::compareGeom(
        r_future,
        r_hist,
        stopOnError = FALSE
      )) {
        
        r <- terra::resample(
          r_future,
          r_hist,
          method = "bilinear"
        )
        
      } else {
        
        r <- r_future
        
      }
      
    }
    
    
    ## Apply bathymetry-only mask ----
    r_masked <- terra::mask(
      r,
      h$mask
    )
    
    
    ## Store analytical rasters ----
    extra_original_preds[[region_name]][[scenario]] <- r
    
    extra_masked_preds[[region_name]][[scenario]] <- r_masked
    
    
    ## Save masked raster ----
    terra::writeRaster(
      r_masked,
      file.path(
        extra_out_dir,
        glue::glue(
          "CTX_{region_name}_{scenario}_bathy_masked.tif"
        )
      ),
      overwrite = TRUE
    )
    
    
    ## Create display rasters ----
    if (extra_params$plot_smooth) {
      
      ## Smooth prediction and reapply mask ----
      r_display <- smooth_for_plot(
        r = r,
        mask = h$mask,
        factor = extra_params$smooth_factor
      )
      
      
      ## Smoothed unrestricted raster ----
      display_grid <- terra::disagg(
        r,
        fact = extra_params$smooth_factor
      )
      
      r_original_display <- terra::resample(
        r,
        display_grid,
        method = "bilinear"
      )
      
    } else {
      
      ## Original resolution ----
      r_display <- r_masked
      
      r_original_display <- r
      
    }
    
    
    ## Store plotting rasters ----
    extra_display_preds[[region_name]][[scenario]] <- r_display
    
    extra_display_original[[region_name]][[scenario]] <-
      r_original_display
    
  }
  
}


## ============================================================
## 6. Summarize retained prediction cells
## ============================================================

count_valid_extra <- function(r) {
  
  terra::global(
    !is.na(r),
    fun = "sum",
    na.rm = TRUE
  )[1, 1]
  
}


extra_retention_results <- list()


for (region_name in extra_region_order) {
  
  for (scenario in scenarios) {
    
    n_original <- count_valid_extra(
      extra_original_preds[[region_name]][[scenario]]
    )
    
    n_masked <- count_valid_extra(
      extra_masked_preds[[region_name]][[scenario]]
    )
    
    
    key <- glue::glue(
      "{region_name}_{scenario}"
    )
    
    
    extra_retention_results[[key]] <- data.frame(
      region = region_name,
      scenario = scenario,
      n_original = n_original,
      n_masked = n_masked,
      pct_retained = if (n_original > 0) {
        100 * n_masked / n_original
      } else {
        NA_real_
      }
    )
    
  }
  
}


## Combine summary ----
extra_retention_summary <- dplyr::bind_rows(
  extra_retention_results
)


## Print ----
print(extra_retention_summary)


## Save ----
write.csv(
  extra_retention_summary,
  file.path(
    extra_out_dir,
    "CTX_bathy_retention_extra_regions.csv"
  ),
  row.names = FALSE
)


## ============================================================
## 7. Plotting helpers
## ============================================================

## Raster to dataframe ----
raster_to_df_extra <- function(r) {
  
  df <- as.data.frame(
    r,
    xy = TRUE,
    na.rm = FALSE
  )
  
  names(df)[3] <- "value"
  
  return(df)
  
}


## Longitude labels for 0-360 ----
lon_labels_extra <- function(x) {
  
  ifelse(
    x < 180,
    paste0(x, "°E"),
    ifelse(
      x > 180,
      paste0(360 - x, "°W"),
      "180°"
    )
  )
  
}


## Latitude labels ----
lat_labels_extra <- function(y) {
  
  ifelse(
    y < 0,
    paste0(abs(y), "°S"),
    paste0(y, "°N")
  )
  
}


## ============================================================
## 8. Plotting function for extra regions
## ============================================================

plot_ctx_region_extra <- function(
    r,
    region_name,
    plot_title,
    show_sites = extra_params$show_sites
) {
  
  region <- extra_regions[[region_name]]
  
  df <- raster_to_df_extra(r)
  
  
  ## Sampling locations ----
  site_df <- extra_sites %>%
    dplyr::filter(
      region == region_name
    )
  
  
  ## Longitude breaks ----
  xbreaks <- pretty(
    c(region$xmin, region$xmax),
    n = 4
  )
  
  xbreaks <- xbreaks[
    xbreaks >= region$xmin &
      xbreaks <= region$xmax
  ]
  
  
  ## Latitude breaks ----
  ybreaks <- pretty(
    c(region$ymin, region$ymax),
    n = 4
  )
  
  ybreaks <- ybreaks[
    ybreaks >= region$ymin &
      ybreaks <= region$ymax
  ]
  
  
  ## Plot ----
  p <- ggplot() +
    
    ## Ocean background ----
  geom_rect(
    xmin = region$xmin,
    xmax = region$xmax,
    ymin = region$ymin,
    ymax = region$ymax,
    fill = "gray94",
    colour = NA
  ) +
    
    ## CTX prediction ----
  ## Already smoothed for display if enabled.
  
  geom_raster(
    data = df,
    aes(
      x = x,
      y = y,
      fill = value
    ),
    interpolate = FALSE,
    na.rm = TRUE
  ) +
    
    ## Land polygons ----
  geom_polygon(
    data = extra_world_regions[[region_name]],
    aes(
      x = long,
      y = lat,
      group = group
    ),
    fill = "black",
    colour = "black",
    linewidth = 0.15,
    inherit.aes = FALSE
  ) +
    
    ## CTX palette ----
  scale_fill_gradientn(
    colours = cpal,
    limits = ctx_limits,
    oob = scales::squish,
    na.value = "transparent",
    name = params$ctx_label
  ) +
    
    ## Longitude axis ----
  scale_x_continuous(
    breaks = xbreaks,
    labels = lon_labels_extra
  ) +
    
    ## Latitude axis ----
  scale_y_continuous(
    breaks = ybreaks,
    labels = lat_labels_extra
  ) +
    
    ## Map extent ----
  coord_fixed(
    xlim = c(region$xmin, region$xmax),
    ylim = c(region$ymin, region$ymax),
    expand = FALSE
  ) +
    
    ## Labels ----
  labs(
    title = plot_title,
    x = NULL,
    y = NULL
  ) +
    
    ## Theme ----
  theme_bw(base_size = 11) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11
      ),
      
      panel.background = element_rect(
        fill = "gray94",
        colour = NA
      ),
      
      panel.grid = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        size = 10
      ),
      
      legend.text = element_text(
        size = 9
      ),
      
      axis.text = element_text(
        size = 9
      )
    )
  
  
  ## Add sampling points ----
  if (show_sites && nrow(site_df) > 0) {
    
    p <- p +
      
      geom_point(
        data = site_df,
        aes(
          x = lon360,
          y = lat
        ),
        shape = 21,
        size = 2.5,
        stroke = 0.8,
        fill = "white",
        colour = "black",
        inherit.aes = FALSE
      )
    
  }
  
  
  ## Optional sampling site labels ----
  if (
    extra_params$show_site_labels &&
    nrow(site_df) > 0
  ) {
    
    p <- p +
      
      geom_text(
        data = site_df,
        aes(
          x = lon360,
          y = lat,
          label = site
        ),
        colour = "black",
        size = 3,
        nudge_y = 0.15,
        check_overlap = TRUE,
        inherit.aes = FALSE
      )
    
  }
  
  
  return(p)
  
}


## ============================================================
## 9. Multi-panel figure function
## ============================================================

make_extra_figure <- function(
    pred_list,
    selected_regions = extra_region_order
) {
  
  plots <- list()
  
  panel_i <- 0
  
  
  for (region_name in selected_regions) {
    
    for (scenario in scenarios) {
      
      panel_i <- panel_i + 1
      
      region <- extra_regions[[region_name]]
      
      
      ## Panel letter ----
      global_panel_i <- (
        match(region_name, extra_region_order) - 1
      ) * length(scenarios) +
        match(scenario, scenarios)
      
      panel_letter <- LETTERS[global_panel_i]
      
      
      ## Panel title ----
      if (scenario == "histclim") {
        
        panel_title <- glue::glue(
          "{panel_letter}. Historical - {region$label}"
        )
        
      } else {
        
        panel_title <- glue::glue(
          "{panel_letter}. {scenario_labels[[scenario]]}"
        )
        
      }
      
      
      ## Plot ----
      plots[[panel_i]] <- plot_ctx_region_extra(
        r = pred_list[[region_name]][[scenario]],
        region_name = region_name,
        plot_title = panel_title
      )
      
    }
    
  }
  
  
  ## Combine panels ----
  p <- patchwork::wrap_plots(
    plots,
    ncol = 3,
    byrow = TRUE,
    guides = "collect"
  ) &
    theme(
      legend.position = "bottom"
    )
  
  
  return(p)
  
}


# ## ============================================================
# ## 10. Combined nine-panel bathymetry-masked figure
# ## ============================================================
# 
# p_extra_regions <- make_extra_figure(
#   pred_list = extra_display_preds,
#   selected_regions = extra_region_order
# )
# 
# 
# ## Display ----
# print(p_extra_regions)
# 
# 
# ## Save PNG ----
# ggsave(
#   filename = file.path(
#     extra_out_dir,
#     "CTX_bathy_masked_Tonga_FP_Rarotonga_9panel.png"
#   ),
#   plot = p_extra_regions,
#   width = extra_params$figure_width,
#   height = extra_params$figure_height,
#   dpi = extra_params$figure_dpi,
#   bg = "white"
# )
# 
# 
# ## Save PDF ----
# ggsave(
#   filename = file.path(
#     extra_out_dir,
#     "CTX_bathy_masked_Tonga_FP_Rarotonga_9panel.pdf"
#   ),
#   plot = p_extra_regions,
#   width = extra_params$figure_width,
#   height = extra_params$figure_height,
#   bg = "white"
# )
# 
# 
# ## ============================================================
# ## 11. Individual regional three-panel figures
# ## ============================================================
# 
# for (region_name in extra_region_order) {
#   
#   ## Create figure ----
#   p_region <- make_extra_figure(
#     pred_list = extra_display_preds,
#     selected_regions = region_name
#   )
#   
#   
#   ## Display ----
#   print(p_region)
#   
#   
#   ## Save PNG ----
#   ggsave(
#     filename = file.path(
#       extra_out_dir,
#       glue::glue(
#         "CTX_bathy_masked_{region_name}_3panel.png"
#       )
#     ),
#     plot = p_region,
#     width = 15,
#     height = 5.5,
#     dpi = extra_params$figure_dpi,
#     bg = "white"
#   )
#   
#   
#   ## Save PDF ----
#   ggsave(
#     filename = file.path(
#       extra_out_dir,
#       glue::glue(
#         "CTX_bathy_masked_{region_name}_3panel.pdf"
#       )
#     ),
#     plot = p_region,
#     width = 15,
#     height = 5.5,
#     bg = "white"
#   )
#   
# }
# 
# 
# ## ============================================================
# ## 12. Combined unrestricted comparison
# ## ============================================================
# 
# p_extra_unrestricted <- make_extra_figure(
#   pred_list = extra_display_original,
#   selected_regions = extra_region_order
# )
# 
# 
# ## Display ----
# print(p_extra_unrestricted)
# 
# 
# ## Save PNG ----
# ggsave(
#   filename = file.path(
#     extra_out_dir,
#     "CTX_unrestricted_Tonga_FP_Rarotonga_9panel.png"
#   ),
#   plot = p_extra_unrestricted,
#   width = extra_params$figure_width,
#   height = extra_params$figure_height,
#   dpi = extra_params$figure_dpi,
#   bg = "white"
# )
# 
# 
# ## Save PDF ----
# ggsave(
#   filename = file.path(
#     extra_out_dir,
#     "CTX_unrestricted_Tonga_FP_Rarotonga_9panel.pdf"
#   ),
#   plot = p_extra_unrestricted,
#   width = extra_params$figure_width,
#   height = extra_params$figure_height,
#   bg = "white"
# )
# 
# 
# ## ============================================================
# ## 13. Final summary
# ## ============================================================
# 
# ## Retained prediction cells ----
# print(extra_retention_summary)
# 
# 
# ## Sampling locations ----
# print(
#   extra_sites %>%
#     dplyr::select(
#       region,
#       site,
#       lon360,
#       lat,
#       toxin_data
#     )
# )
# 
# 
# message(
#   "Tonga, French Polynesia and Rarotonga figures complete."
# )
# 
# message(
#   glue::glue(
#     "Outputs saved to: {extra_out_dir}"
#   )
# )
# ```
# 
# ### Two things to note
# 
# **1. French Polynesia is deliberately broad.** Its extent includes all eight sampling locations you provided, from Nuku Hiva in the Marquesas to the Gambier Islands. This is useful for showing regional differences, but individual islands and shallow habitats will appear relatively small.
# 
# **2. Rarotonga may still have very few retained prediction cells.** This is a limitation of your ~0.083° prediction grid relative to the island's narrow shelf, rather than a plotting error. The saved `CTX_bathy_retention_extra_regions.csv` will show how many cells survive masking.
# 
# The updated plotting function uses `coord_fixed()`, so it avoids the `coord_sf()` degree-label calculation that caused your previous error. It also preserves your existing red-high CTX palette and uses `interpolate = FALSE` for rendering, since the display rasters have already been upsampled.







## ============================================================
## Additional regions: Tonga and French Polynesia only
##
## A-C: Tonga
## D-F: French Polynesia
##
## Historical, SSP126, SSP585
## Bathymetry-only mask (0-200 m)
## ============================================================


## 1. Remove Rarotonga ----
extra_regions$rarotonga <- NULL

extra_region_order <- c(
  "tonga",
  "french_polynesia"
)

extra_sites <- extra_sites %>%
  dplyr::filter(
    region != "rarotonga"
  )


## ============================================================
## 2. Combined six-panel figure
## ============================================================

p_extra_regions <- make_extra_figure(
  pred_list = extra_display_preds,
  selected_regions = extra_region_order
)

print(p_extra_regions)


## Save PNG ----
ggsave(
  filename = file.path(
    extra_out_dir,
    "CTX_bathy_masked_Tonga_FP_6panel.png"
  ),
  plot = p_extra_regions,
  width = 15,
  height = 9,
  dpi = extra_params$figure_dpi,
  bg = "white"
)


## Save PDF ----
ggsave(
  filename = file.path(
    extra_out_dir,
    "CTX_bathy_masked_Tonga_FP_6panel.pdf"
  ),
  plot = p_extra_regions,
  width = 15,
  height = 9,
  bg = "white"
)


## ============================================================
## 3. Save separate three-panel figures
## ============================================================

for (region_name in extra_region_order) {
  
  p_region <- make_extra_figure(
    pred_list = extra_display_preds,
    selected_regions = region_name
  )
  
  print(p_region)
  
  
  ## Save PNG ----
  ggsave(
    filename = file.path(
      extra_out_dir,
      glue::glue(
        "CTX_bathy_masked_{region_name}_3panel.png"
      )
    ),
    plot = p_region,
    width = 15,
    height = 5.5,
    dpi = extra_params$figure_dpi,
    bg = "white"
  )
  
  
  ## Save PDF ----
  ggsave(
    filename = file.path(
      extra_out_dir,
      glue::glue(
        "CTX_bathy_masked_{region_name}_3panel.pdf"
      )
    ),
    plot = p_region,
    width = 15,
    height = 5.5,
    bg = "white"
  )
  
}


## ============================================================
## 4. Unrestricted comparison: Tonga and French Polynesia
## ============================================================

p_extra_unrestricted <- make_extra_figure(
  pred_list = extra_display_original,
  selected_regions = extra_region_order
)

print(p_extra_unrestricted)


## Save PNG ----
ggsave(
  filename = file.path(
    extra_out_dir,
    "CTX_unrestricted_Tonga_FP_6panel.png"
  ),
  plot = p_extra_unrestricted,
  width = 15,
  height = 9,
  dpi = extra_params$figure_dpi,
  bg = "white"
)


## Save PDF ----
ggsave(
  filename = file.path(
    extra_out_dir,
    "CTX_unrestricted_Tonga_FP_6panel.pdf"
  ),
  plot = p_extra_unrestricted,
  width = 15,
  height = 9,
  bg = "white"
)


## ============================================================
## 5. Final summary
## ============================================================

extra_retention_summary <- extra_retention_summary %>%
  dplyr::filter(
    region %in% extra_region_order
  )

print(extra_retention_summary)

message("Tonga and French Polynesia figures complete.")
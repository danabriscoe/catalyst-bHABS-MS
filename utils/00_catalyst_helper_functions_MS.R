# 00 catalyst ms helper functions.R


# get ncs files fdates
get_fdates <- function(ncsIn, pluck_idx){
  ret <- ncsIn %>% 
    str_split(., "_") %>%
    purrr::map_chr(~ pluck(., pluck_idx)) %>%
    substr(., start=1, stop=10)
  return(ret)    
}


# dkb note: this works as of 5 Dec 2024. but was a total nightmare. make sure varnames line: "-v" is included for multivariables.
getNCDF_glorys <- function(productId, 
                               serviceId,
                               varnames,
                               lon, lat, dt,
                               ncpath) {
  
  # Output filename    
  out_name = paste0(ncpath, "/", "cmems_glorys_global_multiyear_phy_full_",dt, "_catalyst",".nc", sep="") # use "phy_full" to signify full spatial extents for MS (covers japan to nz")
 
  if (!file.exists(str_c(ncpath, "/", out_name))) {
    
    # ===================== Method 1. Copernicus Marine Client command ==============================
    command <- paste (path_copernicus_marine_toolbox, " subset -i", productId,                    
                      "-x", lon[1], "-X", lon[2],                  
                      "-y", lat[1], "-Y", lat[2],
                      "-t", dt, "-T", dt,
                      # "-z", depth[1], "-Z", depth[2],                    
                      "-v", varnames[1], "-v", varnames[2], 
                      "-o", ncpath, "-f", out_name, 
                      "--force-download", sep = " ", "--username", glorys_key$user, "--password", glorys_key$pwd)
    
    print(paste("======== Download starting on",dt,"========"))
    
    # print(command)
    
    return(system(command, intern = TRUE))
    
    print(paste("============= Download completed! File is stored in ",out_dir,"=============", sep=" "))
    
  } else {
    message("data already downloaded! \n Located at:\n", str_c(ncpath, out_name, sep = "/"))
  }
  
}


# get predDates
getPredDates <- function(x, idx){
  x %>% 
    str_split(., "_") %>% 
    purrr::map_chr(~ pluck(., idx)) %>% 
    unique()
}


# loadRData with new name. source: 'https://stackoverflow.com/questions/5577221/can-i-load-a-saved-r-object-into-a-new-object-name'
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


make180 <- function(lon){
  isnot360<-min(lon)<0
  if (!isnot360) {
    ind<-which(lon>180)
    lon[ind]<-lon[ind]-360
  }
  return(lon)
}

make360<- function(x) {x %% 360}
to360 <- make360

# ## Prep rasterZ
# prepRasterZ <- function(rs = ras_stack, dt_idx, .incl_names=FALSE){
#   ras_names_yrmon <- as.yearmon(dt_idx)
#   rs <- setZ(rs, ras_names_yrmon)
#   # names(ssta_monthly_stack) <- as.character(ras_names_yrmon)  # mon-yr abbreb
#   if(.incl_names){
#     names(rs) <- as.character(dt_idx) # full Xyr-mon-d 
#   }
#   return(rs)
# }


# process_glorys_ncs (formerly: 'brick ncs')
process_glorys_ncs <- function(ncIn, varname, depth_level, save_grd_dir=NULL) {
  lst_by_var_depth <-
    lapply(
      1:length(ncIn),
      function(i) {
        ret = brick(ncIn[i], varname = varname, lvar = "depth", level = depth_level)
        if(!is.null(save_grd_dir)){
          fname = tools::file_path_sans_ext(basename(ret@file@name))
          if(depth_level == 1){
            fname_depth = '0m'
          } else if(depth_level == 18){
            fname_depth = '50m'
          } else if(depth_level == 22){
            fname_depth = '100m'
          }
          terra::writeRaster(ret, file = str_c(save_grd_dir, "/", fname, "_", varname, "_", fname_depth), overwrite = T)
        }
        return(ret)
      }
    )
  rst_by_var_depth <- brick(lst_by_var_depth)
  
  return(rst_by_var_depth)
}


# process_gcm_ncs
process_gcm_ncs <- function(nclist=nclist, location=location, varname=varname, ann_type = "ann"){
  
  ras_list <- lapply(1:length(nc_list[[location]]), function(x) raster::brick(nc_list[[location]][x], varname = varname))
  names(ras_list) <- vapply(strsplit(basename(nc_list[[location]]),"\\."), `[`, 1, FUN.VALUE=character(1)) %>% str_replace(., ann_type, glue("{ann_type}_{varname}"))
  
  return(ras_list)
}


# points to raster (required for getContours to work on gcm rasters) - for ggplot
pts2ras <- function(df){
  coordinates(df) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(df) <- TRUE
  # coerce to raster
  rasterDF <- raster(df)
  return(rasterDF)
}

# raster to points for ggplot
ras2pts <- function(x) {
  x_spdf <- as(x, "SpatialPixelsDataFrame")
  x_df <- as.data.frame(x_spdf)
  colnames(x_df) <- c("value", "x", "y")
  return(x_df)
  
  
  
## Plot functions -----
plot_var <- function(data, mapdata, bbox, cpal = rev(rainbow(100)), .interp=FALSE, .legend = TRUE) {
  g <- ggplot() +
    
    ## geometries
    # raster data
    geom_tile(data = data, aes(x = x, y = y, fill = value), na.rm = FALSE, interpolate = .interp) +
    
    # geom_raster(data = data, aes(x = x, y = y, fill = value), na.rm = FALSE, interpolate = .interp) +
    scale_fill_gradientn(colours = cpal, na.value = "white") +
    
    # basemap
    geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "black", fill = "black") +
    
    # set scales, coords
    scale_x_continuous(breaks = seq(bbox$xmin, bbox$xmax, by = 10)) +
    coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE, crs = sf::st_crs(4326)) +
    # set theme
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # plot.margin = unit(c(0.1, 0.01, 0.1, 0.01), "null"),
      plot.margin = unit(c(0.02, 0.03, 0.02, 0.02), "null"),
      plot.caption = element_text(hjust = 0)
    ) +
    # labels
    labs(
      x = "\n Longitude",
      y = "Latitude \n"
    ) +
    if (!.legend) {
      theme(legend.position = "none")
    }
  
  return(g)
}


# add color bar
addColorBar <- function(x, cpal, limits, breaks, cbar_labels, barwidth = 1, barheight = 31, plot_type = 'continuous') {
  if (plot_type == "discrete") {
    x +
      scale_fill_manual(
        values = cpal,
        labels = cbar_labels, drop = F, na.translate = F,
        guide = guide_legend(
          label.vjust = +1.2, barwidth = barwidth, barheight = barheight,
          frame.colour = "black", ticks.colour = "black", ncol = 1, reverse = T
        )
      ) +
      theme(legend.key = element_rect(color = "gray80", fill = "white"))
  } else if (plot_type == "continuous") {
    x +
      scale_fill_gradientn(
        colours = cpal, # name = "",
        limits = limits,
        breaks = breaks,
        labels = format(cbar_labels),
        na.value = "#440154FF", #"gray20",
        guide = guide_colourbar(
          barwidth = barwidth, barheight = barheight,
          frame.colour = "gray70",
          ticks.colour = "snow"
        )
      )
  }
}


# add contours major (original)
addContours_major <- function(x, contours, c_show, bbox, contour_color = "snow") {
  xCont <-
    x +
    geom_sf(data = contours %>% filter(level %in% c_show), colour = contour_color, size = 1.25) +
    geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "black", fill = "black") + # keep land on top of contours
    
    ggsflabel::geom_sf_label(
      data = contours %>% filter(level %in% c_show), aes(label = level), size = 4, alpha = 0.99,
      # additional parameters are passed to geom_label_repel()
      nudge_x = +0, nudge_y = +0
    ) +
    coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) #+ 
  
  return(xCont)
}

# add contours minor
addContours_minor <- function(x, contours, c_show, bbox, contour_color = "gray65") {
  xCont <-
    x +
    geom_sf(data = contours %>% filter(level %in% c_show), colour = contour_color, size = 0.25, alpha = 0.5) +
    geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "black", fill = "black") + # keep land on top of contours
    coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) #+ 
  
  return(xCont)
}


# add contours major Nudge
addContours_major_nudge <- function(x, contours, c_show, bbox, line_size = 1.25, label_size=4, nudge_x=nudge_x, nudge_y=nudge_y, contour_color = "snow") {
  xCont <-
    x +
    geom_sf(data = contours %>% filter(level %in% c_show), colour = contour_color, size = line_size) +
    geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "black", fill = "black") + # keep land on top of contours
    
    ggsflabel::geom_sf_label(
      data = contours %>% filter(level %in% c_show), aes(label = level), size = label_size, alpha = 0.99,
      # additional parameters are passed to geom_label_repel()
      nudge_x = nudge_x, nudge_y = nudge_y
    ) +
    coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) #+ 
  
  return(xCont)
}


# # get contours
getContours <- function(rastIn, levels) {
  x_contour <- raster::rasterToContour(rastIn, levels = levels) %>%
    st_as_sf(.) %>%
    mutate(level = as.numeric(as.character(level)))
  return(x_contour)
}


# # plot all together
get_plot <- function(data, ras, mapdata, cpal, bbox, .legend = TRUE, .contours_major = NULL, .contours_minor = NULL, .title = TRUE) {
  g <-
    suppressMessages(plot_var(data = data, mapdata, cpal = cpal, bbox = bbox, .legend = .legend) %>%
                       addColorBar(., cpal, limits, breaks, cbar_labels, barheight = 28, plot_type = "continuous") + labs(fill = "°C"))
  
  if (!is.null(.contours_minor) & is.null(.contours_major)) {
    gg <- g %>%
      addContours_minor(x = ., contours = getContours(ras, levels = .contours_minor), c_show = .contours_minor, bbox = bbox, contour_color = "snow")
  } else if (!is.null(.contours_major) & is.null(.contours_minor)) {
    gg <- g %>%
      addContours_major(x = ., contours = getContours(ras, levels = .contours_major), c_show = .contours_major, bbox = bbox, contour_color = "gray85")
  } else if (!is.null(.contours_minor) & !is.null(.contours_major)) {
    gg <- g %>%
      addContours_minor(x = ., contours = getContours(ras, levels = .contours_minor), c_show = .contours_minor, bbox = bbox, contour_color = "snow") %>%
      addContours_major(x = ., contours = getContours(ras, levels = .contours_major), c_show = .contours_major, bbox = bbox, contour_color = "gray85")
  } else {
    gg <- g
  }
  
  if (.title) {
    title_name <- str_c(
      # sub(".*?_(.*)", "\\1", fname_split) %>% 
      str_replace_all(fname_split, "\\_", " ") %>% 
        str_replace_all(., c("japan|nz|histclim"), '') %>%
        str_replace_all(., c("thetao|sst"), "Temperature") %>% 
        str_replace(., c("so|sss"), "Salinity") %>% 
        str_squish(.) %>% 
        str_replace_all(., c("rcp"), "RCP ") 
    )
    if(grepl("glorys", fname_split, fixed = TRUE)){ 
      title_name <- str_c(title_name," Depth") %>% str_to_title()
    } else {
      title_name <- str_c(title_name)
    } 
    gg <- gg + labs(title = title_name)
  } else {
    gg
  }
  return(gg +
           scale_x_continuous(
             breaks = seq(bbox$xmin, bbox$xmax, by = bbox$xstep),
             labels = function(x) ifelse(x < 180, paste0(x, "°E"), ifelse(x > 180, paste0("", abs(make180(x)), "°W"), ifelse(x == 180, paste0(x, "°"), x)))
           ) +
           scale_y_continuous(
             breaks = seq(bbox$ymin, bbox$ymax, by = bbox$ystep),
             labels = function(x) paste0(ifelse(x < 0, paste0(abs(x), "°S"), ifelse(x > 0, paste0(x, "°N"), x)))
           )
  )
} # end get_plot

}
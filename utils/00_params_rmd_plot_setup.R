# params_rmd_plot_setup.R

# dynamically set param values based on var
if((params$source=='glorys') | (params$type=='histclim')){
  if((params$var=='thetao') | (params$var=='sst')){
    limits <- c(-3, 35)
    breaks <- seq(0, limits[2], 5)
    
    leg_units = "°C"
    
    # choose cbar ticks interval
    if(params$cbar_ticks_int==1){
      breaks <- seq(limits[1], limits[2], 1)
      cbar_labels <- c(rep("", 3), "0", rep("", 4), "5", rep("", 4), "10", rep("", 4), "15", rep("", 4), "20", rep("", 4), "25", rep("", 4), "30", rep("", 5))
    } else {
      breaks <- seq(0, limits[2], params$cbar_ticks_int)
      cbar_labels <- seq(0, limits[2], params$cbar_ticks_int)
    }
    
    # set contour labels
    contours_major <- switch(params$contours_major + 1, NULL, seq(0, limits[2], 5))
    contours_minor<- switch(params$contours_minor + 1, NULL, seq(0, limits[2], 1))
    
  } else if((params$var=='so') | (params$var=='sss')){
    # limits <- c(20, 38)
    # breaks <- seq(20, 38, 1)
    
    limits <- c(27, 36)
    breaks <- seq(27, 36, 1)
    
    leg_units = ""
    
    # choose cbar ticks interval
    cbar_labels <- breaks
    
    # set contour labels
    contours_major <- switch(params$contours_major + 1, NULL, seq(29, limits[2], 1))
    contours_minor<- switch(params$contours_minor + 1, NULL, seq(29, limits[2], 0.5))
    
  } # end set var params
} else if(params$type=='anomaly'){   # end histclim
  limits <- c(-4, 4)
  breaks <- seq(-4, 4, 1)
  
  # choose cbar ticks interval
  cbar_labels <- seq(-4, 4, 1)
  
  # set contour labels
  contours_major <- switch(params$contours_major + 1, NULL, seq(0, limits[2], 0.5))
  contours_minor<- switch(params$contours_minor + 1, NULL, seq(0, limits[2], 0.5))
  
  
} # end anomaly


# set bbox based on filename location
if (params$location == "japan") {
  bbox <- tibble(xmin = 120, xmax = 160, ymin = 15, ymax = 60, xstep = 10, ystep = 10)
} else if (params$location == "nz") {
  bbox <- tibble(xmin = 160, xmax = 210, ymin = -15, ymax = -60, xstep = 10, ystep = -10)
} else if (params$location == "full"){
  bbox <- tibble(xmin = 120, xmax = 210, ymin = -60, ymax = 60, xstep = 10, ystep = -10)
}




## set up manual contour functions for ggplot -- note these are 'label-less' ----
add_major_contour_lines <- function(df, contours, col='snow', linewidth = 1.25){
  list(
    geom_contour(data=df, aes(x=lon, y=lat, z = val), 
                 colour = col, linewidth = linewidth, breaks = c(contours)) 
  )
}

add_minor_contour_lines <- function(df, contours, col='gray85', linewidth = 0.5){
  list(
    geom_contour(data=df, aes(x=lon, y=lat, z = val), 
                 colour = col, linewidth = linewidth, breaks = c(contours)) 
  )
}

add_basemap <- function(){
  list(
    geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "black", fill = "black")
  )
}

add_border <- function(size=2){
  list(
    theme(panel.border = element_rect(colour = "black", fill=NA, size=size))
  )
}

resize_cbar <- function(x){
  list(
    guides(fill = guide_colourbar(barheight =x, ticks = TRUE,
                                  frame.colour = "black", ticks.colour = "black"))
  )
}

rename_legend <- function(title = "Growth \nRate \n(div/day)\n"){
  list(
    labs(fill = title)
  )
}

## set plot contour lines ----
# candidate m 6
if(!is.null(params$pred_model)){
# if(params$location == 'nz'){
#   if(params$pred_model == "growth_rate"){
#     .contours_major <- seq(0, 0.3, 0.1)
#     .contours_minor<- seq(0, 0.3, 0.05)
#     
#     # candidate model 6
#     limits=c(0, 0.3)   # this value is the rounded up to the thousandth of max pred df between glorys, ssp85, and ssp26 (fyi, ssp85 was highest)
#     breaks=seq(0,0.3,0.025)
#     cbar_labels=seq(0,0.3,0.025)
#     
#   } else if(params$pred_model == "CTX_rate"){
#     .contours_major <- seq(0, 0.6, 0.1)
#     .contours_minor<- seq(0, 0.6, 0.05)
#     # .contours_minor<- seq(0, 0.6, 0.1)
#     
#     # candidate model 6
#     limits=c(0, 0.62)   # this value is the rounded up to the thousandth of max pred df between glorys, ssp85, and ssp26 (fyi, ssp85 was highest)
#     breaks=seq(0,0.6,0.05)
#     cbar_labels=seq(0,0.6,0.05)
#     
#   } 
#   
# } else if(params$location == 'japan'){
#   if(params$pred_model == "growth_rate"){
#     .contours_major <- seq(0, 0.3, 0.1)
#     .contours_minor<- seq(0, 0.3, 0.05)
#     
#     # candidate model 1-5
#     limits=c(0, 0.25)   # this value is the rounded up to the thousandth of max pred df between glorys, ssp85, and ssp26 (fyi, ssp85 was highest)
#     breaks=seq(0,0.25,0.025)
#     cbar_labels=seq(0,0.25,0.025)
#     
#   } else if(params$pred_model == "CTX_rate"){
#     .contours_major <- seq(0, 0.6, 0.1)
#     .contours_minor<- seq(0, 0.6, 0.05)
#     # .contours_minor<- seq(0, 0.6, 0.1)
#     
#     # candidate model 6
#     limits=c(0, 0.62)   # this value is the rounded up to the thousandth of max pred df between glorys, ssp85, and ssp26 (fyi, ssp85 was highest)
#     breaks=seq(0,0.6,0.05)
#     cbar_labels=seq(0,0.6,0.05)
#     
#   }
#   
# } else if(params$location == 'full'){
    if(params$pred_model == "growth_rate"){
      .contours_major <- seq(0.1, 0.2, 0.1)
      .contours_minor<- seq(0.1, 0.3, 0.05)
      
      limits=c(0, 0.3)  
      breaks=seq(0,0.3,0.05)
      cbar_labels=seq(0,0.3,0.05)
      
    } else if(params$pred_model == "CTX_rate"){
      .contours_major <- seq(0, 0.6, 0.2)
      .contours_minor<- seq(0, 0.6, 0.05)
      # .contours_minor<- seq(0, 0.6, 0.1)
      
      # candidate model 6
      limits=c(0, 0.62)   # this value is the rounded up to the thousandth of max pred df between glorys, ssp85, and ssp26 (fyi, ssp85 was highest)
      breaks=seq(0,0.6,0.05)
      cbar_labels=seq(0,0.6,0.05)
      
    } 
  # }
}
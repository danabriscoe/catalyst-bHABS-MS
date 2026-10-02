# quick plot for new spatial extent -- UPDATE LATER TO BE FIG 1 script for ms!!
# quick plot for new spatial extent -- UPDATE LATER TO BE FIG 1 script for ms!!

## NOTE: CONSIDER ADDING IN INSET MAPS FOR JAPAN & NZ TO THE RIGHT??

## Load libraries ----
library(tidyverse)
library(raster)
library(ggplot2)
library(stringr)
library(khroma) # color pal
library(sf)
library(glue)
library(here)
library(rnaturalearth)
library(rnaturalearthhires)


## Source helper functions ----
source(
  file.path(
    here() %>% dirname(),
    'catalyst-bHABS',
    'utils',
    'catalyst_helper_functions.R'
  )
) # UPDATE ME!

source(
  here(
    'utils',
    '00_catalyst_helper_functions_MS.R'
  )
)


## Set fpath, pull temp files ----
ncIn <- "~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/sat_data/glorys/cmems_glorys_global_multiyear_phy_full_2024-09-01_catalyst.nc"


## INTERIM: Load as raster ----
ras <- raster(
  ncIn,
  varname = "thetao"
)


## Assign ras name == date ----
g_dates <- get_fdates(
  ncsIn = ncIn,
  pluck_idx = 11
) %>%
  as.Date()


## High-resolution coastline / land ----

# Natural Earth 1:10m land polygons
world <- rnaturalearth::ne_countries(
  scale = 10,
  returnclass = "sf"
)

# Keep original Natural Earth polygons
world_1 <- world

# Make a second copy
world_2 <- world

# Shift second copy +360 degrees longitude
# This places Alaska / North America into the
# 180–360 longitude range used by the raster
sf::st_geometry(world_2) <-
  sf::st_geometry(world_2) + c(360, 0)

# Restore CRS after shifting coordinates
sf::st_crs(world_2) <- sf::st_crs(world_1)

# Combine original and shifted copies
world_360 <- rbind(
  world_1,
  world_2
)


## Optional checks ----

# Check CRS
sf::st_crs(world_1)
sf::st_crs(world_2)

# Check extent
sf::st_bbox(world_360)

# Quick check of land polygons
ggplot() +
  geom_sf(
    data = world_360,
    fill = "black",
    colour = "black",
    linewidth = 0.15
  ) +
  coord_sf(
    xlim = c(120, 210),
    ylim = c(-60, 60),
    expand = FALSE,
    crs = sf::st_crs(4326)
  )


## Color palette ----
smooth_rainbow <- khroma::colour(
  "smooth rainbow"
)

cpal <- smooth_rainbow(
  length(seq(-2, 35, 1)),
  range = c(0, 0.9)
)


## Plot settings ----
bbox <- tibble(
  xmin = 120,
  xmax = 210,
  ymin = -60,
  ymax = 60,
  xstep = 10,
  ystep = -10
)

cbar_title <- 'SST (°C)\n'


## Quick conversion to df ----
tmp_df <- ras2pts(ras)

data <- tmp_df

.interp <- TRUE


## Plot -----
gp <- ggplot() +
  
  ## Raster data
  geom_raster(
    data = data,
    aes(
      x = x,
      y = y,
      fill = value
    ),
    na.rm = FALSE,
    interpolate = .interp
  ) +
  
  ## SST color scale
  scale_fill_gradientn(
    colours = cpal,
    breaks = seq(-2, 32, 2),
    guide = guide_colourbar(
      # barwidth = 1.5,
      # barheight = 38,
      barwidth = 1.0,
      barheight = 20,
      frame.colour = "gray20",
      ticks.colour = "gray30"
    ),
    na.value = "snow",
    name = cbar_title
  ) +
  
  ## High-resolution land
  geom_sf(
    data = world_360,
    fill = "black",
    colour = "black",
    linewidth = 0.15,
    inherit.aes = FALSE
  ) +
  
  ## Longitude breaks
  # Let coord_sf() handle E/W formatting
  scale_x_continuous(
    breaks = seq(
      bbox$xmin,
      bbox$xmax,
      by = 10
    )
  ) +
  
  ## Latitude breaks
  scale_y_continuous(
    breaks = seq(
      bbox$ymin,
      bbox$ymax,
      by = 20
    )
  ) +
  
  ## Coordinates
  coord_sf(
    xlim = c(
      bbox$xmin,
      bbox$xmax
    ),
    ylim = c(
      bbox$ymin,
      bbox$ymax
    ),
    expand = FALSE,
    crs = sf::st_crs(4326)
  ) +
  
  ## Theme
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.margin = unit(
      c(
        0.02,
        0.03,
        0.02,
        0.02
      ),
      "null"
    ),
    
    plot.caption = element_text(
      hjust = 0
    )
  ) +
  
  ## Labels
  labs(
    x = "\n Longitude",
    y = "Latitude \n",
    caption = str_c(
      "Sea Surface Temperature (SST) ",
      format(
        g_dates[1],
        "%B %Y"
      )
    )
  )


## Preview ----
gp


## Save fig ----
filenm <- here(
  "qmd",
  "images",
  glue(
    "glorys_SST_{format(g_dates[1], '%B_%Y')}.png"
  )
)

ggsave(
  gp,
  file = filenm,
  width = 14,
  # height = 8.5,   # comment out height to get auto aspect ratio set!
  bg = 'white'
)


## Crop image ----
source(
  here::here(
    'utils',
    'crop_image.R'
  )
)

crop_image(filenm)


## Add Gambierdiscus polynesiensis locations ----

# Read occurrence locations
gamb_locs <- read.csv(here::here(
                      "data", "raw", "gam_locations",
  "GambMapData_coords_only.csv")
) %>%
  mutate(
    lon_360 = if_else(
      lon < 0,
      lon + 360,
      lon
    )
  )


## Add locations to existing SST map ----

gp_w_locs <- gp +
  
  geom_point(
    data = gamb_locs,
    aes(
      x = lon_360,
      y = lat
    ),
    inherit.aes = FALSE,
    shape = 21,
    size = 4,
    stroke = 0.8,
    fill = "white",
    colour = "black"
  ) +
  
  ## Labels
  labs(
    caption = str_c(
      format(
        g_dates[1],
        "%B %Y "
      ), 
      "SST ",
      "with G.polynesiensis locations (white circles)"
    )
  )


## Preview ----
gp_w_locs



## save gp with locations
filenm_gp_locs <- here(
  "qmd",
  "images",
  glue(
    "study_area_with_sst_GambMap_Data.png"
  )
)

# ggsave(
#   gp_w_locs,
#   file = filenm_gp_locs,
#   width = 18,
#   # height = 8.5,   # comment out height to get auto aspect ratio set!
#   bg = 'white'
# )

# Save only — DON'T crop
ggsave(
  gp_w_locs,
  file= filenm_gp_locs,
  width = 14,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Then separately run your normal cropped version


## Crop image ----
source(
  here::here(
    'utils',
    'crop_image.R'
  )
)

crop_image(filenm_gp_locs)


### DKB UPDATED: 2 Oct 2026 ---- deal with low res 'world' coastline issue

# ## NOTE: CONSIDER ADDING IN INSET MAPS FOR JAPAN & NZ TO THE RIGHT??
# 
# 
# ## Load libraries ----
# library(tidyverse)
# library(raster)
# library(ggplot2)
# library(stringr)
# library(khroma) # color pal
# # library(cowplot)
# library(maps)
# library(sf)
# library(glue)
# library(here)
# 
# ## Source helper functions ----
# source(file.path(here() %>% dirname(), 'catalyst-bHABS','utils','catalyst_helper_functions.R')) # UPDATE ME!
# source(here('utils','00_catalyst_helper_functions_MS.R'))
# 
# ## Set fpath, pull temp files ----
# ncIn <- "~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/sat_data/glorys/cmems_glorys_global_multiyear_phy_full_2024-09-01_catalyst.nc"
# 
# 
# ## INTERIM: Load as raster:
# ras <- raster(ncIn, varname = "thetao")
# 
# 
# ## Assign ras name == date
# g_dates <- get_fdates(ncsIn = ncIn, pluck_idx = 11) %>% as.Date()
# 
# 
# # ## Set variables ----
# mapdata <- ggplot2::map_data("world", wrap = c(-25, 335), ylim = c(-75, 75)) %>%
#   filter(long >= 100 & long <= 210)
# 
# 
# # color pal
# smooth_rainbow <- khroma::colour("smooth rainbow")
# cpal <- smooth_rainbow(length(seq(-2, 35, 1)), range = c(0, 0.9))
# 
# # limits <- c(-3, 35)
# # breaks <- seq(-3, 35, 1)
# # cbar_labels <- c(rep("", 3), "0", rep("", 4), "5", rep("", 4), "10", rep("", 4), "15", rep("", 4), "20", rep("", 4), "25", rep("", 4), "30", rep("", 5))
# # # breaks = seq(0, limits[2], 5)
# # # cbar_labels = seq(0, limits[2], 5) #c(rep("",3), "0", rep("",4),"5", rep("",4),"10",rep("",4),"15",rep("",4),"20",rep("",4),"25",rep("",4), "30", rep("",2))
# # 
# # contours_major <- seq(0, limits[2], 5)
# # contours_minor <- seq(0, limits[2], 2)
# # # contours_min = NULL
# 
# 
# bbox <- tibble(xmin = 120, xmax = 210, ymin = -60, ymax = 60, xstep = 10, ystep = -10)
# cbar_title <- 'SST (°C)\n'
# 
# # quick conversion to df
# tmp_df <- ras2pts(ras)
# 
# data <- tmp_df
# .interp=TRUE
# 
# ## Plot -----
# gp <- ggplot() +
#   
#   ## geometries
#   # raster data
#   geom_raster(data = data, aes(x = x, y = y, fill = value), na.rm = FALSE, interpolate = .interp) +
#   scale_fill_gradientn(colours = cpal, 
#                        breaks=seq(-2,32,2),
#                        guide = guide_colourbar(
#                          barwidth = 1.5, barheight = 38,
#                          frame.colour = "gray20",
#                          ticks.colour = "gray30"
#                        ),
#                        na.value = "snow", name = cbar_title) +
#   
#   # basemap
#   geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "black", fill = "black") +
#   
#   # set scales, coords
#   scale_x_continuous(breaks = seq(bbox$xmin, bbox$xmax, by = 10)) +
#   coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE, crs = sf::st_crs(4326)) +
#   # set theme
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # plot.margin = unit(c(0.1, 0.01, 0.1, 0.01), "null"),
#     plot.margin = unit(c(0.02, 0.03, 0.02, 0.02), "null"),
#     plot.caption = element_text(hjust = 0)
#   ) +
#   # labels
#   labs(
#     x = "\n Longitude",
#     y = "Latitude \n"
#   ) +
#   # if (!.legend) {
#   #   theme(legend.position = "none")
#   # }
#   labs(caption = str_c("Sea Surface Temperature (SST) ", format(g_dates[1], "%B %Y")))
# 
# gp
# 
# ## Save fig ----
# 
# filenm = here("qmd", "images" , glue("glorys_SST_{format(g_dates[1], '%B_%Y')}.png"))
# 
# ggsave(gp, 
#        file = filenm, 
#        width=14,
#        # height=8.5,   # comment out height to get auto aspect ratio set!
#        bg = 'white')
# 
# source(here::here('utils','crop_image.R'))
# crop_image(filenm)
# 
# 
# 
# ### robinson proj ------
# library("ecmwfr")
# library("raster")
# library("tidyverse")
# library("rnaturalearth")
# library("sf")
# library("gganimate")
# library("gifski")
# library(RColorBrewer)
# library(terra)
# library(tidyterra)
# library(marmap)
# library(sfheaders)
# library(ggforce)
# library(sf)
# library(here)
# library(glue)
# 
# 
# sf_use_s2(FALSE)  # dkb note: need to add this line for below to work...
# 
# 
# robinson <- "+proj=robin +lon_0=-180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# 
# 
# world_pac <- ne_countries(scale = "medium", returnclass = "sf") %>%
#   st_break_antimeridian(lon_0 = -180) %>% 
#   # lwgeom::st_wrap_x(., 0, 360) |> st_set_crs(4326) 
#   lwgeom::st_wrap_x(.,0, 360) |> st_set_crs(4326) 
# 
# 
# 
# bbox <- st_polygon(list(rbind(c(20, 100),
#                               c( 60, 100),
#                               c( 60, 290),
#                               c(20, 290),
#                               c(20, 100)))) %>% 
#   st_segmentize(1/3) %>% # segmentize first...
#   st_sfc() %>% 
#   st_set_crs(4326) %>% # ... define CRS second
#   st_transform(robinson)
# 
# 
# # g <- raster(grd_dir, varname = "thetao")
# g <- raster(ncIn, varname = "thetao")
# 
# 
# dts <- seq.Date(as.Date('2024-09-01'), as.Date('2024-09-01'), by = "1 month")
# names(g) <- dts
# 
# # idx = which(names(g) == "X2022.01.15")
# 
# 
# 
# g_df <- g %>%
#   # projectRaster(., res=50000, crs = robinson) %>%
#   rasterToPoints %>%
#   as.data.frame() %>%
#   `colnames<-`(c("x", "y", names(g))) %>%
#   pivot_longer(cols = starts_with("X20"),
#                names_to = "layer",
#                values_to = "val") %>%
#   mutate(layer = substr(layer, 2, 14)) %>%
#   mutate(date = as.POSIXct(layer, "%Y.%m.%d", tz = "UTC") 
#   )
# 
# 
# 
# g_dates = unique(g_df$date)
# 
# 
# 
# # set up bboxes
# bb_enpac = sf::st_bbox(c(xmin = 120, xmax =210,
#                          ymin = -60, ymax = 60),
#                        crs = st_crs(4326))
# bb_enpac_robinson <- bb_enpac  %>%
#   st_as_sfc() %>%
#   st_transform(crs = as.character(robinson)) %>%
#   st_bbox()
# 
# 
# bb_enpac_robinson_df<-data.frame(
#   # lon = c(100,290,100,290),  
#   # lat = c(70,70,20,20)
#   lon = c(110,210,110,210),  
#   lat = c(60,60,-60,-60)
# ) %>% 
#   # st_segmentize(1/3) %>% 
#   st_as_sf(coords=c("lon","lat"), crs = 4326) %>% 
#   st_transform(robinson) %>% 
#   sf_to_df(fill=T)
# 
# 
# # world pac - 
# world_pac_r <- world_pac %>% 
#   st_transform(robinson) %>% 
#   st_intersection(bbox) 
# 
# ### plot ---
# 
# i=1
# 
# spat_ras_i =  as_tibble(g_df %>% filter(date == g_dates[i]), xy = TRUE) %>%
#   mutate(z = val) %>%
#   # mutate(z = case_when(
#   #   z > 5 ~ 5,
#   #   z < -5 ~ -5,
#   #   TRUE ~ z
#   # )) %>%
#   dplyr::select(c(x,y,z)) %>%
#   as_spatraster(.,crs=4326) %>%  
#   project(robinson)
# 
# world_sized_plot <- world_pac %>% 
#   st_transform(robinson) %>% 
#   st_intersection(bbox) |>
#   ggplot()+
#   
#   geom_sf()+
#   # geom_spatraster(data=spat_ras_i,aes(fill=z))+
#   geom_raster(data = spat_ras_i, aes(x = x, y = y, fill = z), na.rm = FALSE, interpolate = .interp) +
#   scale_fill_gradientn(colours = cpal, 
#                        breaks=seq(-2,32,2),
#                        guide = guide_colourbar(
#                          barwidth = 1.5, barheight = 38,
#                          frame.colour = "gray20",
#                          ticks.colour = "snow"
#                        ),
#                        na.value = "transparent", name = cbar_title) +
#   
#   # geom_sf(data = world_pac["level"],colour='black',
#   #         linetype='solid',
#   #         fill = 'snow',
#   #         fill = NA,
#   #         size=1) +
#   geom_sf(data = world_pac["level"],colour='black',
#           linetype='solid',
#           # fill = 'gray40',
#           fill = "black",
#           size=1) +
#   
#   # coord_sf(xlim=c(min(bb_enpac_robinson_df$x),max(bb_enpac_robinson_df$x)),
#   #          ylim=c(min(bb_enpac_robinson_df$y),max(bb_enpac_robinson_df$y)), crs = robinson, expand = F) +
# 
#   # 
#   # labs(caption = format(g_dates[i], "%Y %b")) #+
# labs(x = "Longitude", y = "Latitude", colour = cbar_title)
# 
# 
# # preview ----
# world_sized_plot
# 
# # saved with screenshot & moved to qmd/images folder: "glorys_SST_September_2024_world_map.png"
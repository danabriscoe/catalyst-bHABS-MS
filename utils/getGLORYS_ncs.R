# title: get_GLORYS_ncs.R
#
# description: r version to download GLORYS monthly data from copernicus. uses reticulate to call python
#   - vars:
#       - salinity: 'so'
#       - temperature: 'thetao'
#
# author: d k briscoe
#
# date: jan 2023 (original from catalyst bhabs repo)
# updated: dec 2024 (new copernicus api call)
# ___________________________________________________________________________________________________________

## Load libraries ----
pkgs <- c(
  "reticulate",
  "lubridate",
  "tidyverse",
  "config",
  "here"
)

lapply(pkgs, library, character.only = TRUE)

# # must for reticulate to work
# os <- import("os")
# os$listdir(".")
# 


## Source helper functions ----
source(here('code', '00_automate_EOV_helper_functions.R')) #### UPDATE ME!!!!
# source(file.path(here() %>% dirname(), 'cc-stretch-get-ncdf','code','00_automate_EOV_helper_functions.R')) # UPDATE ME!

# If your are using Rstudio, please follow the article to add your path to the Copernicus Marine Toolbox
path_copernicus_marine_toolbox = "/Users/briscoedk/opt/anaconda3/envs/R_env/bin/copernicusmarine"

## Set path for glorys downloads (special location) ----
ncpath <- "~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/sat_data/glorys" # NOTE, cm api doesn't like spaces! 

## Set Copernicus credentials 
glorys_key <- invisible(config::get(file = "~/github/catalyst-bHABs/utils/glorys_config.yml") )




# ================ Variables for your query ================

# Product ID
# productId = "cmems_mod_glo_phy_my_0.083deg_P1M-m" # use this up to 2021-06-01
productId = "cmems_mod_glo_phy_myint_0.083deg_P1M-m" # use this from 2021-07-01

# Service ID for the Motu request
serviceId = "GLOBAL_MULTIYEAR_PHY_001_030"

# Ocean Variable(s)
# Please keep the space at the beginning
# To add another variable please add " --variable your_var" after the last one 
varnames <- c("so", "thetao")

# Time range
# date_min = ymd(19930101) # end_date ---
# date_max = ymd(20210701) # end date

# date_min = ymd(20241101) # end_date ---
# date_max = ymd(20241201) # end date

date_min = ymd(20241101) # end_date --- dkb updated: 21 sept 2026
date_max = ymd(20260901) # end date


dates <- seq.Date(date_min, date_max, by = "1 month")

# Geographic area and depth level 
lon = list(120, 210)  # lon_min, lon_max
# lat = list(25, 50) # lat_min, lat_max
lat = list(-60, 60) # lat_min, lat_max
# depth = list(0.49, 155.8507) # depth_min, depth_max
# depths = list(0.49, 155.8507) # depth_min, depth_max -- ## dkb updated 21 sept 2026
depths = list(0.49402499198913574,155.85069274902344) # depth_min, depth_max -- ## dkb updated 21 sept 2026


for (i in 1:length(dates)) {
  tryCatch({
    getNCDF_glorys(productId = productId,
                       serviceId = serviceId,
                       varnames = varnames,
                       lon = lon,  # lon_min, lon_max
                       lat = lat, # lat_min, lat_max
                       dt = dates[i],
                       ncpath = ncpath
    )
  }, error = function(e){
    message('Caught an error!')
    print(e)
  }
  )
}


# ## dkb updated 21 sept 2026 -- copied/pasted directly from 'cc-stretch-get-netcdf/get_GLORYS.R'
# tstep = 'monthly'
# 
# for (i in 1:length(dates)) {
#   tryCatch({
#     getNCDF_glorys(productId = productId,
#                    serviceId = serviceId,
#                    varnames = varnames,
#                    lon = lon,  # lon_min, lon_max
#                    lat = lat, # lat_min, lat_max
#                    dt = dates[i],
#                    depth = depths,
#                    tstep = tstep,
#                    ncpath = ncpath
#     )
#   }, error = function(e){
#     message('Caught an error!')
#     print(e)
#   }
#   )
# }
# 


# 
# credentials_file <- os$path$expanduser("~/.copernicusmarine/.copernicusmarine-credentials")
# if (os$path$exists(credentials_file)) {
#   os$remove(credentials_file)
# }
# 
# cm$login(user, pwd)

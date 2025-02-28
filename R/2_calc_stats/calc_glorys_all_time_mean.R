# title: calc glorys all time mean.R
#
# description: script to calculate meansurfaces from glorys monthly data - 
#              outputs will then be used to:
#               1. plot all-time composite maps (jan/1993-dec/2024)
#               2. as inputs for regression modelling
#
# note: updated code from 'catalyst-bHABs' repo (feb 2025)
#
# author: d k briscoe
# 
# date: feb 2025
# __________________________________________________________________________________________________


## Load libraries ----
library(tidyverse)
library(raster)
library(terra)
library(here)

params <- tibble(depth = "_0m",
                 location = "full")

fpath <- '~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/sat_data/glorys/grd'
## Get list of .grd files
flist <- list.files(fpath, pattern = "grd$", full.names=T)
## Pull all files by location, var, and type
varfiles <- flist %>% imap(., ~ .x[grepl(str_c("(?=.*", params$depth,")(?=.*",params$location,")"), 
                                         .x, 
                                         perl = TRUE)]) %>% unlist()

## thetao
fl <- sapply("thetao", grep, varfiles)
fl_thetao <- varfiles[fl]
stack_thetao <- raster::stack(fl_thetao) %>% calc(., fun = mean, na.rm = T) 
 
spat_rast_thetao_mean <- as(stack_thetao, "SpatRaster")

## save 
grd_dir = here('data','processed')
fname = glue('glorys_all_time_mean_ras_{params$location}_1993-01-01_2024-12-31_thetao{params$depth}.grd')
# terra::writeRaster(spat_rast_thetao_mean, file = str_c(grd_dir, fname, sep='/'), overwrite = T) # strange min/max vals when saving as spatras. avoid and just use raster
terra::writeRaster(stack_thetao, file = str_c(grd_dir, fname, sep='/'), overwrite = T)



## so
fl <- sapply("so", grep, varfiles)
fl_so <- varfiles[fl]
stack_so <- raster::stack(fl_so) %>% calc(., fun = mean, na.rm = T) 

spat_rast_so_mean <- as(stack_so, "SpatRaster")

## save 
grd_dir = here('data','processed')
fname = glue('glorys_all_time_mean_ras_{params$location}_1993-01-01_2024-12-31_so{params$depth}.grd')
# terra::writeRaster(spat_rast_so_mean, file = str_c(grd_dir, fname, sep='/'), overwrite = T)
terra::writeRaster(stack_so, file = str_c(grd_dir, fname, sep='/'), overwrite = T)


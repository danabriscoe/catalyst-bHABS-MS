# title: calc glorys stats.R
#
# description: script to calculate all-time min, mean, max surfaces from glorys monthly data -  by location, variable, and desired depths
#              outputs will then be used to:
#               1. plot all-time composite maps (jan/1993-dec/2024)
#               2. as inputs for regression modelling
#
# note: updated code from 'catalyst-bHABs' repo (jan 2023)
#
# author: d k briscoe
# 
# date: jan 2025
# __________________________________________________________________________________________________


## Load libraries ----
library(tidyverse)
library(raster)
library(terra)
library(here)


## Source helper functions ----
source(file.path(here() %>% dirname(), 'catalyst-bHABS','utils','catalyst_helper_functions.R')) # UPDATE ME!
source(here('utils','00_catalyst_helper_functions_MS.R'))


# Set save output dir ----
grd_dir = here('data','processed')



## Calculate summary statistics by location, depth: min/mean/max ----

## 1. Temperature -----------------------

## Load rds files ----
load(here('data','interim','full_thetao_brick.rdata'))

ras = full_thetao_list
nc_source = 'glorys'

### 1.1 Min Temp ----
full_thetao_min <- lapply(ras, function(x) min(x, na.rm=FALSE)) %>% rename_elements(., stat='min', ras)
save_as_grd(var_name = 'full_thetao_min', fpath = grd_dir, nc_source = 'glorys')

### 1.2 Mean Temp ---- 
full_thetao_mean <- lapply(ras, function(x) mean(x, na.rm=FALSE)) %>% rename_elements(., stat='mean', ras)
save_as_grd(var_name = 'full_thetao_mean', fpath = grd_dir, nc_source = 'glorys')


### 1.3 Max Temp----
full_thetao_max <- lapply(ras, function(x) max(x, na.rm=FALSE)) %>% rename_elements(., stat='max', ras)
save_as_grd(var_name = 'full_thetao_max', fpath = grd_dir, nc_source = 'glorys')

# clear ws
rm(ras)
rm(list=ls(pattern="full_"))


## 2. Salinity  -----------------------
# Load rds files
load(here('data','interim','full_so_brick.rdata'))

ras = full_so_list

### 1.4 Min Salinity ----
full_so_min <- lapply(ras, function(x) min(x, na.rm=FALSE)) %>% rename_elements(., stat='min', ras)
save_as_grd(var_name = 'full_so_min', fpath = grd_dir, nc_source = 'glorys')

### 1.5 Mean Salinity ----
full_so_mean <-  lapply(ras, function(x) mean(x, na.rm=FALSE)) %>% rename_elements(., stat='mean', ras)
save_as_grd(var_name = 'full_so_mean', fpath = grd_dir, nc_source = 'glorys')

### 1.6 Max Salinity----
full_so_max <- lapply(ras, function(x) max(x, na.rm=FALSE)) %>% rename_elements(., stat='max', ras)
save_as_grd(var_name = 'full_so_max', fpath = grd_dir, nc_source = 'glorys')


## Clean up / gc
rm(ras)
rm(list=ls(pattern="full_"))



## fin --



# title: process_glorys_ncs_sfc_depth_only.R
#
# description: script to pre-process glorys monthly data by creating raster brick by location, variable, and desired depth -- Surface (0.49 m depth) only
#              outputs will then be used to generate all-time min, mean, max surfaces.
#
#             save output rds note: updated file ext to include '_expanded_lon_range.rds'
#
# note: updated code from 'catalyst-bHABs' repo (jan 2023)
#
# author: d k briscoe
# 
# date: jan 2025
# updated sept 2026 for 2025 glorys data incl
# updated oct 2026 for (1) surface depth extraction only; (2) expanded lon range for input glorys ncdfs
# __________________________________________________________________________________________________

## Load libraries ----
library(tidyverse)
library(raster)
library(ncdf4)
library(here)

## Source helper functions ----
source(file.path(here() %>% dirname(), 'catalyst-bHABS','utils','catalyst_helper_functions.R')) # UPDATE ME!
source(here('utils','00_catalyst_helper_functions_MS.R'))

# ## Set paths -----       
# fpath <- ncpath <- '~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/sat_data/glorys'
# save_grd_dir = str_c(fpath, '/grd') #'./data/raw/glorys'
# fpath <- ncpath <- '~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/sat_data/glorys'
fpath <- ncpath <- '~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/sat_data/glorys/expanded_lon'
save_grd_dir = str_c(fpath, '/grd') #'./data/raw/glorys'


## Set variables ----
locations = c('japan', 'nz', 'full') # add full = everything in between japan & nz
varnames = c('thetao', 'so')  # (temperature, salinity)
# desired_depths = c(0.5, 50, 100)  # depths of interest in meters 
desired_depths = c(0.5)  # depths of interest in meters 

## Get files ----
# list files by location: japan, nz, and full
# nc_list <- lapply(1:3, function(x) {list.files(fpath, pattern = locations[x], full.names=T)})
# names(nc_list) <- locations

# For ms, prob just want 'full' location = #3
nc_list <- lapply(3, function(x) {list.files(fpath, pattern = locations[x], full.names=T)})
names(nc_list) <- locations[3]


## Get depths ----
# list all possible depths within nc
nc_depths <- get_depths(nc_list[[1]][[1]], var='depth') # just using first file of nc list
print(nc_depths)

# determine closest match to desired depths & pull only those depths from ncs
# depth_idx = lapply(1:3, function(x) {findInterval(desired_depths[x], nc_depths)}) %>% unlist()  # multi depths
depth_idx = lapply(1, function(x) {findInterval(desired_depths[x], nc_depths)}) %>% unlist() # sfc depth only
depths <- nc_depths[depth_idx]
print(paste0('closest depths: ', round(depths,2), ' m'))


## Get FULL Spatial Extent ----
## 1) Stack full ncdfs by variable ----

# * Temperature ('thetao') by the 3 depths ----
# full_thetao_list <- lapply(1:3, function(x) process_glorys_ncs(ncIn = nc_list[['full']], varname = 'thetao', depth_level = depth_idx[x], save_grd_dir))   # multi depths
full_thetao_list <- lapply(1, function(x) process_glorys_ncs(ncIn = nc_list[['full']], varname = 'thetao', depth_level = depth_idx[x], save_grd_dir)) # sfc depth only
names(full_thetao_list) <- str_c('depth_', floor(desired_depths), 'm')


# * Salinity ('so') by the 3 depths----
# full_so_list <- lapply(1:3, function(x) process_glorys_ncs(ncIn = nc_list[['full']], varname = 'so', depth_level = depth_idx[x], save_grd_dir))   # multi depths
full_so_list <- lapply(1, function(x) process_glorys_ncs(ncIn = nc_list[['full']], varname = 'so', depth_level = depth_idx[x], save_grd_dir))   # sfc depth only
names(full_so_list) <- str_c('depth_', floor(desired_depths), 'm')


## 2) Save as .rds files ----
save(full_thetao_list, file = './data/interim/full_thetao_brick_expanded_lon_range.rdata')
save(full_so_list, file = './data/interim/full_so_brick_expanded_lon_range.rdata')


# fin --


# ## 1) Stack Japan ncdfs by variable ----
# # * Temperature ('thetao') ----
# japan_thetao_list <- lapply(1:3, function(x) process_glorys_ncs(ncIn = nc_list[['japan']], varname = 'thetao', depth_level = depth_idx[x], save_grd_dir))
# names(japan_thetao_list) <- str_c('depth_', floor(desired_depths), 'm')
# 
# 
# # * Salinity ('so') ----
# japan_so_list <- lapply(1:3, function(x) process_glorys_ncs(ncIn = nc_list[['japan']], varname = 'so', depth_level = depth_idx[x], save_grd_dir))
# names(japan_so_list) <- str_c('depth_', floor(desired_depths), 'm')
# 
# 
# 
# ## 2) Stack NZ ncdfs by variable ----
# # * Temperature ('thetao') ----
# nz_thetao_list <- lapply(1:3, function(x) process_glorys_ncs(ncIn = nc_list[['nz']], varname = 'thetao', depth_level = depth_idx[x], save_grd_dir))
# names(nz_thetao_list) <- str_c('depth_', floor(desired_depths), 'm')
# 
# 
# # * Salinity ('so') ----
# nz_so_list <- lapply(1:3, function(x) process_glorys_ncs(ncIn = nc_list[['nz']], varname = 'so', depth_level = depth_idx[x], save_grd_dir))
# names(nz_so_list) <- str_c('depth_', floor(desired_depths), 'm')
# 
# 
# ## 3) Save as .rds files ----
# save(japan_thetao_list, file = './data/interim/japan_thetao_brick.rdata')
# save(japan_so_list, file = './data/interim/japan_so_brick.rdata')
# 
# save(nz_thetao_list, file = './data/interim/nz_thetao_brick.rdata')
# save(nz_so_list, file = './data/interim/nz_so_brick.rdata')


# fin --

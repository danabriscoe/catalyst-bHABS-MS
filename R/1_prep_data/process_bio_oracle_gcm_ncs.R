# title: process gcm ncs.R
#
# description: script to pre-process global climate model netcdf data by creating raster brick by location, variable, and desired RCP scenarios
#              outputs will then be used to plot historic and projected temp, salinity as well as anomalies
#
# note: updated code from 'catalyst-bHABs' repo (feb 2023)
#
# author: d k briscoe
#
# description: script previously used cmip6 ann files (downloaded manually from source that no longer exists :/)
#              updated version (oct 2026 uses downloaded tiles from bio oracle's erddap (see 'get_ncdf_bio_oracle_gcm.R in the github get ncdf repo.))
#              fyi: metadata can also be found here: https://erddap.bio-oracle.org/erddap/griddap/so_ssp585_2020_2100_depthsurf.graph
#
# date: jan 2025
# updated: oct 2026 
# __________________________________________________________________________________________________

## Load libraries ----
library(tidyverse)
library(raster)
library(ncdf4)
library(here)

## Source helper functions ----
source(file.path(here() %>% dirname(), 'catalyst-bHABS','utils','catalyst_helper_functions.R')) # UPDATE ME!
source(here('utils','00_catalyst_helper_functions_MS.R'))


# Set paths ----
# ncpath
# ncpath <- here('data','raw','gcms','cmip6', 'ann')
# # ncpath <- here('data','raw','gcms','cmip6', 'stdann')

fpath <- ncpath <- '~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/gcm_data/bio_oracle_gcm/interim'
# output dir ----
grd_dir = '~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/gcm_data/bio_oracle_gcm/processed'

## Set variables ----
# locations = c('japan', 'nz', 'full') # add full = everything in between japan & nz
# # locations = c('full') # for ms use full = everything in between japan & nz

# new patthern for bio oracle clims: .tif
locations = '.tif'

## Get files ----
# # list files by location: japan, nz, and full
# nc_list <- lapply(1:3, function(x) {list.files(ncpath, pattern = locations[x], full.names=T)})
# names(nc_list) <- locations

# For ms, prob just want 'full' location = #3
# nc_list <- lapply(3, function(x) {list.files(ncpath, pattern = locations[x], full.names=T)})
# names(nc_list) <- locations[3]

nc_list <- lapply(1, function(x) {list.files(ncpath, pattern = locations[x], full.names=T)})
names(nc_list) <- locations[1]


## Process ncs ----
# # 1. RCP Scenario: 8.5 ----
# # * Full ---
# ## 1.1 Full -  Histclims RCP 8.5----
# full_ssp_histclim <- process_gcm_ncs(nclist=nclist, location='full', varname='histclim', ann_type = "ann")
# names(full_ssp_histclim)
# save_as_grd(var_name = 'full_ssp_histclim', fpath = grd_dir, nc_source = 'gcm')
# 
# ## 1.2 Full - Anomalies RCP.8.5 ----
# full_ssp_anomaly <- process_gcm_ncs(nclist, location='full', varname='anomaly', ann_type = "ann")
# names(full_ssp_anomaly)
# save_as_grd(var_name = 'full_ssp_anomaly', fpath = grd_dir, nc_source = 'gcm')


full_ssp_histclim <- process_gcm_tifs(nclist=nclist, location='.tif')

names(full_ssp_histclim)

save_as_grd(var_name = 'full_ssp_histclim', fpath = grd_dir, nc_source = 'gcm')

## fin --
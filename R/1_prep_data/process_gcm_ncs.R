# title: process gcm ncs.R
#
# description: script to pre-process global climate model netcdf data by creating raster brick by location, variable, and desired RCP scenarios
#              outputs will then be used to plot historic and projected temp, salinity as well as anomalies
#
# note: updated code from 'catalyst-bHABs' repo (feb 2023)
#
# author: d k briscoe
# 
# date: jan 2025
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
ncpath <- here('data','raw','gcms','cmip6', 'ann')
# ncpath <- here('data','raw','gcms','cmip6', 'stdann')

# output dir ----
grd_dir = here('data','processed')

## Set variables ----
locations = c('japan', 'nz', 'full') # add full = everything in between japan & nz
# locations = c('full') # for ms use full = everything in between japan & nz

## Get files ----
# # list files by location: japan, nz, and full
# nc_list <- lapply(1:3, function(x) {list.files(ncpath, pattern = locations[x], full.names=T)})
# names(nc_list) <- locations

# For ms, prob just want 'full' location = #3
nc_list <- lapply(3, function(x) {list.files(ncpath, pattern = locations[x], full.names=T)})
names(nc_list) <- locations[3]


## Process ncs ----
# 1. RCP Scenario: 8.5 ----
# * Full ---
## 1.1 Full -  Histclims RCP 8.5----
full_ssp_histclim <- process_gcm_ncs(nclist=nclist, location='full', varname='histclim', ann_type = "ann")
names(full_ssp_histclim)
save_as_grd(var_name = 'full_ssp_histclim', fpath = grd_dir, nc_source = 'gcm')

## 1.2 Full - Anomalies RCP.8.5 ----
full_ssp_anomaly <- process_gcm_ncs(nclist, location='full', varname='anomaly', ann_type = "ann")
names(full_ssp_anomaly)
save_as_grd(var_name = 'full_ssp_anomaly', fpath = grd_dir, nc_source = 'gcm')


## fin --
# # title: fit_models.R
# #
# # description: script to use regression model (from tn) and generate predictive maps
# #
# # note: updated code from 'catalyst-bHABs' repo (feb 2023)
# #
# # author: d k briscoe
# # 
# # date: jan 2025
# #_________________________________________________________________________________
# 
# 
# ## Load libraries ----
# library(tidyverse)
# library(readxl)
# library(mgcv)
# library(here)
# 
# 
# 
# ## Source helper functions -----
# source(file.path(here() %>% dirname(), 'catalyst-bHABS','utils','catalyst_helper_functions.R')) # UPDATE ME!
# source(here('utils','00_catalyst_helper_functions_MS.R'))
# 
# ## Set Params -----
# params <- list(source =  'glorys',
#                var  =  'thetao',
#                depth  =  '_0m',
#                scenario = 'ssp85',
#                type = 'histclim',
#                stat = 'mean',
#                location = 'full'#,
#                # location = 'nz',
#                # location = 'japan',
#                # pred_model = 'growth_rate',
#                # # pred_model = 'CTX_rate',
#                # contour_labs = FALSE,
#                # cbar_ticks_int =  1,
#                # contours_major = TRUE,
#                # contours_minor = FALSE,
#                # cbar = TRUE,
#                # include_title = TRUE,
#                # out_dir_plots = './figures/pred_glorys'
# )
# 
# 
# ## Load regression models -----
# 
# # source model (fyi, model fits from TN)
# source('./utils/get_gpoly_model.R')
# 
# ## Set spp name -----
# params$spp <- 'G.polynesiensis'
# 
# 
# ## Rename model fits -----
# ### 1. Growth rates ----
# model_fit_growth <- growth_GAM
# 
# ### 2. CTX ----
# model_fit_ctx <- CTX_GAM
# 
# 
# ## Save model fits ----
# ### 1. Growth rates ----
# save(model_fit_growth, file = here('data', 'model_fits', 'model_fit_growth.rds'))
# 
# ### 2. CTX ----
# save(model_fit_ctx, file = here('data', 'model_fits', 'model_fit_ctx.rds'))
# 
# 
# ## fin --
# 
# ## Set filepaths -----
# if(params$source=='glorys' & is.null(params$stat)){
#   ## Pull all var files
#   fpath <- '~/Dropbox/bkup_cawth/OneDrive/Catalyst_project_2021/data/sat_data/glorys/grd'
#   ## Get list of .grd files
#   flist <- lapply(1:length(params$var), function(x) {list.files(fpath, pattern = "grd$", full.names=T)})
#   ## Pull all files by location, var, and type
#   varfiles <- flist %>% imap(., ~ .x[grepl(str_c("(?=.*", params$depth,")(?=.*",params$location,")"), .x, perl = TRUE)]) %>% unlist()
#   
# } else if(params$source=='glorys' & params$stat=='mean'){
#   fpath <- here('data','processed')
#   flist <- lapply(1:length(params$var), function(x) {list.files(fpath, pattern = "grd$", full.names=T)})
#   
#   varfiles <- flist %>% imap(., ~ .x[grepl(str_c("(?=.*", params$depth,")(?=.*",params$location,")(?=.*",params$stat,")"), .x, perl = TRUE)]) %>% unlist()
#   
# } else if(params$source=='gcm'){
#   fpath <- "~/github/catalyst-bHABs/data/processed"
#   ## Get list of .grd files
#   flist <- lapply(1:length(params$var), function(x) {list.files(fpath, pattern = "grd$", full.names=T)})
#   ## Pull all files by location, var, and type under each RCP or SSP scenario
#   varfiles <- flist %>% imap(., ~ .x[grepl(str_c("(?=.*", params$depth,")(?=.*",params$location,")(?=.*",params$scenario,")(?=.*",params$type,")"), .x, perl = TRUE)]) %>% unlist()
# }

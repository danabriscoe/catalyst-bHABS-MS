# title: get_gpoly_model.R
#
# orig script: 'get_dummy_data_and_model.R'
# 
# description: placeholder until get regression model from tn
#
# author: dk briscoe
#
# date: feb 2023
# _______________________________________________________________________________________


library(tidyverse)
library(readxl)
library(mgcv)
library(GGally)


print('running model for: G.polynesiensis')

## 1. Load test growth data -----

## UPDATED TO USE (5 Apr 2023)
growth_rate_tbl <- read_excel(here::here("data","raw","growth_rates_test_data","growth_rates_test_data.xlsx")) %>%
  separate(2, c("temp", "salinity"), sep = ",") %>%
  rename(max_growth_rate   = 4,
         max_biomass_yield = 5) %>%
  dplyr::select(-c(species, max_biomass_yield)) %>%
  mutate_if(is.character,as.numeric) 


## PART 1: GROWTH RATE ----------
### 2. Fit models -----

### B) CANDIDATE 6
growth_GAM <- lm(max_growth_rate ~ 
                   temp + 
                   salinity +
                   
                   I(temp^2) +
                   I(salinity^2) +
                   
                   I(temp^3) +
                   I(salinity^3) +
                   
                   temp:salinity +
                   
                   I(salinity^2):temp +
                   I(temp^2):salinity,
                 data = growth_rate_tbl)

# # Candidate #6
# Growth rate = 14.569191734269 - 1.35260486440551t - 0.52151345521245s +
#   0.0474990577392411t2 + 0.00921822586835457s2 + 0.0225070780455982ts - 0.000579024969295755t3 - 0.0000812999138819055s3 - 0.000345172504733166t2s - 0.0000919784039270799ts2



# # manually fix from tomo's Growth rate regression (candidate 6) -- DKB UPDATED OCT 2023 -- NOTE!!!! USE THIS FOR FINAL PRODUCTS!
growth_GAM$coefficients[1] <- c(14.569191734269) # intercept
growth_GAM$coefficients[2] <- c(-1.35260486440551) # temp
growth_GAM$coefficients[3] <- c(-0.52151345521245) # sal
growth_GAM$coefficients[4] <- c(0.0474990577392411) # temp^2
growth_GAM$coefficients[5] <- c(0.00921822586835457) # sal^2

growth_GAM$coefficients[6] <- c(-0.000579024969295755) # temp^3
growth_GAM$coefficients[7] <- c(-0.0000812999138819055) # sal^3
growth_GAM$coefficients[8] <- c(0.0225070780455982) # temp*sal
growth_GAM$coefficients[10] <- c(-0.000345172504733166) # temp*sal^2
growth_GAM$coefficients[9] <- c(-0.0000919784039270799) # temp^2*sal



## Get coeffs ----
cc <- growth_GAM$coefficients

## Get equation ----
eqn <- paste("Y =", paste(cc[1], paste(cc[-1], names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e")



## PART 2 - TOTAL CTX PRODUCTION MODEL

CTX_GAM <- lm(max_growth_rate ~ 
                temp + 
                salinity +   
                I(temp^2) + 
                I(salinity^2) +
                I(temp^3) + 
                
                I(salinity^3) +
                temp:salinity +

                I(temp^2):salinity + 
                I(salinity^2):temp,
             data = growth_rate_tbl)

# # # Candidate #6
# Total CTXs production rate = 48.676196780726 - 4.12175600455083t - 1.86081995621889s + 0.148853385180593t2 + 0.0420391880987703s2 +
#   0.0519274951899574ts - 0.00194335195968719t3 - 0.000367676783299383s3 - 0.000557513639124216t2s - 0.000377976147960586ts2
# # 

# # manually fix from tomo's CTX regression (candidate 6) -- DKB UPDATED OCT 2023 -- NOTE!!!! USE THIS FOR FINAL PRODUCTS!
CTX_GAM$coefficients[1] <- c(48.676196780726) # intercept
CTX_GAM$coefficients[2] <- c(-4.12175600455083) # temp
CTX_GAM$coefficients[3] <- c(-1.86081995621889) # sal
CTX_GAM$coefficients[4] <- c(0.148853385180593) # temp^2
CTX_GAM$coefficients[5] <- c(0.0420391880987703) # sal^2

CTX_GAM$coefficients[6] <- c(-0.00194335195968719) # temp^3
CTX_GAM$coefficients[7] <- c(-0.000367676783299383) # sal^3

CTX_GAM$coefficients[8] <- c(0.0519274951899574) # temp*sal

CTX_GAM$coefficients[9] <- c(-0.000557513639124216) # temp^2*sal
CTX_GAM$coefficients[10] <- c(-0.000377976147960586) # temp*sal^2


## Get coeffs ----
cc_CTX_GAM <- CTX_GAM$coefficients

## Get equation ----
eqn_CTX_GAM <- paste("Y =", paste(cc_CTX_GAM[1], paste(cc_CTX_GAM[-1], names(cc_CTX_GAM[-1]), sep=" *", collapse=" + "), sep=" + "), "+ e")



## fin ------

## dkb notes: 17 Jun 2023 ---

# candidate model 4-5 coeffs
#−1.05974863418282−0.029313024864959s−0.00091599742938173s2+0.00820770665308019ts−0.000170382605821333t2s

# using candidate model 4-5 coefficients, but output is strange
# need to mask t/s rasters above/below temp and sal ranges of lab growth experiment?
# why are there negative prediction values?
# why are max growth prediction values an order of magnitude > than lab growth divisions? (ie 3.0 div/day vs 0.3 div/day)
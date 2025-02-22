# similar to 'get_gsilvae_model.R'
# orig script: 'get_dummy_data_and_model.R'


library(tidyverse)
library(readxl)
library(mgcv)
library(GGally)


print('running model for: G.silvae')


## 1. Load test growth data -----

# growth_tbl <- read_excel("./data/raw/growth_rates_test_data/growth_rates_test_data.xlsx") %>%
#   separate(2, c("temp", "salinity"), sep = ",") %>%
#   rename(max_growth_rate   = 4,
#          max_biomass_yield = 5) %>%
#   dplyr::select(-c(species, max_growth_rate)) %>%
#   mutate_if(is.character,as.numeric) 
# 
# # for consistency in ex:
# growth_tbl <- growth_tbl %>%
#   mutate(biomass = max_biomass_yield) %>%
#   # mutate(biomass = type2_cell_density) %>%
#   dplyr::select(c(temp, salinity, biomass))
# 
# 
# ## Check for collinarity ----
# ggpairs(growth_tbl[-3])
# 
# corpcor::cor2pcor(cov(growth_tbl[-3]))   # checks for partial collinearity
# 
# ## 2. Fit model -----
# growth_GAMM <- gam(biomass ~ 
#                      s(temp, k=3) +
#                      s(salinity, k=3) +
#                      te(temp, salinity),
#                    # family = gaussian(),
#                    # family = poisson(link="log"),
#                    # family = nb,
#                    family = tw,                       # note: using Tweedie to keep response positive
#                    # method = 'REML',                 # note: The default method uses generalized cross validation to fit the smooth. The restricted maximum likelihood method is more robust for small sample sizes. So generally in ecological datasets you’ll want to specify the “REML” method like this (source: https://www.seascapemodels.org/rstats/2021/03/27/common-GAM-problems.html)
#                    data=na.omit(growth_tbl))
# 
# 
# # summary(growth_GAMM)
# # gam.check(growth_GAMM)
# # concurvity(growth_GAMM, full = TRUE)
# # plot(growth_GAMM, residuals = TRUE, pch = 1)
# 
# ## notes: this is not a great model -- JUST A PLACEHOLDER TO BUILD OUT PREDICTIVE CODE UNTIL ACTUAL MODEL IS READY


## UPDATED TO USE (5 Apr 2023)
growth_rate_tbl <- read_excel("./data/raw/growth_rates_test_data/growth_rates_test_data.xlsx") %>%
  separate(2, c("temp", "salinity"), sep = ",") %>%
  rename(max_growth_rate   = 4,
         max_biomass_yield = 5) %>%
  dplyr::select(-c(species, max_biomass_yield)) %>%
  mutate_if(is.character,as.numeric) 

## PART 1: GROWTH RATE ----------
### A) CANDIDATE 1-5 : GSILVAE
growth_GAM <- lm(max_growth_rate ~ 
                   temp + 
                   salinity +
                   
                   # I(temp^2) +
                   I(salinity^2) +
                   
                   I(temp^3) +
                   I(salinity^3), #+
                   
                   # temp:salinity +
                   # 
                   # I(salinity^2):temp +
                   # I(temp^2):salinity,
                 data = growth_rate_tbl)

# # Candidate #6
# Growth rate = 14.569191734269 - 1.35260486440551t - 0.52151345521245s +
#   0.0474990577392411t2 + 0.00921822586835457s2 + 0.0225070780455982ts - 0.000579024969295755t3 - 0.0000812999138819055s3 - 0.000345172504733166t2s - 0.0000919784039270799ts2


# # manually fix from tomo's regression (candidate 4-5) -- DKB UPDATED SEPT 2023 -- USE THIS FOR FINAL PRODUCTS!
# growth_GAM$coefficients[1] <- c(-1.05974863418282) # intercept
# growth_GAM$coefficients[2] <- c(-0.029313024864959) # sal
# growth_GAM$coefficients[3] <- c(-0.00091599742938173) # sal^2
# growth_GAM$coefficients[4] <- c(0.00820770665308019) # temp*sal
# growth_GAM$coefficients[5] <- c(-0.000170382605821333) # temp^2*sal


# # manually fix from tomo's GSILVAE Growth rate regression (candidate 1-5) -- DKB UPDATED DEC 2023 -- NOTE!!!! USE THIS FOR FINAL PRODUCTS!
growth_GAM$coefficients[1] <- c(0.16948987049259) # intercept
growth_GAM$coefficients[2] <- c(0.0596747909302137) # temp
growth_GAM$coefficients[3] <- c(-0.14948512790646) # sal
# growth_GAM$coefficients[4] <- c(0.0474990577392411) # temp^2
growth_GAM$coefficients[4] <- c(0.00646136774992585) # sal^2

growth_GAM$coefficients[5] <- c(-0.0000322541931556056) # temp^3
growth_GAM$coefficients[6] <- c(-0.0000846013161940115) # sal^3

# 

# for (i in 1:6){
#   print(format(growth_GAM$coefficients[i], scientific = F))
# }

cc <- growth_GAM$coefficients
eqn <- paste("Y =", paste(cc[1], paste(cc[-1], names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e")



## PART 2 - TOTAL CTX PRODUCTION MODEL --- DKB DEC 2023 NOTE -- NO CTX GROWTH RATE MODELS FOR NOW -----

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



# for (i in 1:10){
#   print(format(CTX_GAM$coefficients[i], scientific = F))
# }

cc_CTX_GAM <- CTX_GAM$coefficients
eqn_CTX_GAM <- paste("Y =", paste(cc_CTX_GAM[1], paste(cc_CTX_GAM[-1], names(cc_CTX_GAM[-1]), sep=" *", collapse=" + "), sep=" + "), "+ e")

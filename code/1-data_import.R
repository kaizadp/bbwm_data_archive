library(googlesheets4)
library(tidyverse)



# file names --------------------------------------------------------------
STREAMS_ANNUAL = "data/streams/bbwm_stream_annual.csv"
STREAMS_MONTHLY = "data/streams/bbwm_stream_manual.csv"
STREAMS_ALL = "data/streams/bbwm_stream_all.csv"
DEP_ANNUAL = "data/deposition/bbwm_deposition_annual.csv"
DEP_MONTHLY = "data/deposition/bbwm_deposition_monthly.csv"
DEP_ALL = "data/deposition/bbwm_deposition_all.csv"

# DEPOSITION --------------------------------------------------------------
## annual
dep_annual_gs = 
  read_sheet("1qbo7J_UeuHhdsfBE4skzFmMvvyTonjwf_zNQ_0KbnOg") 
dep_annual = 
  dep_annual_gs %>% 
  mutate(NH4_eq_ha = round(NH4_eq_ha,1))
dep_annual %>% write.csv(DEP_ANNUAL, row.names = F, na="")  
  
## monthly
dep_monthly_gs = 
  read_sheet("1eEV7hrqJidF2wV5TlOxMRGXdWwzvBY_yAVkrUELAAec") 
dep_monthly_gs %>% write.csv(DEP_MONTHLY, row.names = F, na="") 

## all
dep_all_gs = 
  read_sheet("15YaRa83Iyj7mwKg_MfShbLyuf2vXQflNrxFag3GOldU") 
dep_all_gs %>% write.csv(DEP_ALL, row.names = F, na="")



# STREAMS -----------------------------------------
## annual
stream_annual_gs = 
  read_sheet("1Fm1_ZvZBpFpRlNPiPW1gxCIZV3SEeCQ9rWBzjGY2nPs") 

stream_annual = 
  stream_annual_gs %>% 
  mutate(H2O_1e6L = as.integer(H2O_1e6L),
         Ca_eq_ha = as.integer(Ca_eq_ha),
         Mg_eq_ha = as.integer(Mg_eq_ha),
         K_eq_ha = as.integer(K_eq_ha),
         Na_eq_ha = as.integer(Na_eq_ha),
         SO4_eq_ha = as.integer(SO4_eq_ha),
         Cl_eq_ha = as.integer(Cl_eq_ha),
         DOC = as.integer(DOC),

         NO3_eq_ha = round(NO3_eq_ha,1),
         NH4_eq_ha = round(NH4_eq_ha,1),
         inorganic_N_eq_ha = round(inorganic_N_eq_ha,1),
         Al = round(Al,1),
         H_eq_ha = round(H_eq_ha,1),
         ANC_eq_ha = round(ANC_eq_ha,1),
         HCO3_eq_ha = round(HCO3_eq_ha,1),
         Si = round(Si, 1)
         )

stream_annual %>% 
    write.csv(STREAMS_ANNUAL, row.names = F, na="")


## monthly
stream_monthly_gs = 
  read_sheet("1Otu18_jCOnLbL1Ax2fusEpv09km_2F8nhyDCrjAp6z4") 

stream_monthly = 
  stream_monthly_gs %>% 
  filter(year<=2016) 
stream_monthly %>% 
  write.csv(STREAMS_MONTHLY, row.names = F, na="")


## all
# stream_all_gs = 
#   read_sheet("13RPwYd53_GGyvBwZ7G90IB_De0tEv7zOuE9sKGgldm4") 


stream_all_gs2 = read.csv("3-bbwm_streams_all - Sheet1.csv")
stream_all = 
  stream_all_gs2  %>% 
  filter(year<=2016) %>% 
  mutate(total_N_ppm = round(total_N_ppm,2),
         NO3_ueq_L = round(NO3_ueq_L,2),
         )
stream_all %>% write.csv(STREAMS_ALL, row.names = F, na="")

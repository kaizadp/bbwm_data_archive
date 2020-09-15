library(googlesheets4)
library(tidyverse)



# file names --------------------------------------------------------------
STREAMS_ANNUAL = "data/streams/bbwm_stream_annual.csv"
DEP_ANNUAL = "data/deposition/bbwm_deposition_annual.csv"



# DEPOSITION --------------------------------------------------------------
dep_annual_gs = 
  read_sheet("1qbo7J_UeuHhdsfBE4skzFmMvvyTonjwf_zNQ_0KbnOg") 
dep_annual = 
  dep_annual_gs %>% 
  mutate(NH4_eq_ha = round(NH4_eq_ha,1))
dep_annual %>% write.csv(DEP_ANNUAL, row.names = F)  
  
  
  

# STREAMS -----------------------------------------
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
    write.csv(STREAMS_ANNUAL, row.names = F)

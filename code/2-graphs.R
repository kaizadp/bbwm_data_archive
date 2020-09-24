library(drake)
library(tidyverse)

STREAMS_ANNUAL = "data/streams/bbwm_stream_annual.csv"
STREAMS_MONTHLY = "data/streams/bbwm_stream_manual.csv"
STREAMS_ALL = "data/streams/bbwm_stream_all.csv"
DEP_ANNUAL = "data/deposition/bbwm_deposition_annual.csv"
DEP_MONTHLY = "data/deposition/bbwm_deposition_monthly.csv"
DEP_ALL = "data/deposition/bbwm_deposition_all.csv"





# FUNCTIONS ---------------------------------------------------------------
theme_kp <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0.05, size = 14),
          axis.text = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

clean_dep_annual = function(dep_annual_temp){
  dep_annual_temp %>% 
    select(WY, NO3_eq_ha, SO4_eq_ha) %>% 
    rename(nitrate = NO3_eq_ha,
           sulfate = SO4_eq_ha)
}
clean_stream_annual = function(stream_annual_temp){
  stream_annual_temp %>% 
    select(WY, watershed, NO3_eq_ha, SO4_eq_ha) %>% 
    rename(nitrate = NO3_eq_ha,
           sulfate = SO4_eq_ha)
}

clean_dep_annual2 = function(dep_annual_temp){
  dep_annual_temp2 = 
    dep_annual_temp %>% 
    select(WY, NH4_eq_ha, NO3_eq_ha, SO4_eq_ha, NH4_mani_eq_ha, SO4_mani_eq_ha) %>% 
    mutate(EB_N = NH4_eq_ha + NO3_eq_ha,
           WB_N = EB_N + NH4_mani_eq_ha,
           EB_S = SO4_eq_ha,
           WB_S = EB_S + SO4_mani_eq_ha) %>% 
    select(WY, EB_N, WB_N, EB_S, WB_S)
  
  dep_EB_temp = dep_annual_temp2 %>% 
    select(WY, starts_with("EB")) %>% 
    rename(N_in = EB_N,
           S_in = EB_S) %>% 
    mutate(watershed="EB")
  
  dep_WB_temp = dep_annual_temp2 %>% 
    select(WY, starts_with("WB")) %>% 
    rename(N_in = WB_N,
           S_in = WB_S) %>% 
    mutate(watershed="WB")
  
  rbind(dep_EB_temp, dep_WB_temp)
}
clean_stream_annual2 = function(stream_annual_temp){
  stream_annual_temp %>% 
    select(WY, watershed, NH4_eq_ha, NO3_eq_ha, SO4_eq_ha) %>% 
    mutate(NH4_eq_ha = replace_na(NH4_eq_ha, 0),
           N_out = (NH4_eq_ha+NO3_eq_ha),
           S_out = (SO4_eq_ha)) %>% 
    select(WY, watershed, N_out, S_out)
}

plot_annualdep_eb = function(dep_annual){
  dep_annual %>% 
    pivot_longer(-WY,
                 values_to = "eq_ha",
                 names_to = "species") %>% 
    ggplot(aes(x = WY, y = eq_ha, color = species))+
    geom_path(size=0.7, show.legend = F)+
    geom_point(size=2, stroke=1, alpha = 1)+
    scale_color_manual(values = soilpalettes::soil_palette("redox",2))+
    labs(x = "Water Year",
         y = "eq. per hectare",
         title = "ambient deposition")+
    theme_kp()+
    theme(legend.position = c(0.8, 0.8))+
    NULL
}
plot_annualstreams = function(stream_annual){
  stream_annual %>% 
    filter(WY<=2016) %>% 
    pivot_longer(-c(WY, watershed),
                 values_to = "eq_ha",
                 names_to = "species") %>% 
    ggplot(aes(x = WY, y = eq_ha, color = species, shape = watershed))+
    geom_path(size=0.7, show.legend = F)+
    geom_point(size=2, stroke=1, alpha = 1)+
    scale_color_manual(values = soilpalettes::soil_palette("redox",2))+
    scale_shape_manual(values = c(21,19))+
    facet_grid(species~., scales = "free_y")+
    labs(x = "Water Year",
         y = "eq. per hectare",
         title = "stream export")+
    theme_kp()+
    guides(color = F)+
    theme(legend.position = c(0.1,0.93))+
    NULL
}



# DRAKE PLAN --------------------------------------------------------------


ms_figs_plan = drake_plan(
  #----
  # LOAD FILES
  dep_annual_temp = read.csv(file_in(DEP_ANNUAL)),
  stream_annual_temp = read.csv(file_in(STREAMS_ANNUAL)),
  
  # CLEAN
  dep_annual = clean_dep_annual(dep_annual_temp),
  stream_annual = clean_stream_annual(stream_annual_temp),
  
  # PLOT
  gg_deposition_eb = plot_annualdep_eb(dep_annual),
  gg_streams = plot_annualstreams(stream_annual),
  
  # REPORT
  report = rmarkdown::render(
    knitr_in("reports/manuscript_figures.Rmd"),
    output_format = rmarkdown::github_document())
)


# make plan ----
make(ms_figs_plan)


# qaqc plan ---------------------------------------------------------------

qaqc_plan = drake_plan(
  # dep ---------------------------------------------------------------------
  dep_all = read.csv(file_in(DEP_ALL)),
  
  gg_dep_all = dep_all %>% 
    dplyr::select(-record_type, -month, -day, -julian_day, -running_day_of_project) %>% 
    pivot_longer(-c("site", "year"))  %>% 
    ggplot(aes(x = year, y = value))+
    geom_point()+
    facet_wrap(~name, scales = "free_y"),
  
  dep_annual = read.csv(file_in(DEP_ANNUAL)),  
  gg_dep_annual = 
    dep_annual %>% 
    pivot_longer(-WY) %>% 
    ggplot(aes(x = WY, y = value))+
    geom_point()+ geom_path()+
    facet_wrap(~name, scales = "free_y"),
  
  dep_monthly = read.csv(file_in(DEP_MONTHLY)),
  gg_dep_monthly = 
     dep_monthly %>% 
     pivot_longer(-c(year,month)) %>% 
     ggplot(aes(x = year, y = value))+
     geom_point()+
     facet_wrap(~name, scales = "free_y"),
  
  #
  # streams -----------------------------------------------------------------
  
  streams_annual = read.csv(file_in(STREAMS_ANNUAL)),
  gg_streams_annual = 
    streams_annual %>% 
    pivot_longer(-c(WY, watershed)) %>% 
    ggplot(aes(x = WY, y = value, color = watershed))+
    geom_point()+geom_path()+
    facet_wrap(~name, scales = "free_y")+
    NULL,
  
  streams_monthly = read.csv(file_in(STREAMS_MONTHLY)),
  gg_streams_monthly = 
    streams_monthly %>% 
    pivot_longer(-c(year, month, watershed)) %>% 
    ggplot(aes(x = year, y = value, color = watershed))+
    geom_point()+
    facet_wrap(~name, scales = "free_y")+
    NULL,
  
  streams_all = read.csv(file_in(STREAMS_ALL)),
  gg_streams_all = 
    streams_all %>% 
    dplyr::select(year, watershed, ends_with("ueq_L")) %>% 
    reshape2::melt(id = c("watershed", "year"),
                   variable.name = "name", value.name = "value") %>% 
    ggplot(aes(x = year, y = value, color = watershed))+
    geom_point()+
    facet_wrap(~name, scales = "free_y")+
    NULL,
  
  
  # report ------------------------------------------------------------------
  report = rmarkdown::render(
    knitr_in("reports/qaqc_figures.Rmd"),
    output_format = rmarkdown::github_document())
)

make(qaqc_plan)

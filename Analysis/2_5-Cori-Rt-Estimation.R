# ---------------------------------------
# Estimate Rt through time for state prison facilities
# Chris Hoover Jan 2021
# ---------------------------------------

library(EpiEstim)
library(tidyverse)

# Load data (from 0) -------------------
dat <- readRDS(here::here("data", "derived", "state_prisons_pop_cases_fin.rds")) %>% 
  filter(!is.na(Facility) & 
           !grepl("CHCF", Facility) & 
           !grepl("SATF", Facility)) # Both of these facilities had 0 cases. Both seem to be specilized for heatlhcare/treatment, so makes sense
  

# Instantaneous Rt estimates using Cori et al method implemented in EpiEstim package ----------------------
cori_rt_res <- function(facility, dat){
  
  inc_dat <- dat %>% 
    filter(Facility == facility) %>% 
    ungroup() %>% 
    dplyr::select(Date, New_Residents_Confirmed_rmv_neg) %>% 
    rename(dates = Date,
           I = New_Residents_Confirmed_rmv_neg)
  
  # Restrict to beginning of outbreak
  t_1st <- min(inc_dat$dates[which(inc_dat$I != 0)])
  inc_dat <- inc_dat %>% 
    filter(dates >= t_1st)
  
  # Sliding weekly windows in which to estimate Rt
  window = 7
  ts <- window:(nrow(inc_dat)-window)
  te <- window:(nrow(inc_dat)-window)+window
  
  R_config <- EpiEstim::make_config(t_start = ts,
                                    t_end = te,
                                    mean_si = 5.2, # From https://dx.doi.org/10.1016%2Fj.cegh.2020.08.007
                                    std_si = 1,
                                    seed = 430)
  
  Cori_R <- EpiEstim::estimate_R(incid = inc_dat,
                                 method = "parametric_si",
                                 config = R_config)
  
  out <- Cori_R$R %>% 
    mutate(
      Date = Cori_R$dates[c(window:(nrow(inc_dat)-window))],
      Facility = facility
    )
  
  return(out)
}

cori_rts <- bind_rows(lapply(unique(dat$Facility), 
                             FUN = cori_rt_res,  
                             dat = dat))

saveRDS(cori_rts, 
        here::here("data", "derived", "Cori_etal_Rts_residents.rds"))

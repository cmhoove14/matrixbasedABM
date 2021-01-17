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
# Get times for estimation
cori_rt <- function(facil, dat){
  print(facil)
  
  inc_dat <- dat %>% 
    filter(Facility == facil) %>% 
    mutate(
      res_new = round(Residents_Confirmed2 - lag(Residents_Confirmed2)),
      res_new2 = if_else(res_new < 0, 0, res_new),
      stf_new = round(Staff_Confirmed2 - lag(Staff_Confirmed2)),
      stf_new2 = if_else(stf_new < 0, 0, stf_new)
    ) %>% 
    ungroup() %>% 
    dplyr::select(Date, res_new2, stf_new2) %>% 
    rename(date = Date,
           local = res_new2,
           imported = stf_new2)
  
  # Restrict to beginning of outbreak
  t_1st <- min(inc_dat$date[which(inc_dat$local + inc_dat$imported != 0)])
  inc_dat <- inc_dat %>% 
    filter(date >= t_1st)
  
  # Sliding weeklywindows in which to estimate Rt
  window = 7
  ts <- 2:(nrow(inc_dat)-(window+1))
  te <- 2:(nrow(inc_dat)-(window+1))+window
  
  R_config <- EpiEstim::make_config(t_start = ts,
                                    t_end = te,
                                    mean_si = 5.2, # From https://dx.doi.org/10.1016%2Fj.cegh.2020.08.007
                                    std_si = 1)
  
  Cori_R <- EpiEstim::estimate_R(incid = inc_dat,
                                 method = "parametric_si",
                                 config = R_config)
  
  out <- Cori_R$R %>% 
    mutate(
      Facility = facil
    )
  
  return(out)
}

cori_rts <- bind_rows(lapply(unique(dat$Facility), 
                             FUN = cori_rt,  
                             dat = dat))





SF_cori_R$R %>%
  mutate(Date = as.Date("2020-03-05")+t_start) %>% 
  ggplot() +
  theme_bw() +
  geom_col(data = sf_case,
           aes(x = Date, y = (Cases/SF.pop)*1e5),
           fill = "blue", col = "darkblue", alpha = 0.3) +
  geom_line(aes(x = Date, y = `Median(R)`),
            size = 1.2, col = "red") +
  geom_ribbon(aes(x = Date, y = `Median(R)`,
                  ymin = `Quantile.0.025(R)`,
                  ymax = `Quantile.0.975(R)`),
              fill = "red", alpha = 0.4) +
  geom_hline(yintercept = 1, lty = 2) +
  scale_x_date(date_labels = "%m/%d", 
               date_breaks = "7 day") +
  theme(axis.title = element_text(size = 14,
                                  face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(x = "Date",
       y = expression(paste(R[e], " estimate/cases reported per 100k SF residents")),
       title = "Cori et al estimate of R through time from SF testing data")

#-------------------------------------
# Estimate R0 for CA state prison facilities using exponential growth method
# Chris Hoover, Jan 2020
# ------------------------------------

library(R0) # https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147#Sec17
library(tidyverse)

# Load data
dat <- readRDS(here::here("data", "derived", "state_prisons_pop_cases_fin.rds")) %>% 
  filter(!is.na(Facility) & 
           !grepl("CHCF", Facility) & 
           !grepl("SATF", Facility)) %>%  # Both of these facilities had 0 cases. Both seem to be specilized for heatlhcare/treatment, so makes sense
  mutate(
    Facility2 = str_replace(Facility, " State Prison", "")
  )

dat_exp_phase <- readRDS(here::here("data", "derived", "state_prisons_dat_exp_phase.rds")) %>% 
  mutate(
    Facility2 = str_replace(Facility, " State Prison", "")
  )

# Plot to build on 
I_curves <- dat %>% 
  ggplot() +
  geom_line(aes(x = Date, y = New_Residents_Confirmed_7day)) +
  facet_wrap(facets = "Facility2",
             nrow = 4, ncol = 8,
             labeller = label_wrap_gen()) +
  theme_bw() +
  theme(strip.text = element_text(size = 6.5)) +
  labs(x = "Date",
       y = "Resident incident cases (weekly average)")

# create generation time
GT_sars2 <-  generation.time("gamma", c(3.95,1.51)) 


# Add expnential phase to facilities plot
I_curves <- I_curves +
  geom_line(data = dat_exp_phase,
            aes(x = Date, y = New_Residents_Confirmed_7day),
            col = "red")

# Get R0 for all facilities based o exponential phase
fac_R0s <- bind_rows(lapply(unique(dat$Facility),
                            function(f){
                              inc_dat <- dat_exp_phase %>% 
                                filter(Facility == f) %>% 
                                pull(New_Residents_Confirmed_7day) %>% 
                                round()
                              
                              R_est <- est.R0.EG(inc_dat, GT_sars2)
                              
                              return(c("Facility" = f,
                                       "R0" = round(R_est$R,2),
                                       "R0_lo" = round(R_est$conf.int[1],2),
                                       "R0_hi" = round(R_est$conf.int[2],2)))
                            }))

# Get labels and add to plot
fac_R0s <- fac_R0s %>% 
  mutate(
    label     = paste0(R0," (",
                       R0_lo, " - ",
                       R0_hi, ")"),
    Facility2 = str_replace(Facility, " State Prison", ""),
    GT = "Singapore"
  )

I_curves_label <- I_curves +
  geom_text(
    data    = fac_R0s,
    mapping = aes(x = as.Date("2020-10-20", format = "%Y-%m-%d"), 
                  y = 120, 
                  label = label),
    size = 2,
    col = "red"
  )

# R0 sensitivity to Generation time: Re-estimate with diff generation time
GT2_sars2 <- generation.time("gamma", c(5.2,1.72)) 

fac_R0s2 <- bind_rows(lapply(unique(dat$Facility),
                            function(f){
                              inc_dat <- dat_exp_phase %>% 
                                filter(Facility == f) %>% 
                                pull(New_Residents_Confirmed_7day) %>% 
                                round()
                              
                              R_est <- est.R0.EG(inc_dat, GT2_sars2)
                              
                              return(c("Facility" = f,
                                       "R0" = round(R_est$R,2),
                                       "R0_lo" = round(R_est$conf.int[1],2),
                                       "R0_hi" = round(R_est$conf.int[2],2)))
                            }))

# Get labels and add to plot
fac_R0s2 <- fac_R0s2 %>% 
  mutate(
    label     = paste0(R0," (",
                       R0_lo, " - ",
                       R0_hi, ")"),
    Facility2 = str_replace(Facility, " State Prison", ""),
    GT = "Tianjin"
    
  )

I_curves_label2 <- I_curves_label +
  geom_text(
    data    = fac_R0s2,
    mapping = aes(x = as.Date("2020-10-20", format = "%Y-%m-%d"), 
                  y = 105, 
                  label = label),
    size = 2,
    col = "blue"
  )

ggsave(plot = I_curves_label2,
       filename = here::here("Plots", "R0_estimates_exponential_growth_incident_cases.jpg"),
       height = 6, 
       width = 9,
       units = "in",
       dpi = 300)


# Separate plot with R0s alone and for two different generation times -----------------
R0_plot <- bind_rows(fac_R0s,fac_R0s2) %>% 
  ggplot() +
    geom_point(aes(x = Facility2, y = as.numeric(R0), col = GT)) +
    geom_errorbar(aes(x = Facility2, 
                      ymin = as.numeric(R0_lo), 
                      ymax = as.numeric(R0_hi),
                      col = GT),
                  width = 0.2) +
    ylim(c(0,max(as.numeric(fac_R0s$R0_hi)))) +
    geom_hline(yintercept = 1, lty = 3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Facility", 
         y = expression(R[0]~Estimate),
         col = "Generation\nInterval\nsource")
  
ggsave(plot = R0_plot,
       filename = here::here("Plots", "R0_estimates_exponential_growth_point_intervals.jpg"),
       height = 6, 
       width = 9,
       units = "in",
       dpi = 300)

# Compare R0 to Percent occupied at beginning of outbreak and percent depopulated at beginning of outbreak ---------------------
R0s_percap <- fac_R0s %>% 
  left_join(dat_exp_phase %>% 
              group_by(Facility2) %>% 
              filter(Date == min(Date)) %>% 
              dplyr::select(Nt, N0, N_frac, Percent_Occupied))

R0s_percap %>% 
  ggplot(aes(x = Percent_Occupied, y = as.numeric(R0))) +
    geom_point() +
    theme_bw() +
    geom_smooth(method = "lm")

R0s_percap %>% 
  ggplot(aes(x = N_frac, y = as.numeric(R0))) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm")

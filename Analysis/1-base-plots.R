# ---------------------------------------
# Plot CA State Prison data and fit SIR models
# Chris Hoover Jan 2021
# ---------------------------------------

library(ggplot2)
library(tidyverse)

# Load data (from 0) -------------------
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

# Plot new cases among residents ----------------------
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

ggsave(plot = I_curves,
       filename = here::here("Plots", "incident_cases_by_date_faceted.jpg"),
       height = 6, 
       width = 9,
       units = "in",
       dpi = 300)

I_curves

# Plot active cases among residents ----------------------
I_curves2 <- dat %>% 
  ggplot() +
  geom_line(aes(x = Date, y = Residents_Active)) +
  facet_wrap(facets = "Facility2",
             nrow = 4, ncol = 8,
             labeller = label_wrap_gen()) +
  theme_bw() +
  theme(strip.text = element_text(size = 6.5)) +
  labs(x = "Date",
       y = "Resident Active Cases")

ggsave(plot = I_curves2,
       filename = here::here("Plots", "active_cases_by_date_faceted.jpg"),
       height = 6, 
       width = 9,
       units = "in",
       dpi = 300)

I_curves


# Plot facility population curves ----------------------
pop_curves <- dat %>% 
  ggplot() +
  geom_line(aes(x = Date, y = Pop_interpolated)) +
  theme_bw() +
  facet_wrap(facets = "Facility2",
             nrow = 4, ncol = 8,
             labeller = label_wrap_gen()) +
  theme(strip.text = element_text(size = 6)) +
  labs(y = "Facility Resident Population")

ggsave(plot = pop_curves,
       filename = here::here("Plots", "ca_state_facilities_pop_over_time_2020_faceted.jpg"),
       height = 6, 
       width = 9,
       units = "in",
       dpi = 300)


pop_curves



# Get and plot attack rates -------------------------
attack_rates <- dat %>% 
  filter(Date == as.Date("2021-01-01")) %>% 
  mutate(
    attack_rate = Residents_Confirmed2/N0
  ) 

# Rearrange order of facilities by attack rate for plot aesthetics
attack_rates$Facility_order <- factor(attack_rates$Facility2, 
                                      levels = attack_rates$Facility2[order(-attack_rates$attack_rate)])

ar_plot <- attack_rates %>% 
  ggplot() +
    geom_col(aes(x = Facility_order, y = attack_rate)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Facility",
         y = "Attack Rate")

ggsave(plot = ar_plot,
       filename = here::here("Plots", "facility_april1_jan1_attack_rates.jpg"),
       height = 6, 
       width = 8,
       units = "in",
       dpi = 300)

# Depopulation prior to outbreak (exponential phase) ------------------------
exp_phase_start <- dat_exp_phase %>% 
  filter(Date == min(Date)) 

exp_phase_start$Facility_order <- factor(exp_phase_start$Facility2, 
                                         levels = exp_phase_start$Facility2[order(-exp_phase_start$N_frac)])


exp_phase_start_plot <- exp_phase_start %>% 
  ggplot() +
    geom_col(aes(x = Facility_order, y = N_frac)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = "Facility",
         y = "Proportion of 4/1 population at outbreak start")

ggsave(plot = exp_phase_start_plot,
       filename = here::here("Plots", "facility_april1_outbreak_start_depop.jpg"),
       height = 7, 
       width = 8,
       units = "in",
       dpi = 300)


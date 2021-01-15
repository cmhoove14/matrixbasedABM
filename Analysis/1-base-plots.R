# ---------------------------------------
# Plot CA State Prison data and fit SIR models
# Chris Hoover Jan 2021
# ---------------------------------------

library(ggplot2)
library(tidyverse)

# Load data (from 0) -------------------
dat <- readRDS(here::here("data", "derived", "state_prisons_pop_cases_fin.rds")) %>% 
  filter(!is.na(Facility))

# Plot case curves
I_curves <- dat %>% 
  filter(Resident_Outbreak_Day >=0) %>% 
  ggplot() +
    geom_line(aes(x = Resident_Outbreak_Day, y = Residents_Active_7day, col = Facility)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = )) +
    labs(x = "Days since first resident case",
         y = "Active cases")

ggsave(plot = I_curves,
       filename = here::here("Plots", "active_infections_days_since_1st_resident_case.jpg"),
       height = 6, 
       width = 8,
       units = "in",
       dpi = 300)

I_curves

#Plot facility population curves
pop_curves <- dat %>% 
  ggplot() +
  geom_line(aes(x = Date, y = Pop_interpolated, col = Facility)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6)) +
  #guides(col = guide_legend(ncol = 4)) +
  labs(y = "Facility Population")

ggsave(plot = pop_curves,
       filename = here::here("Plots", "ca_state_facilities_pop_over_time_2020.jpg"),
       height = 6, 
       width = 8,
       units = "in",
       dpi = 300)


pop_curves

# Identify facilities with rapid depopulation


pop_curves_outbreak <- dat %>% 
  filter(Resident_Outbreak_Day >=0) %>% 
  ggplot() +
    geom_line(aes(x = Resident_Outbreak_Day, y = Pop_interpolated, col = Facility)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 6)) +
    #guides(col = guide_legend(ncol = 4)) +
    labs(y = "Facility Population",
         x = "Days since outbreak")

pop_curves_outbreak

ggsave(plot = pop_curves_outbreak,
       filename = here::here("Plots", "pop_curves_since_outbreak_start.jpg"),
       height = 6,
       width = 8,
       units = "in",
       dpi = 300)

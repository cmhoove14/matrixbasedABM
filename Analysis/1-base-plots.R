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

# Plot new cases among residents ----------------------
I_curves <- dat %>% 
  ggplot() +
    geom_line(aes(x = Date, y = New_Resident_Cases_7day)) +
    facet_wrap(facets = "Facility2",
               nrow = 4, ncol = 8,
               labeller = label_wrap_gen()) +
    theme_bw() +
    theme(strip.text = element_text(size = 6)) +
    labs(x = "Date",
         y = "Resident incident cases (weekly average)")

ggsave(plot = I_curves,
       filename = here::here("Plots", "incident_cases_by_date_faceted.jpg"),
       height = 6, 
       width = 9,
       units = "in",
       dpi = 300)

I_curves

# Plot new cases among residents and staff----------------------
I_curves2 <- dat %>% 
  ggplot() +
  geom_line(aes(x = Date, y = New_Resident_Cases_7day)) +
  geom_line(aes(x = Date, y = New_Staff_Cases_7day), col = "blue") +
  facet_wrap(facets = "Facility2",
             nrow = 4, ncol = 8,
             labeller = label_wrap_gen()) +
  theme_bw() +
  theme(strip.text = element_text(size = 6)) +
  labs(x = "Date",
       y = "Incident cases (weekly average)")

ggsave(plot = I_curves2,
       filename = here::here("Plots", "incident_cases_by_date_faceted.jpg"),
       height = 6, 
       width = 9,
       units = "in",
       dpi = 300)

I_curves


#Plot facility population curves
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



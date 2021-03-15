# ---------------------------------------
# Get distribution of outbreak sizes among facilities
# Chris Hoover Jan 2021
# ---------------------------------------

library(tidyverse)

# Load data (from 0) -------------------
dat <- readRDS(here::here("data", "derived", "state_prisons_pop_cases_fin2021-02-02.rds")) %>% 
  filter(!is.na(Facility) & 
           !grepl("CHCF", Facility) & 
           !grepl("SATF", Facility)) # Both of these facilities had 0 cases. Both seem to be specilized for heatlhcare/treatment, so makes sense

# Identify outbreaks --------------------

# 7 day washout period 
outbreaks_df7day <- dat %>% 
  dplyr::select(c(Facility:Residents_Active)) %>% 
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 7 days with no cases
    new_cases_7day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 7, 
                                   na.pad = T, align = "right"),
    new_cases_7day_lead1 = lead(new_cases_7day),
    outbreak_start = if_else(new_cases_7day == 0 & new_cases_7day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    # For plotting outbreaks 
    plot7day_cases = if_else(new_cases_7day == 0, NA_real_, New_Residents_Confirmed_7day)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(outbreak_num) & outbreak_num > 0)



# 10 day washout period 
outbreaks_df10day <- dat %>% 
  dplyr::select(c(Facility:Residents_Active)) %>% 
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 10 days with no cases
    new_cases_10day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 10, 
                                   na.pad = T, align = "right"),
    new_cases_10day_lead1 = lead(new_cases_10day),
    outbreak_start = if_else(new_cases_10day == 0 & new_cases_10day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    plot7day_cases = if_else(new_cases_10day == 0, NA_real_, New_Residents_Confirmed_7day)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(outbreak_num) & outbreak_num > 0)



# 14 day washout period      
outbreaks_df14day <- dat %>% 
  dplyr::select(c(Facility:Residents_Active)) %>% 
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 14 days with no cases
    new_cases_14day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 14, 
                                   na.pad = T, align = "right"),
    new_cases_14day_lead1 = lead(new_cases_14day),
    outbreak_start = if_else(new_cases_14day == 0 & new_cases_14day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    plot7day_cases = if_else(new_cases_14day == 0, NA_real_, New_Residents_Confirmed_7day)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(outbreak_num) & outbreak_num > 0)


# Plot raw outbreak size distribution -------------------
outbreak_size7day <- outbreaks_df7day %>% 
  group_by(Facility_Outbreak) %>% 
  summarise(outbreak_size = sum(New_Residents_Confirmed_rmv_neg))


outbreak_size10day <- outbreaks_df10day %>% 
  group_by(Facility_Outbreak) %>% 
  summarise(outbreak_size = sum(New_Residents_Confirmed_rmv_neg))


outbreak_size14day <- outbreaks_df14day %>% 
  group_by(Facility_Outbreak) %>% 
  summarise(outbreak_size = sum(New_Residents_Confirmed_rmv_neg))



par(mfrow = c(3,1))
hist(outbreak_size7day$outbreak_size, breaks = 30,
     xlab = "Outbreak Size", main = "Outbreak size dist'n with 7 day washout")
hist(outbreak_size10day$outbreak_size, breaks = 30,
     xlab = "Outbreak Size", main = "Outbreak size dist'n with 10 day washout")
hist(outbreak_size14day$outbreak_size, breaks = 30,
     xlab = "Outbreak Size", main = "Outbreak size dist'n with 14 day washout")



# Plot outbreak size distribution with upper cutoff-------------------
upper_cutoff <- 10

outbreak_size7day_uppercut <- outbreak_size7day %>% 
  mutate(outbreak_size_uppercut = if_else(outbreak_size > upper_cutoff, upper_cutoff, outbreak_size))

outbreak_size10day_uppercut <- outbreak_size10day  %>% 
  mutate(outbreak_size_uppercut = if_else(outbreak_size > upper_cutoff, upper_cutoff, outbreak_size))

outbreak_size14day_uppercut <- outbreak_size14day %>% 
  mutate(outbreak_size_uppercut = if_else(outbreak_size > upper_cutoff, upper_cutoff, outbreak_size))

jpeg(here::here("Plots/outbreak_size_distns_collate_gt10.jpg"),
     width = 6, height = 6, units = "in", res = 300)
par(mfrow = c(3,1),
    mar = c(3,3,1,0.2),
    mgp = c(1.75,0.75,0))

hist(outbreak_size7day_uppercut$outbreak_size_uppercut, breaks = 30,
     xlab = "", main = "Outbreak size dist'n with 7 day washout", 
     xlim = c(1,10), ylim = c(0,60))
hist(outbreak_size10day_uppercut$outbreak_size_uppercut, breaks = 30,
     xlab = "", main = "Outbreak size dist'n with 10 day washout", 
     xlim = c(1,10), ylim = c(0,60))
hist(outbreak_size14day_uppercut$outbreak_size_uppercut, breaks = 30,
     xlab = "Outbreak Size", main = "Outbreak size dist'n with 14 day washout", 
     xlim = c(1,10), ylim = c(0,60))

dev.off()

# Plot time series with outbreaks delineated -----------------------
outbreaks_df10day %>% 
  mutate(Facility2 = str_replace(Facility, " State Prison", "")) %>% 
  ggplot() +
    geom_line(aes(x = Date, y = plot7day_cases, col = as.factor(outbreak_num))) +
    theme_classic() +
    scale_y_continuous(trans = "log1p",
                       breaks = c(0,10,100,1000)) +
    facet_wrap(facets = "Facility2",
               nrow = 4, ncol = 8,
               labeller = label_wrap_gen()) +
    labs(y = "7-day average of incident resident cases",
         col = "Facility\nOutbreak\nNumber")
  
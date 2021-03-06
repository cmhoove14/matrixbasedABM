

library(zoo)
library(tidyverse)
library(gridExtra)

# Data -----------------
dat           <- readRDS(here::here("data/derived/state_prisons_pop_cases_fin2021-03-18.rds"))

# NAs in county FIPS causing join to fill in NAs, so need to make sure all obs have proper county fips
dat_lookup_fips <- dat %>% filter(Date == "2020-04-01") %>% dplyr::select(Facility, County.FIPS)

dat <- dat %>% 
  left_join(dat_lookup_fips,
            by = "Facility",
            suffix = c(".x","")) %>% 
  dplyr::select(-County.FIPS.x)

CA_cases_pops <- readRDS(here::here("data/derived/CA_Cases_Pops2021-03-18.rds")) %>% 
  group_by(County) %>% 
  # Add measures of cumulative prevalence
  mutate(cum_cases_7day = zoo::rollsum(cases, k =7,
                                       align = "right", na.pad = T),
         prev_7day = cum_cases_7day/population,
         prev_7day_p100k = prev_7day*1e5,
         cum_cases_14day = zoo::rollsum(cases, k =14,
                                        align = "right", na.pad = T),
         prev_14day = cum_cases_14day/population,
         prev_14day_p100k = prev_14day*1e5) %>% 
  ungroup()

dat_case_pop_merge <- dat %>% 
  left_join(CA_cases_pops %>% mutate(County.FIPS = as.numeric(fips)),
            by = c("County.FIPS" = "County.FIPS",
                   "Date" = "date"),
            suffix = c(".x",""))


# define_outbreaks -----------------
# 7 day washout period 
outbreaks_df7day <- dat_case_pop_merge %>% 
  dplyr::select(-c(Residents.Recovered:New_Staff_Deaths, Residents.Tadmin:fips)) %>%
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 7 days with no cases
    New_Residents_Confirmed_rmv_neg = if_else(Date == as.Date("2020-04-01"), 0, New_Residents_Confirmed_rmv_neg), #Remove NA on first obs day
    new_cases_7day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 7, 
                                  na.pad = T, align = "right"),
    new_cases_7day_lead1 = lead(new_cases_7day),
    outbreak_start = if_else(new_cases_7day == 0 & new_cases_7day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    # For plotting outbreaks 
    plot7day_cases = if_else(new_cases_7day == 0, NA_real_, New_Residents_Confirmed_7day),
    # For censoring outbreaks with high prior case counts (high chance of effects of immunity)
    Cum_Residents_Confirmed_rmv_neg = cumsum(New_Residents_Confirmed_rmv_neg),
    Cum_attack_rate = Cum_Residents_Confirmed_rmv_neg/N0,
    Ns = N0 - Cum_Residents_Confirmed_rmv_neg
  ) %>% 
  ungroup()



# 10 day washout period 
outbreaks_df10day <- dat_case_pop_merge %>% 
  dplyr::select(-c(Residents.Recovered:New_Staff_Deaths, Residents.Tadmin:fips)) %>%
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 10 days with no cases
    New_Residents_Confirmed_rmv_neg = if_else(Date == as.Date("2020-04-01"), 0, New_Residents_Confirmed_rmv_neg), #Remove NA on first obs day
    new_cases_10day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 10, 
                                   na.pad = T, align = "right"),
    new_cases_10day_lead1 = lead(new_cases_10day),
    outbreak_start = if_else(new_cases_10day == 0 & new_cases_10day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    plot7day_cases = if_else(new_cases_10day == 0, NA_real_, New_Residents_Confirmed_7day),
    # For censoring outbreaks with high prior case counts (high chance of effects of immunity)
    Cum_Residents_Confirmed_rmv_neg = cumsum(New_Residents_Confirmed_rmv_neg),
    Cum_attack_rate = Cum_Residents_Confirmed_rmv_neg/N0,
    Ns = N0 - Cum_Residents_Confirmed_rmv_neg
  ) %>% 
  ungroup()



# 14 day washout period      
outbreaks_df14day <- dat_case_pop_merge %>% 
  dplyr::select(-c(Residents.Recovered:New_Staff_Deaths, Residents.Tadmin:fips)) %>%
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 14 days with no cases
    New_Residents_Confirmed_rmv_neg = if_else(Date == as.Date("2020-04-01"), 0, New_Residents_Confirmed_rmv_neg), #Remove NA on first obs day
    new_cases_14day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 14, 
                                   na.pad = T, align = "right"),
    new_cases_14day_lead1 = lead(new_cases_14day),
    outbreak_start = if_else(new_cases_14day == 0 & new_cases_14day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    plot7day_cases = if_else(new_cases_14day == 0, NA_real_, New_Residents_Confirmed_7day),
    # For censoring outbreaks with high prior case counts (high chance of effects of immunity)
    Cum_Residents_Confirmed_rmv_neg = cumsum(New_Residents_Confirmed_rmv_neg),
    Cum_attack_rate = Cum_Residents_Confirmed_rmv_neg/N0,
    Ns = N0 - Cum_Residents_Confirmed_rmv_neg
  ) %>% 
  ungroup()

# Plot time series with outbreaks delineated -----------------------
#Facilities with at least three outbreaks
g5_ob_facs <- outbreaks_df14day %>% 
  group_by(Facility) %>% 
  summarise(n_obs = max(outbreak_num, na.rm = T)) %>% 
  filter(n_obs >= 5) %>% 
  pull(Facility)

outbreaks_14d_wash_plot <- outbreaks_df14day %>% 
  filter(!is.na(outbreak_num),
         Facility %in% g5_ob_facs,
         outbreak_num > 0) %>% 
  mutate(Facility2 = str_remove(unlist(str_split(Facility, pattern = "\\("))[seq(2,nrow(.)*2, by=2)], pattern = "\\)")) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = plot7day_cases, col = as.factor(outbreak_num))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 8),
        axis.title = element_text(size = 14),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  scale_y_continuous(trans = "log1p",
                     breaks = c(1,10,100,1000)) +
  scale_x_date(date_labels = "%b'%y") +
  facet_wrap(facets = "Facility2",
             nrow = 2, ncol = 5) +
  labs(y = "7-day average of incident resident cases",
       col = "Introduction")

outbreaks_14d_wash_plot

ggsave("Plots/Outbreaks_5min.png",
       height = 4, width = 6, units = "in")

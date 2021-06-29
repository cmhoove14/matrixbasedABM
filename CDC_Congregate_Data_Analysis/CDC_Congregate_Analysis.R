# -------------------------
# CDCR Outbreak delineation and size
# Chris Hoover March 2021 
#--------------------------

library(zoo)
library(tidyverse)

# Utils ---------------------
# Function for initial estimate of incidence from change in cumulative, then replace negative changes with NAs
change_rmv_nas <- function(vec){
  # Find values that result in negative changes and replace with NA
  diff_vec <- c(NA, diff(vec))
  vec2 <- vec
  vec2[is.na(vec)] <- NA
  vec2[diff_vec < 0] <- NA
  
  return(vec2)
}


# Function to return difference of cumsum vectors removing NAs
cumsum_diffs <- function(vec1, vec2, vec3){
  vec1[is.na(vec1)] <- 0
  vec2[is.na(vec2)] <- 0
  vec3[is.na(vec3)] <- 0
  
  out <- cumsum(vec1) - cumsum(vec2) - cumsum(vec3)
  
  return(out)
}

# Load Case and population data, merge and clean -------------------

# Data acquired from covid behind bars project on March 18, 2021 via call below  
# system(paste0("curl -o ", here::here("data", "raw", "ucla_law_covid_behind_bars"), Sys.Date(), ".csv " ,
#               "https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/CA-historical-data.csv"))

uclabb <- read_csv(here::here("data", "raw", "ucla_law_covid_behind_bars2021-03-18.csv")) %>% 
  dplyr::select(-(Is.Different.Operator:Source.Capacity)) %>% 
  filter(Date >= as.Date("2020-04-01"),
         Jurisdiction == "state")

# See data/get/get_cdcr_pop_pdfs.R and data/get/process_cdcr_pop_pdfs.R
pop_sps <- readRDS(here::here("data", "derived", "cdcr_population_ts_2021-03-18.rds")) %>% 
  mutate(dplyr::across(.cols = c("Capacity", "Design_Capacity", "Percent_Occupied", "Staffed_Capacity"),
                       .fns = as.numeric))

# First generate lookup table to merge the two datasets
pop_facs <- unique(pop_sps$Facility)
case_facs <- unique(uclabb$Name)

facility_lookup_table <- data.frame(Facility_pop = pop_facs,
                                    Facility_case = NA)

# Match facility names
facility_lookup_table <- bind_rows(
  lapply(case_facs, function(f){
    init_grep <- grepl(f, pop_facs, ignore.case = T)
    cat(f, sum(init_grep),"\n")
    return(list("Facility_pop" = pop_facs[which(init_grep)][sum(init_grep)],
                "Facility_case" = f))
  })
)

# Remove duplicates that arise due to partial matching
facility_lookup_table <- facility_lookup_table[!duplicated(facility_lookup_table$Facility_pop),]

#Identify facilities that got left behind and append to lookup table
missed_facs <- pop_facs[!pop_facs %in% facility_lookup_table$Facility_pop]

facility_lookup_table <- rbind(facility_lookup_table,
                               data.frame(Facility_pop = missed_facs, 
                                          Facility_case = c("CALIFORNIA MENS COLONY",
                                                            "CALIFORNIA STATE PRISON SAN QUENTIN",
                                                            "CENTRAL CALIFORNIA WOMENS FACILITY")))

# Should be 0 if all matched
pop_facs[!pop_facs %in% facility_lookup_table$Facility_pop]

#Visual check
View(facility_lookup_table)

# Merge datasets

#Folsom State Prison has both male and female populationsin the population dataset, but the cases are reported facility wide. Unclear whether the case reporting is for both facilities or only for the male facility, but will assume they're facility-wide and therefore just merge the population data for Folsom
pop_sps2 <- pop_sps %>% 
  group_by(Facility, Report_Date) %>% 
  summarise(
    across(
      .cols = c("Capacity", "Design_Capacity", "Staffed_Capacity"), .fns = sum
    )
  ) %>% 
  mutate(
    Percent_Occupied = Capacity/Design_Capacity*100
  )

# Add case facility names to population dataset then merge datasets by facility and date
pop_case_merge <- uclabb %>% 
  left_join(facility_lookup_table, by = c("Name" = "Facility_case")) %>% 
  left_join(pop_sps2, by = c("Facility_pop" = "Facility",
                             "Date" = "Report_Date"))

intermediate_clean <- pop_case_merge %>%
  rename(Facility = Facility_pop) %>%
  filter(!is.na(Facility)) %>% 
  group_by(Facility) %>%
  padr::pad() %>%
  # Add a few columns   
  mutate(
    Resident_Outbreak_Start = min(Date[which(Residents.Confirmed > 0)], na.rm = T),
    Resident_Outbreak_Day   = as.numeric(Date - Resident_Outbreak_Start),
    Staff_Outbreak_Start    = min(Date[which(Staff.Confirmed > 0)], na.rm = T),
    Staff_Outbreak_Day      = as.numeric(Date - Staff_Outbreak_Start),
    Nt                      = na.approx(Capacity, na.rm = FALSE),
    N0                      = first(Capacity),
    N_frac                  = Nt/N0,
    Design_Capacity         = first(Design_Capacity),
    Percent_Occupied        = Nt/Design_Capacity
  ) %>% 
  # Correct cumulative counts for days resulting in negative cumulative changes
  mutate(
    Residents_Confirmed2    = change_rmv_nas(Residents.Confirmed),
    Staff_Confirmed2        = change_rmv_nas(Staff.Confirmed),
    Residents_Recovered2    = change_rmv_nas(Residents.Recovered),
    Staff_Recovered2        = change_rmv_nas(Staff.Recovered),
    Residents_Deaths2       = change_rmv_nas(Residents.Deaths),
    Staff_Deaths2           = change_rmv_nas(Staff.Deaths)
  ) %>% 
  # Fill days that result in negatives with NA  
  tidyr::fill(Residents_Confirmed2:Staff_Deaths2) %>% 
  mutate(
    New_Residents_Confirmed = Residents_Confirmed2 - lag(Residents_Confirmed2),
    New_Staff_Confirmed     = Staff_Confirmed2 - lag(Staff_Confirmed2),
    New_Residents_Recovered = Residents_Recovered2 - lag(Residents_Recovered2),
    New_Staff_Recovered     = Staff_Recovered2 - lag(Staff_Recovered2),
    New_Residents_Deaths    = Residents_Deaths2 - lag(Residents_Deaths2),
    New_Staff_Deaths        = Staff_Deaths2 - lag(Staff_Deaths2)
  )

# Still a couple dozen days that end up with negative incident countsamong residents, but nothing that appears systematic and all values are small negative (-4 or greater), so will simply replace them with 0 and move on. Staff on the other hand still have days that result in very large negative numbers, particularly around labor day (09-05-2020) and the beginning of December, so best for now to stick to analyses that rely on Resident metrics only

#View(intermediate_clean %>% filter(New_Staff_Confirmed < 0))

fin_dat <- intermediate_clean %>% 
  mutate(
    New_Residents_Confirmed_rmv_neg = if_else(New_Residents_Confirmed < 0, 0, New_Residents_Confirmed),
    New_Residents_Recovered_rmv_neg = if_else(New_Residents_Recovered < 0, 0, New_Residents_Recovered),
    New_Residents_Deaths_rmv_neg    = if_else(New_Residents_Deaths < 0, 0, New_Residents_Deaths),
    New_Residents_Confirmed_7day    = zoo::rollapply(
      data = New_Residents_Confirmed_rmv_neg,
      width = 7,
      FUN = mean,
      na.rm = T,
      fill = NA,
      align = "right"
    ),
    New_Residents_Recovered_7day    = zoo::rollapply(
      data = New_Residents_Recovered_rmv_neg,
      width = 7,
      FUN = mean,
      na.rm = T,
      fill = NA,
      align = "right"
    ),
    New_Residents_Deaths_7day       = zoo::rollapply(
      data = New_Residents_Deaths_rmv_neg,
      width = 7,
      FUN = mean,
      na.rm = T,
      fill = NA,
      align = "right"
    ),
    Residents_Active                = cumsum_diffs(
      New_Residents_Confirmed_rmv_neg,
      New_Residents_Recovered_rmv_neg,
      New_Residents_Deaths_rmv_neg
    ),
    Residents_Active_7day           = zoo::rollapply(
      data = Residents_Active,
      width = 7,
      FUN = mean,
      na.rm = T,
      fill = NA,
      align = "right"
    ),
  ) %>% 
  dplyr::select("Facility", "Date", "Resident_Outbreak_Day", "Staff_Outbreak_Day", "Nt", "N0", "N_frac",
                "Residents.Confirmed", "Residents_Confirmed2", "New_Residents_Confirmed", "New_Residents_Confirmed_rmv_neg", "New_Residents_Confirmed_7day",
                "Residents.Recovered", "Residents_Recovered2", "New_Residents_Recovered", "New_Residents_Recovered_rmv_neg", "New_Residents_Recovered_7day",
                "Residents.Deaths", "Residents_Deaths2", "New_Residents_Deaths", "New_Residents_Deaths_rmv_neg", "New_Residents_Deaths_7day",
                "Residents_Active", "Residents_Active_7day",
                "Staff.Confirmed", "Staff_Confirmed2", "New_Staff_Confirmed", 
                "Staff.Recovered", "Staff_Recovered2", "New_Staff_Recovered",
                "Staff.Deaths", "Staff_Deaths2", "New_Staff_Deaths",
                "Design_Capacity", "Percent_Occupied", "Staffed_Capacity", 
                "Residents.Tadmin", "Staff.Tested", "Residents.Negative", "Staff.Negative", "Residents.Pending", "Staff.Pending", 
                "Residents.Quarantine", "Staff.Quarantine", "Residents.Active", "Residents.Tested",
                "Address", "Zipcode", "City", "County", "County.FIPS", "Latitude", "Longitude", "source")

saveRDS(fin_dat, here::here("data", "derived", paste0("state_prisons_pop_cases_fin2021-03-18.rds")))

# Define outbreaks -------------------
# 14 day washout period      
outbreaks_df14day <- fin_dat %>% 
  dplyr::select(-c(Residents.Recovered:New_Staff_Deaths)) %>%
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 14 days with no cases
    New_Residents_Confirmed_rmv_neg = if_else(Date == as.Date("2020-04-01"), 
                                              0, New_Residents_Confirmed_rmv_neg), #Remove NA on first obs day
    new_cases_14day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 14, 
                                   na.pad = T, align = "right"),
    new_cases_14day_lead1 = lead(new_cases_14day),
    outbreak_start = if_else(new_cases_14day == 0 & new_cases_14day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    plot7day_cases = if_else(new_cases_14day == 0, NA_real_, New_Residents_Confirmed_7day),
    # For censoring outbreaks with high prior case counts
    Cum_Residents_Confirmed_rmv_neg = cumsum(New_Residents_Confirmed_rmv_neg),
    Cum_attack_rate = Cum_Residents_Confirmed_rmv_neg/N0,
    Ns = N0 - Cum_Residents_Confirmed_rmv_neg
  ) %>% 
  ungroup()

#Facilities with at least five outbreaks
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
        strip.text = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  scale_y_continuous(trans = "log1p",
                     breaks = c(1,10,100,1000)) +
  scale_x_date(date_labels = "%b'%y") +
  facet_wrap(facets = "Facility2",
             nrow = 2, ncol = 5) +
  labs(y = "7-day average of incident resident cases",
       col = "Introduction")

outbreaks_14d_wash_plot

ggsave(here::here("Plots/Outbreaks_5min_anon.png"),
       height = 4, width = 6, units = "in")

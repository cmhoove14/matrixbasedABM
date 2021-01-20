# ---------------------------------------
# Load and merge CA state prison COVID-19 cases and population
# Chris Hoover Jan 2021
# ---------------------------------------

library(zoo)
library(tidyverse)

# Functions (to be moved to R package infrastructure)

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

# Load Case and population data -------------------

# See data/get for scripts to get and process data from online sources

uclabb <- readRDS(here::here("data", "raw", "ucla_law_covid_behind_bars_github_data_1-17-21.rds")) %>% 
  dplyr::select(-(Is.Different.Operator:Source.Capacity)) %>% 
  filter(Date >= as.Date("2020-04-01"))
pop_sps <- readRDS(here::here("data", "derived", "cdcr_population_ts_2020-01-11.rds")) %>% 
  mutate(dplyr::across(.cols = c("Capacity", "Design_Capacity", "Percent_Occupied", "Staffed_Capacity"),
                .fns = as.numeric))

# First generate lookup table to merge the two datasets ----------------
pop_facs <- unique(pop_sps$Facility)
case_facs <- unique(uclabb$Name)

facility_lookup_table <- data.frame(Facility_pop = pop_facs,
                                    Facility_case = NA)

# Match facility names
facility_lookup_table <- bind_rows(lapply(
  case_facs, function(f){
    init_grep <- grepl(f, pop_facs, ignore.case = T)
    
    return(list("Facility_pop" = pop_facs[which(init_grep)],
                "Facility_case" = f))
  }
))

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

# Merge datasets -----------------
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

saveRDS(fin_dat, here::here("data", "derived", "state_prisons_pop_cases_fin.rds"))

# ------------------------------
# function to get data for exponential growth phase for facility
get_growth_phase <- function(facility, DF){
  data <- DF %>% 
    filter(Facility == facility)
  
  peak_day <- which.max(data %>% pull(New_Residents_Confirmed_7day))
  data_before_peak <- data[1:peak_day,]
  inc_til_peak <- round(data_before_peak$New_Residents_Confirmed_7day)
  
  day <- peak_day
  inc_day <- inc_til_peak[day]
  
  while(inc_day > 0 & day > 7){ # Stop once day gets to 7 due to 7 day smoothing wndow right align means NAs for first 7 observations
    day <- day-1
    inc_day <- inc_til_peak[day]
  }
  
  
  start_day <- day
  
  peak_date <- data$Date[peak_day]
  start_date <- data$Date[start_day]
  
  return(c(start_date, peak_date))
}

# Get data frame of expnential phase for all facilities
dat_exp_phase <- bind_rows(lapply(unique(dat %>% 
                                           filter(!is.na(Facility) & 
                                                    !grepl("CHCF", Facility) & 
                                                    !grepl("SATF", Facility)) %>% #Facilities with no cases
                                           pull(Facility)), 
                                  function(f){
                                    start_to_peak <- get_growth_phase(f, dat)
                                    out <- dat %>% 
                                      filter(Facility == f) %>% 
                                      filter(Date >= start_to_peak[1] & Date <= start_to_peak[2])
                                    return(out)
                                  }))

saveRDS(dat_exp_phase, here::here("data", "derived", "state_prisons_dat_exp_phase.rds"))

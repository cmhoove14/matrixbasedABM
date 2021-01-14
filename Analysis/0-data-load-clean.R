# ---------------------------------------
# Load and merge CA state prison COVID-19 cases and population
# Chris Hoover Jan 2021
# ---------------------------------------

library(zoo)
library(tidyverse)

# Load Case and population data -------------------

# See data/get for scripts to get and process data from online sources

uclabb <- readRDS(here::here("data", "raw", "ucla_law_covid_behind_bars_github_data_1-11-21.rds"))
pop_sps <- readRDS(here::here("data", "derived", "cdcr_population_ts_2020-01-11.rds")) %>% 
  mutate(dplyr::across(.cols = c("Capacity", "Design_Capacity", "Percent_Occupied", "Staffed_Capacity"),
                .fns = as.numeric))

# First generate lookup table to merge the two datasets ----------------
pop_facs <- unique(pop_sps$Facility)
case_facs <- unique(uclabb$Name)

facility_lookup_table <- data.frame(Facility_pop = pop_facs,
                                    Facility_case = NA)

# Match 3/4 letter codes for facilities
for(i in 1:length(facility_lookup_table$Facility_pop)){
  fac_code <- gsub(".*\\((.*)\\).*", "\\1", facility_lookup_table$Facility_pop[i])
  
  init_grep <- grepl(fac_code, case_facs)
  
  if(sum(init_grep) == 1){
    fac_case <- case_facs[which(init_grep)]
  } else {
    fac_case <- sum(init_grep)
  }
  
  facility_lookup_table$Facility_case[i] <- fac_case
}

# Above works for most, but some require manual assignment
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "Calipatria State Prison (CAL)")] <- "CAL CALIPATRIA STATE PRISON"
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "Centinela State Prison (CEN)")] <- "CEN CALIFORNIA STATE PRISON CENTINELA"
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "California Health Care Facility - Stockton (CHCF)")] <- "CALIFORNIA HEALTH CARE FACILITY CHCF STOCKTON"
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "California State Prison Corcoran (COR)")] <- "COR CALIFORNIA STATE PRISON CORCORAN"
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "California Rehabilitation Center (CRC)")] <- "CRC CALIFORNIA REHABILITATION CENTER"
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "Ironwood State Prison (ISP)")] <- "ISP IRONWOOD STATE PRISON"
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "California State Prison Sacramento (SAC)")] <- "SAC CALIFORNIA STATE PRISON SACRAMENTO"
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "California Substance Abuse Treatment Facility (SATF)")] <- "SATF CSP CALIFORNIA SUBSTANCE ABUSE TREATMENT FACILITY AND STATE PRISON CORCORAN"
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "California State Prison Solano (SOL)")] <- "SOL CALIFORNIA STATE PRISON SOLANO"
facility_lookup_table$Facility_case[which(facility_lookup_table$Facility_pop == "Valley State Prison (VSP)")] <- "VSP VALLEY STATE PRISON"

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

# Function to clean, interpolate, and smooth vector of values. Used below to convert cumulative counts into incident counts, skipping over data errors that present as negative changes and then smoothing 
clean_interpolate <- function(vec){
  # Find values that result in negative changes and replace with NA
  diff_vec <- c(NA, diff(vec))
  vec2 <- vec
  vec2[is.na(vec)] <- NA
  vec2[diff_vec < 0] <- NA
  
  # Fill in NAs via interpolation then get new differences and smooth
  vec_interp <- na.approx(vec2, na.rm = FALSE)
  
  return(vec_interp)
}

# final processing to drop superfluous columns, interpolate population, derive smoothed and corrected case incidence
fin_dat <- pop_case_merge %>%
  rename(Facility = Facility_pop) %>%
  group_by(Facility) %>%
  padr::pad() %>%
  mutate(
    Resident_Outbreak_Start = min(Date[which(Residents.Confirmed > 0)], na.rm = T),
    Resident_Outbreak_Day = as.numeric(Date - Resident_Outbreak_Start),
    Staff_Outbreak_Start = min(Date[which(Staff.Confirmed > 0)], na.rm = T),
    Staff_Outbreak_Day = as.numeric(Date - Staff_Outbreak_Start),
    Pop_interpolated        = na.approx(Capacity, na.rm = FALSE),
    Residents_Confirmed2    = clean_interpolate(Residents.Confirmed),
    New_Resident_Cases_7day = zoo::rollapply(data = Residents_Confirmed2 - lag(Residents_Confirmed2), 
                                             width = 7,
                                             FUN = mean,
                                             na.rm = T,
                                             fill = NA,
                                             align = "right"),
    Residents_Recovered2 = clean_interpolate(Residents.Recovered),
    Residents_Recovered_7day = zoo::rollapply(na.approx(Residents.Recovered, na.rm = F),
                                             width = 7,
                                             FUN = mean,
                                             na.rm = T,
                                             fill = NA,
                                             align = "right"),
    Residents_Active_7day = zoo::rollapply(Residents_Confirmed2 - Residents_Recovered2,
                                           width = 7,
                                           FUN = mean,
                                           na.rm = T,
                                           fill = NA,
                                           align = "right"),
    Residents_Deaths_7day = zoo::rollapply(na.approx(Residents.Deaths, na.rm = F),
                                           width = 7,
                                           FUN = mean,
                                           na.rm = T,
                                           fill = NA,
                                           align = "right"),
    Staff_Confirmed2    = clean_interpolate(Staff.Confirmed),
    New_Staff_Cases_7day = zoo::rollapply(data = Staff_Confirmed2 - lag(Staff_Confirmed2), 
                                             width = 7,
                                             FUN = mean,
                                             na.rm = T,
                                             fill = NA,
                                             align = "right"),
    Staff_Recovered2 = clean_interpolate(Staff.Recovered),
    Staff_Recovered_7day = zoo::rollapply(na.approx(Staff.Recovered, na.rm = F),
                                              width = 7,
                                              FUN = mean,
                                              na.rm = T,
                                              fill = NA,
                                              align = "right"),
    Staff_Active_7day = zoo::rollapply(Staff_Confirmed2 - Staff_Recovered2,
                                           width = 7,
                                           FUN = mean,
                                           na.rm = T,
                                           fill = NA,
                                           align = "right"),
    Staff_Deaths_7day = zoo::rollapply(na.approx(Staff.Deaths, na.rm = F),
                                           width = 7,
                                           FUN = mean,
                                           na.rm = T,
                                           fill = NA,
                                           align = "right")
  ) %>% 
  dplyr::select("Facility", "Date", "Resident_Outbreak_Day", "Staff_Outbreak_Day", "Pop_interpolated", 
                "Residents.Confirmed", "Residents_Confirmed2", "New_Resident_Cases_7day",
                "Residents.Recovered", "Residents_Recovered2", "Residents_Recovered_7day",
                "Residents_Active_7day",
                "Residents.Deaths", "Residents_Deaths_7day",
                "Staff.Confirmed", "Staff_Confirmed2", "New_Staff_Cases_7day",
                "Staff.Recovered", "Staff_Recovered2", "Staff_Recovered_7day",
                "Staff_Active_7day",
                "Staff.Deaths", "Staff_Deaths_7day",
                "Design_Capacity", "Percent_Occupied", "Staffed_Capacity", 
                "Residents.Tadmin", "Staff.Tested", "Residents.Negative", "Staff.Negative", "Residents.Pending", "Staff.Pending", 
                "Residents.Quarantine", "Staff.Quarantine", "Residents.Active", "Residents.Tested",
                "Address", "Zipcode", "City", "County", "County.FIPS", "Latitude", "Longitude", "source")

saveRDS(fin_dat, here::here("data", "derived", "state_prisons_pop_cases_fin.rds"))

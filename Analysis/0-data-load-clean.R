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
# Add case facility names to population dataset then merge datasets by facility and date
pop_case_merge <- uclabb %>% 
  left_join(facility_lookup_table, by = c("Name" = "Facility_case")) %>% 
  left_join(pop_sps, by = c("Facility_pop" = "Facility",
                            "Date" = "Report_Date"))

# final processing to drop superfluous columns, interpolate population, derive smoothed case incidence
fin_dat <- pop_case_merge %>% 
  rename(Facility = Facility_pop) %>% 
  group_by(Facility) %>% 
  padr::pad() %>% 
  mutate(
    Pop_interpolated        = na.approx(Capacity, na.rm = FALSE),
    new_residents_confirmed = Residents.Confirmed - lag(Residents.Confirmed),
    new_res_cases_rmv_neg   = if_else(new_residents_confirmed < 0,
                                      NA_real_,
                                      new_residents_confirmed),
    New_Resident_Cases_7day = zoo::rollapply(data = new_res_cases_rmv_neg, 
                                             width = 7,
                                             FUN = mean,
                                             na.rm = T,
                                             fill = NA,
                                             align = "right"),
    new_staff_confirmed     = Staff.Confirmed - lag(Staff.Confirmed),
    new_staff_cases_rmv_neg = if_else(new_staff_confirmed < 0,
                                      NA_real_,
                                      new_staff_confirmed),
    New_Staff_Cases_7day    = zoo::rollapply(data = new_staff_cases_rmv_neg, 
                                             width = 7,
                                             FUN = mean,
                                             na.rm = T,
                                             fill = NA,
                                             align = "right")
 
  ) %>% 
  dplyr::select("Facility", "Date", "Capacity","Pop_interpolated", "Residents.Confirmed", "New_Resident_Cases_7day","Staff.Confirmed", "New_Staff_Cases_7day",
                "Design_Capacity", "Percent_Occupied", "Staffed_Capacity", "Facility_Type", 
                 "Residents.Deaths", "Staff.Deaths", "Residents.Recovered", "Staff.Recovered", 
                "Residents.Tadmin", "Staff.Tested", "Residents.Negative", "Staff.Negative", "Residents.Pending", "Staff.Pending", 
                "Residents.Quarantine", "Staff.Quarantine", "Residents.Active", "Residents.Tested",
                "Address", "Zipcode", "City", "County", "County.FIPS", "Latitude", "Longitude", "source")

saveRDS(fin_dat, here::here("data", "derived", "state_prisons_pop_cases_fin.rds"))

# ---------------------------------------
# Fitting Depopulation SIR model to CA State Prisons data
# Chris Hoover Jan 2021
# ---------------------------------------

# Load Data -------------------
uclabb <- readRDS(here::here("data", "raw", "ucla_law_covid_behind_bars_github_data_1-11-21.rds"))
pop_sps <- readRDS(here::here("data", "derived", "cdcr_population_ts_2020-01-11.rds")) %>% 
  mutate(dplyr::across(.cols = c("Capacity", "Design_Capacity", "Percent_Occupied", "Staffed_Capacity"),
                .fns = as.numeric))

pop_dates <- unique(pop_sps$Report_Date)

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

# Add case facility names to population dataset then merge datasets by facility and date
pop_case_merge <- pop_sps %>% 
  filter(Facility_Type == "Male Institution") %>% 
  left_join(facility_lookup_table, by = c("Facility" = "Facility_pop")) %>% 
  left_join(uclabb, by = c("Facility_case" = "Name",
                           "Report_Date" = "Date")) %>% 
    group_by(Facility) %>% 
    mutate(
      new_residents_confirmed  = Residents.Confirmed - lag(Residents.Confirmed),
      new_staff_confirmed      = Staff.Confirmed - lag(Staff.Confirmed),
      new_residents_death      = Residents.Deaths - lag(Residents.Deaths),
      new_staff_death          = Staff.Deaths - lag(Staff.Deaths),
      new_residents_recovered  = Residents.Recovered - lag(Residents.Recovered),
      new_staff_recovered      = Staff.Recovered - lag(Staff.Recovered),
      new_residents_tested     = Residents.Tested - lag(Residents.Tested),
      new_staff_tested         = Staff.Tested - lag(Staff.Tested),
      new_residents_quarantine = Residents.Quarantine - lag(Residents.Quarantine),
      new_staff_quarantine     = Staff.Quarantine - lag(Staff.Quarantine)
    )
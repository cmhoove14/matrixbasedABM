# ---------------------------------------
# Estimate Rt through time for state prison facilities
# Chris Hoover Jan 2021
# ---------------------------------------

library(EpiNow2)
library(data.table)
library(future)
library(tidyverse)

options(mc.cores = parallel::detectCores())

#################################################################
# More complex method as implemented in EpiNow2 that calls Stan
# This takes a long time 
#################################################################

# Reporting delay, generation time, and serial interval parameters to be used for estimating Rt in all runs -----------------
# Reporting delay distribution (time from infection to report)
reporting_delay <- estimate_delay(rlnorm(250,  log(5), 1),
                                  max_value = 15, bootstraps = 100)


# Generation time and incubation period distributions taken from package
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# Stan options
stan_use <- stan_opts()
stan_use$cores = 4
stan_use$warmup = 200
stan_use$seed = 430
stan_use$iter = 800

# Function to estimate Rt in single facility -------------------
est_Rt <- function(df, facility){
  
  # Data frame with date, confirm (number new cases), and region (defining independent units)
  inc_dat <- df %>% 
    filter(Facility == facility & 
             Resident_Outbreak_Day >= 0) %>% 
    ungroup %>% 
    mutate(pos = round(Residents_Confirmed2 - lag(Residents_Confirmed2))) %>% 
    dplyr::select(Date, pos) %>% 
    rename(date = Date,
           confirm = pos)
  
  # Generate estimates
  estimates <- EpiNow2::epinow(reported_cases = inc_dat, 
                               generation_time = generation_time,
                               delays = delay_opts(incubation_period, reporting_delay),
                               stan = stan_use,
                               horizon = 0,
                               verbose = TRUE)
  
  # Return summarised esimates
  return(estimates)
}

# test_Rt <- est_Rt(df = dat,
#                   facility = unique(dat$Facility)[1])

# Estimate Rt in all facilities in parallel -------------------
  # Data frame with date, confirm (number new cases), and region (defining independent units)
  inc_dat <- dat %>% 
    filter(Resident_Outbreak_Day >= -1 | Staff_Outbreak_Day >= -1) %>% 
    mutate(pos = round((Residents_Confirmed2+Staff_Confirmed2) - lag(Residents_Confirmed2+Staff_Confirmed2)),
           pos2 = if_else(pos < 0, 0, pos)) %>% 
    dplyr::select(Date, pos2, Facility) %>% 
    ungroup() %>% 
    rename(date = Date,
           confirm = pos2, 
           region = Facility)
  
  


# create "outer" workers
n_cores_per_worker <- 4
n_workers <- floor(parallel::detectCores()/n_cores_per_worker)

setup_future(
  inc_dat,
  strategies = c("multiprocess", "multiprocess"),
  min_cores_per_worker = n_cores_per_worker
)

# Generate estimates
estimates <- EpiNow2::regional_epinow(reported_cases = inc_dat, 
                                      generation_time = generation_time,
                                      delays = delay_opts(incubation_period, reporting_delay),
                                      rt = rt_opts(prior = list("mean" = 2,
                                                                "sd" = 1)),
                                      stan = stan_use,
                                      horizon = 0,
                                      verbose = TRUE)

saveRDS(estimates$summary$summarised_measures$rt,
        here::here("data", "derived", "Rt_estimates_Staff&Residents_burn200_samp800.rds"))
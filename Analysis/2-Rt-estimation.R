# ---------------------------------------
# Estimate Rt through time for state prison facilities
# Chris Hoover Jan 2021
# ---------------------------------------

library(EpiNow2)
library(data.table)
library(future)
library(ggplot2)
library(tidyverse)

options(mc.cores = 4)

# Load data (from 0) -------------------
dat <- readRDS(here::here("data", "derived", "state_prisons_pop_cases_fin.rds")) %>% 
  filter(!is.na(Facility))

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
stan_use$warmup = 1000
stan_use$seed = 430
stan_use$iter = 3000

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
  inc_dat <- df %>% 
    filter(Resident_Outbreak_Day >= 0) %>% 
    mutate(pos = round(Residents_Confirmed2 - lag(Residents_Confirmed2))) %>% 
    dplyr::select(Date, pos, Facility) %>% 
    ungroup() %>% 
    rename(date = Date,
           confirm = pos, 
           region = Facility)
  
  


# create "outer" workers
n_cores_per_worker <- 4
n_workers <- detectCores()/n_cores_per_worker

setup_future(
  dat,
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













# Estimate for all facilities in parallel. --------------------
# Setup creates workers each with 4 cores. Taken from https://stackoverflow.com/questions/49436580/parallelization-with-multiple-cores-per-worker


workers <- makeCluster(n_workers)
# register outer_workers
registerDoParallel(workers)

clusterExport(workers, 
              c("dat", generation_time, incubation_period, reporting_delay, est_Rt_all)


# assuming you use foreach directly
foreach(i = 1L:2L) %dopar% {
  foreach(j = 1L:2L) %dopar% {
    # code
  }
  
  NULL
}

# stop inner workers
clusterEvalQ(outer_workers, {
  stopCluster(inner_workers)
  registerDoSEQ()
  NULL
})

stopCluster(outer_workers); registerDoSEQ()


ggplot(rt_dat) +
  geom_col(data = inc_dat,
           aes(x = date, y = confirm/1e2),
           fill = "grey50", alpha = 0.5) +
  geom_col(data = sf_test,
           aes(x = Date, y = pct),
           fill = "darkred", alpha = 0.5) +
  geom_line(aes(x = date,y = mean), col = "darkblue", size = 1.2) +
  geom_ribbon(aes(x = date, ymin = mean-sd, ymax = mean+sd),
              fill = "blue", alpha = 0.5) +
  theme_classic() +
  geom_hline(lty = 2, yintercept = 1) +
  labs(x = "Date",
       y = expression(R[t]),
       title = "SF County Confirmed Cases and Rt Estimate")

# ---------------------------------------
# Fit SEIR and identify effect of depopulation
# Chris Hoover Jan 2021
# ---------------------------------------

library(deSolve)
library(tidyverse)

source(here::here("R", "SEIR_Fxs.R"))

dat <- readRDS(here::here("data", "derived", "state_prisons_pop_cases_fin.rds")) %>% 
  filter(!is.na(Facility) & 
           !grepl("CHCF", Facility) & 
           !grepl("SATF", Facility)) # Both of these facilities had 0 cases. Both seem to be specilized for heatlhcare/treatment, 

######################################
# Start with San Quentin as example
######################################
# Setup data ---------------
sq <- dat %>% filter(Facility == "San Quentin State Prison (SQ)")

# Get start of outbreak
sq_t0 <- sq %>% 
  filter(Resident_Outbreak_Day == 0) %>% 
  pull(Date)

# Prep data for use in fitting
SQ_DAT <- sq %>% 
  ungroup() %>% 
  filter(Date >= sq_t0) %>% 
  rename(I = Residents_Active,
         D = Residents_Deaths2,
         t = Resident_Outbreak_Day) %>% 
  dplyr::select(Date, t, I, D, Nt, N0) %>% 
  drop_na()

# Get function returns population vector
sq_N_fx <- approxfun(SQ_DAT %>% 
                       pull(Nt))



# Initial parameter space to sample from -----------------
n_sweeps <- 1000
set.seed(430)
par_sweeps <- cbind(runif(n_sweeps, 1, 10),               # R0 prior
                    runif(n_sweeps, 0, 1),                # sigma (incubation period) prior
                    runif(n_sweeps, 0, 1),                # gamma (infectious duration) prior
                    rep(0,n_sweeps),                      # alpha (effect of depopulation on transmission)
                    rep(1,n_sweeps),                      # rhoS (depopulation fraction that are susceptible)  
                    rep(0,n_sweeps),                      # rhoE  (depopulation fraction that are exposed)
                    rep(0,n_sweeps),                      # rhoI  (depopulation fraction that are infected)
                    rep(0,n_sweeps))                      # rhoR  (depopulation fraction that are recovered)

# Test run -------------
x <- par_sweeps[1,]

Y0 <- c(sq_N_fx(1) - 1, 1, 0,  0)

test_run <- seir_discrete(TIME = 60+ceiling(1/x[2]), # When SQ outbreak levels out, plus incubation period
                          Y    = Y0, 
                          PAR  = x, 
                          N_FX = sq_N_fx)

test_N <- matrixStats::rowSums2(test_run[,1:4])

View(cbind(test_run[,5], test_N))

# Sweep to get initial par estimates------------------------
  sweep_lls <- apply(par_sweeps, 1, FUN = function(x){
    Y0 <- c(sq_N_fx(1) - 1, 1, 0,  0)
    
    seir_discrete(TIME = 60+ceiling(1/x[2]), # When SQ outbreak levels out, plus incubation period
                  Y    = Y0, 
                  PAR  = x, 
                  N_FX = sq_N_fx)

  })

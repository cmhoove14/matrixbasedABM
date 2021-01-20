# ---------------------------------------
# PLots of Rt through time
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
par_sweeps <- cbind(runif(n_sweeps, 0,10),    # E0 prior
                    runif(n_sweeps, 1, 10),   # R0 prior
                    runif(n_sweeps, 0, 1),   # sigma (incubation period) prior
                    runif(n_sweeps, 0, 1))   # gamma (infectious duration) prior

# Sweep to get initial par estimates
sweep_lls <- apply(par_sweeps, 1, FUN = function(x){
  seir_ll(THETA = x,
          DATA = SQ_DAT,
          MOD = seir_ode,
          TMAX = 60, # When SQ outbreak levels out
          POP_FX = sq_N_fx)
})

# Check fit of initial estimates
init_pars <- par_sweeps[which.min(sweep_lls),]

init_sim <- sim_seir(States0 = c("E" = init_pars[1], "I" = 1, "R" = 0),
                     tsim = 1:60,
                     mod = seir_ode,
                     theta = init_pars[2:4],
                     pop_force = sq_N_fx)

plot(SQ_DAT$t[1:116], SQ_DAT$I[1:116], pch = 16, col = 2,
     ylab = "Active Infections", 
     xlab = "Time since first confirmed case", 
     main = "San Quentin Outbreak")
  lines(init_sim$time, init_sim$I, col = 4)  
  
# Improve fit -----------------------
  opt_pars <- optim(par = init_pars,
                    fn = seir_ll,
                    method = "L-BFGS-B",
                    lower = c(0, 1, 0, 0),
                    upper = c(Inf, Inf, 1, 1),
                    hessian = T,
                    DATA = SQ_DAT,
                    MOD = seir_ode,
                    TMAX = 60, # When SQ outbreak levels out
                    POP_FX = sq_N_fx)
  
  best_pars <- opt_pars$par
  
  best_sim <- sim_seir(States0 = c("E" = best_pars[1], "I" = 1, "R" = 0),
                       tsim = 1:60,
                       mod = seir_ode,
                       theta = best_pars[2:4],
                       pop_force = sq_N_fx)
  
    lines(best_sim$time, best_sim$I, col = 3)  
  

    
# Estimate target parameter in depopulation model-----------------    
    par_sweeps2 <- cbind(runif(n_sweeps, 0,10),    # E0 prior
                        runif(n_sweeps, 1, 10),   # R0 prior
                        runif(n_sweeps, 0, 1),   # sigma (incubation period) prior
                        runif(n_sweeps, 0, 1),     # gamma (infectious duration) prior
                        runif(n_sweeps, -2, 2))  # alpha (pop_red) prior
    
    # Sweep to get initial par estimates
    sweep_lls2 <- apply(par_sweeps2, 1, FUN = function(x){
      seir_ll(THETA = x,
              DATA = SQ_DAT,
              MOD = seir_ode_pop_change,
              TMAX = 60, # When SQ outbreak levels out
              POP_FX = sq_N_fx)
    })
    
    # Check fit of initial estimates
    init_pars2 <- par_sweeps2[which.min(sweep_lls2),]
    
    init_sim2 <- sim_seir(States0 = c("E" = init_pars2[1], "I" = 1, "R" = 0),
                          tsim = 1:60,
                          mod = seir_ode_pop_change,
                          theta = init_pars2[2:5],
                          pop_force = sq_N_fx)
    
      lines(init_sim2$time, init_sim2$I, col = 4, lty = 2)  
    
  # Improve fit -----------------------
      opt_pars2 <- optim(par = init_pars2,
                        fn = seir_ll,
                        method = "L-BFGS-B",
                        lower = c(0, 1, 0, 0, -2),
                        upper = c(Inf, Inf, 1, 1, 2),
                        hessian = T,
                        DATA = SQ_DAT,
                        MOD = seir_ode_pop_change,
                        TMAX = 60, # When SQ outbreak levels out
                        POP_FX = sq_N_fx)
      
      best_pars2 <- opt_pars2$par
      
      best_sim2 <- sim_seir(States0 = c("E" = best_pars2[1], "I" = 1, "R" = 0),
                           tsim = 1:60,
                           mod = seir_ode_pop_change,
                           theta = best_pars2[2:5],
                           pop_force = sq_N_fx)
      
      lines(best_sim2$time, best_sim2$I, col = 3 , lty = 2)  
      
      
      
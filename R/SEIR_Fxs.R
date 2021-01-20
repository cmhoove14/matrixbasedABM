

library(deSolve)

# Function to simulate with no effect of depopulation on transmission
seir_ode<-function(t, Y, par, N_fx){
# State variables  
  E<-Y[1]
  I<-Y[2]
  R<-Y[3]

# Parameters  
  R0   <-par[1]
  sigma<-par[2]
  gamma<-par[3]
  beta <- R0*gamma

# Population size forcing variable  
  N <- N_fx(t)
  S <- N - E - I - R 

# Transitions  
  dEdt = beta*I*S/N - sigma*E
  dIdt = sigma*E - gamma*I
  dRdt = gamma*I 

  return(list(c(dEdt, dIdt, dRdt)))
}

# Function to simulate with effects of depopulation
seir_ode_pop_change<-function(t, Y, par, N_fx){
# State variables  
  E<-Y[1]
  I<-Y[2]
  R<-Y[3]

# Parameters  
  R0    <-par[1]
  sigma <-par[2]
  gamma <-par[3]
  alpha <- par[4]
  beta  <- R0*gamma
  
# Population size forcing variable  
  N     <- N_fx(t)
  Prop_N0 <- 1-N/N_fx(1)
  beta_red <- beta*(1-alpha*Prop_N0)
  S <- N - E - I - R
  
# Transitions  
  dEdt = beta_red*I*S/N - sigma*E
  dIdt = sigma*E - gamma*I
  dRdt = gamma*I 

  return(list(c(dEdt, dIdt, dRdt)))
}

# Function to run simulation
sim_seir <- function(States0,
                     tsim,
                     mod,
                     theta,
                     pop_force){
  
  sim <- as.data.frame(ode(y = States0,
                           times = tsim,
                           func = mod,
                           parms = theta,
                           N_fx = pop_force))
  
  return(sim)
}

# Function to return likelihood of model fit given input parameters (theta)
seir_ll <- function(THETA, DATA, MOD, TMAX, POP_FX, PLOT = F){
  E0   <- THETA[1]
  pars <- THETA[2:length(THETA)]
  
  init_states <- c("E" = E0, "I" = 1, "R" = 0)
  
  sim <- sim_seir(States0 = init_states,
                  tsim = 1:TMAX,
                  mod = MOD,
                  theta = pars,
                  pop_force = POP_FX)
  
  sim_I <- sim$I
  
  if(PLOT) lines(sim$time, sim$I, col = 4)

  negLL <- sum((DATA$I-sim_I)^2)
  
  return(negLL)
}

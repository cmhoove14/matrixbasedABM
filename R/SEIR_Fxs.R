

library(deSolve)

# Function to simulate with no effect of depopulation on transmission ------------
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


seir_ode_pop_change<-function(t, Y, par, N_fx){
# State variables  
  S<-Y[1]
  E<-Y[2]
  I<-Y[3]
  R<-Y[4]

# Parameters  
  R0    <- par[1]
  sigma <- par[2]
  gamma <- par[3]
  alpha <- par[4]
  rhoS  <- par[5]
  rhoE  <- par[6]
  rhoI  <- par[7]
  rhoR  <- par[8]
  beta  <- R0*gamma
  
# Population size forcing variable  
  N        <- N_fx(t)
  rmved    <- 1-N_fx(t)/N_fx(t-1)
  beta_red <- beta*(1-alpha*rmved)
  S        <- N - E - I - R
  
# Transitions  
  dSdt = -beta_red*I*S/N - rhoS*rmved*S 
  dEdt = beta_red*I*S/N - sigma*E - rhoE*rmved*E
  dIdt = sigma*E - gamma*I - rhoI*rmved*I
  dRdt = gamma*I - rhoR*rmved*R

  return(list(c(dSdt, dEdt, dIdt, dRdt)))
}

# Function to run simulation -------------------
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

# Function to return likelihood of model fit given input parameters (theta) ---------------------
seir_ll <- function(THETA, DATA, MOD, TMAX, POP_FX, PLOT = F){
  E0   <- THETA[1]
  pars <- THETA[2:length(THETA)]
  
  init_states <- c("S" = POP_FX(1)-E0, 
                   "E" = E0, 
                   "I" = 1, 
                   "R" = 0)
  
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


# Function to simulate discrete model with effects of depopulation --------------------
seir_discrete <- function(TIME, Y, PAR, N_FX){
  # To return
  OUT <- matrix(NA, ncol = length(Y)+1, nrow = TIME+1)
  
  # State variables  
  S<-Y[1]
  E<-Y[2]
  I<-Y[3]
  R<-Y[4]
  N<-S+E+I+R
  
  # Parameters  
  R0    <- PAR[1]
  sigma <- PAR[2]
  gamma <- PAR[3]
  alpha <- PAR[4]
  rhoS  <- PAR[5]
  rhoE  <- PAR[6]
  rhoI  <- PAR[7]
  rhoR  <- PAR[8]
  beta  <- R0*gamma
  
  # Setup loop
  OUT[1,] <- c(S,E,I,R,NA)
  OUT[,5] <- N_FX(1:(TIME+1))      # Pre-fill population size 
  pop_diff <- c(NA,diff(OUT[,5]))  # Population change
  t <- 2
  
  while(t < TIME){
    St       <- OUT[t-1,1]
    Et       <- OUT[t-1,2]
    It       <- OUT[t-1,3]
    Rt       <- OUT[t-1,4]
    Nt       <- OUT[t-1,5]

    rmved    <- 1-Nt/N
    beta_red <- beta*(1-alpha*rmved)
    
    Stp1 <- St - beta_red*It*St/Nt + rhoS*pop_diff[t] 
    Etp1 <- Et + beta_red*It*St/Nt - sigma*Et + rhoE*pop_diff[t]
    Itp1 <- It + sigma*Et - gamma*It + rhoI*pop_diff[t]
    Rtp1 <- gamma*It + rhoR*pop_diff[t]
    
    OUT[t,1:4] <- c(Stp1, Etp1, Itp1, Rtp1)
    t <- t+1
  }
  
  return(OUT)
  
}


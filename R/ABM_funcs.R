#------------------------
# COVID ABM for aggregate settings
#------------------------


# Helper/Util functions ---------------- 

#' @title Not in 
#' 
#' @description Opposite of `%in%`
#' 
#' @return function for opposite of `%in%``
#' @export
#' 
`%!in%` <- Negate(`%in%`)







#' @title Symmetric matrix
#' 
#' @description make an unsymmetrical matrix symmetrical  
#' 
#' @param mat input matrix
#' 
#' @return symmetric matrix
#' @export

sym_mat <- function(mat){
  mat[lower.tri(mat)] = t(mat)[lower.tri(mat)]
  return(mat)
}









#' @title Summarize Infection
#' 
#' @description Function to summarize infection vector with individual states into aggregate
#' 
#' @param x infections status vector from `ABM` run
#' 
#' @return vector of length 9 with integer counts of number of agents in each class
#' @export
#' 
sum.inf <- function(x) {
  S   = sum(x == "S")
  E   = sum(x == "E")
  Ip  = sum(x == "Ip")
  Ia  = sum(x == "Ia")
  Im  = sum(x == "Im")
  Imh = sum(x == "Imh")
  Ih  = sum(x == "Ih")
  D   = sum(x == "D")
  R   = sum(x == "R")
  
  return(c(S, E, Ip, Ia, Im, Imh, Ih, D, R))
}

#-------------------------
# Infection functions
#-------------------------

#' @title latent period
#'  
#' @description Draw from distribution of proposed latent periods. Used in `next.state`
#' 
#' @param n_E Number of draws
#' @param shape.l Shape of gamma distribution
#' @param scale.l Scale of gamma distribution
#' 
#' @return numeric of time spent in latent period (E)
#' @export
#'        

t_latent <- function(n_E,
                     shape.l = 4,
                     scale.l = 1) {
  rgamma(n_E, shape.l, scale.l)
} 
      




#' @title Presymptomatic period
#'  
#' @description Draw from distribution of proposed presymptomatic periods (basically incubation-latent periods) to model presymptomatic transmission. Used in `next.state`
#' 
#' @param n_Ip Number of draws
#' @param shape Shape of gamma distribution
#' @param scale Scale of gamma distribution
#' 
#' @return numeric of time spent in presymptomatic period (Ip)
#' @export
#'        
t_presymp <- function(n_Ip,
                      shape.ip = 10,
                      scale.ip = 7) {
  rgamma(n_Ip, shape.ip, scale.ip)
} 







#' @title Asymptomatic period
#'  
#' @description Draw from distribution of proposed asymptomatic period. Used in `next.state`
#' 
#' @param n_Ia Number of draws
#' @param shape.ia Shape of gamma distribution
#' @param scale.ia Scale of gamma distribution
#' 
#' @return numeric of time spent in presymptomatic period (Ip)
#' @export
#'        
t_asymp <- function(n_Ia,
                    shape.ia = 12,
                    scale.ia = 2) {
  rgamma(n_Ia, shape.ia, scale.ia)
} 








#' @title Mild infection duration
#'  
#' @description Draw from distribution of proposed length spent with minor symptoms. Used in `next.state`
#' 
#' @param n_Im number of draws
#' @param shape.im Shape parameter of gamma distn
#' @param scale.im Scale parameter of gamma distn
#' 
#' @return numeric of time spent in mildly symptomatic (Im) state
#' @export
#'        
t_msymp <- function(n_Im,
                    shape.im = 6,
                    scale.im = 1) {
  rgamma(n_Im, shape.im, scale.im)
}







#' @title Symptom onset to hospitalization
#'  
#' @description Draw from distribution of proposed time spent with minor symptoms before being hospitalized for severe infections (i.e. those with B(1,p.sevsymp==1)). Used in `next.state`
#' 
#' @param n_Imh number of draws
#' @param shape.imh shape of gamma distn
#' @param scale.imh scale of gamma distn
#' 
#' @return numeric of time spent mildly symptomatic before hospitalization (Imh)
#' @export
#'        
t_mtosev <- function(n_Imh,
                     shape.imh = 3,
                     scale.imh = 1) {
  rgamma(n_Imh, shape.imh, scale.imh)
}









#' @title time spent severely symptomatic (in hospital)
#'  
#' @description Draw from distribution of proposed time spent with severe symptoms before dying or being discharged. Used in `next.state`
#' 
#' @param n_Ih number of draws
#' @param shape.ih shape of gamma distn
#' @param scale.ih scale of gamma distn
#' 
#' @return numeric of time spent hospitalized (Ih)
#' @export
#'        
t_sevsymp <- function(n_Ih,
                      shape.ih = 6,
                      scale.ih = 0.5) {
  rgamma(n_Ih, shape.ih, scale.ih)
}










#' @title Probability of symptomatic infection
#'  
#' @description Age-dependent probabilities of having clinical (symptomatic) infection. Used in `next.state`. From https://www.nature.com/articles/s41591-020-0962-9/figures/8
#' 
#' @param age age of person
#' 
#' @return numeric of probability of clinical (symptomatic).
#' @export
#'
p_symp <- function(age) {
  
  n <- length(age)
  
  n[age %in% c(0:9)]   <- 0.29
  n[age %in% c(10:19)] <- 0.21
  n[age %in% c(20:29)] <- 0.27
  n[age %in% c(30:39)] <- 0.33
  n[age %in% c(40:49)] <- 0.4
  n[age %in% c(50:59)] <- 0.49
  n[age %in% c(60:69)] <- 0.63
  n[age %in% c(70:79)] <- 0.69
  n[age >= 80]         <- 0.69
  
  return(n)
}














#' @title Probability of severely symptomatic (will be hospitalized) infection
#'  
#' @description Age-dependent probabilities of having severe symptoms. Used in `next.state`
#' 
#' @param age age of person
#' 
#' @return numeric of probability of having severe symptoms.
#' @export
#'
p_sevsymp <- function(age){
  
  n <- length(age)
  
  n[age %in% c(0:9)]   <- 0.004
  n[age %in% c(10:19)] <- 0.004
  n[age %in% c(20:29)] <- 0.01
  n[age %in% c(30:39)] <- 0.04
  n[age %in% c(40:49)] <- 0.09
  n[age %in% c(50:59)] <- 0.13
  n[age %in% c(60:69)] <- 0.19
  n[age %in% c(70:79)] <- 0.2
  n[age >= 80]         <- 0.25 
  
  return(n)
}










#' @title Probability of death
#'  
#' @description Age-dependent probabilities of dying given hospitalization (Ih) Used in `next.state`; from https://www.bmj.com/content/369/bmj.m1923 Fig 4 mean of sex-stratified
#' 
#' @param age age of person
#' 
#' @return numeric of probability of dying.
#' @export
#'
p_mort <- function(age) {
  n <- length(age)
  
  n[age %in% c(0:9)]   <- 0.0005#0.01 Probabilities altered due to too high death rate in abm sims 2020-10-28
  n[age %in% c(10:19)] <- 0.001#0.0205
  n[age %in% c(20:29)] <- 0.01#0.031
  n[age %in% c(30:39)] <- 0.02#0.0475
  n[age %in% c(40:49)] <- 0.03#0.0785
  n[age %in% c(50:59)] <- 0.06#0.12
  n[age %in% c(60:69)] <- 0.1 #0.186
  n[age %in% c(70:79)] <- 0.15 #0.3
  n[age >= 80]         <- 0.25#0.45
  
  return(n)
}










#' @title Time til Next state
#' 
#' @description Take input infection state the time spent in that state  
#' 
#' @param pre.status Infection status that has expired 
#' 
#' @return vector time to be spent in infection state. Time should be converted to `numeric` for subsequent use
#' @export

t_til_nxt <- function(pre.status) {
  n                      <- vector("numeric", length(pre.status))
  n_E                    <- sum(pre.status == "E")
  n_Ip                   <- sum(pre.status == "Ip")
  n_Ia                   <- sum(pre.status == "Ia")
  n_Im                   <- sum(pre.status == "Im")
  n_Imh                  <- sum(pre.status == "Imh")
  n_Ih                   <- sum(pre.status == "Ih")
  
  n[pre.status == "E"]   <- t_latent(n_E)
  n[pre.status == "Ip"]  <- t_presymp(n_Ip)
  n[pre.status == "Ia"]  <- t_asymp(n_Ia)
  n[pre.status == "Im"]  <- t_msymp(n_Im)
  n[pre.status == "Imh"] <- t_mtosev(n_Imh)
  n[pre.status == "Ih"]  <- t_sevsymp(n_Ih)
  
  return(n)
}












#' @title Next State
#' 
#' @description Take input infection state and age and return the next infection state and the time spent in that state  
#' 
#' @param pre.status Infection status that has expired 
#' @param age age of individual for probability of moving to different states
#' 
#' @return vector the next state
#' @export

next_state <- function(pre.status, age) {
  n                                              <- length(pre.status)
  post.status                                    <- rep("R", n)
  p_symps                                        <- p_symp(age)
  p_sevsymps                                     <- p_sevsymp(age)
  p_deads                                        <- p_mort(age)
  p1                                             <- dqrunif(n)
  p2                                             <- dqrunif(n)
  
  post.status[pre.status == "E"]                 <- "Ip"
  post.status[pre.status == "Ip" & p1 > p_symps] <- "Ia"
  post.status[pre.status == "Ip" &
                p1 < p_symps & p2 > p_sevsymps]  <- "Im"
  post.status[pre.status == "Ip" &
                p1 < p_symps & p2 < p_sevsymps]  <- "Imh"
  post.status[pre.status == "Imh"]               <- "Ih"
  post.status[pre.status == "Ih" & p1 < p_deads] <- "D"

  return(post.status)
}












#' @title Generate Infections
#' 
#' @description Generate new infections given contact matrix and indices of people who are infectious. Restricts the contact matrix (`contact.mat`) to include only those columns corresponding to infectious people based on `inf.ind` Then goes through each row and performs a bernoulli trial based on the transmission probability in each cell Equivalent to a chance of infection for every contact if contact exists (else the cell is 0)Returns a vector of row indicies corresponding to new infections to enter the E compartment Infection multiplier allows alteration of the infection probability, e.g. if contacts are with asymptomatics and want to model reduced infectiousness
#' 
#' @param contact.mat matrix representing contact network where entries are transmission probabilities
#' @param inf.ind indices of individuals who are infectious. Matrix will be subset to these columns to simulate infectious contacts
#' @param inf.multiplier multiplier to apply to the infection probabilities in the matrix e.g. to simulate reduction in infectiousness among asymptomatic individuals
#' 
#' @return vector of indicies of new infections (e.g. people who have moved from S to E)
#' @export 
#' 

new_infection <- function(contact.mat, inf.ind, inf.multiplier){
  #drop=F allows apply to proceed even if length(inf.ind=1) since apply expects a matrix 
  #across each row, apply a bernoulli trial to all columns corresponding to infectious individuals
  init.Es <- t(apply(contact.mat[,inf.ind,drop=F], 1, function(e){ 
    sapply(e, function(beta) rbinom(1, 1, (1-exp(-beta*inf.multiplier)))) 
  }))
  # Make sure summing over rows since if inf.ind=1, above returns a 1xN matrix rather than Nx1 rows
  if(dim(init.Es)[1] == 1){
    new.Es <- colSums(init.Es)
  } else {
    new.Es <- rowSums(init.Es)
  }
                   
  return(new.Es)
}















#' @title Simulate Infection
#'  
#' @description Randomly determine if infection occurs from FOI
#' 
#' @param foi  Person's FOI
#'  
#' @return binary of infection occurring or not
#' @export
#'        

foi_infect <- function(foi) {
  p <- dqrunif(length(foi))
  return(as.numeric(p < (1 - exp(-foi))))
}











#' @title Generate random edges in network
#'  
#' @description Function to generate random edges on top of network
#' 
#' @param start.net Network matrix to add random edges to
#' @param r.rate Rate at which individuals add random edges (similar to sociality)
#' @param n.prob individual probabilities of being involved in random edge generation
#' @param r.trans.rate Numeric indicating probability a random contact between a susceptible and infectious results in a new infection
#' 
#' @return Updated contact matrix with random edges added  
#' @export
#'        

add_r_edges <- function(start.net, r.rate, n.prob, r.trans.rate){
  new.net <- start.net
  #Number of random edges generated
    n.r.edges <- rbinom(1, nrow(start.net), r.rate)
  #Allocate random edges to network with individual probability correspondng to n.prob
    r.pairs <- matrix(sample(nrow(start.net), 2*n.r.edges, replace = TRUE, prob = n.prob), ncol=2)
  #If edge already exists in another capacity, keep it  
    pre.edges <- new.net[r.pairs]
    post.edges <- ifelse(pre.edges == 0, r.trans.rate, pre.edges)
    new.net[r.pairs] <- post.edges

  #Make symmetrical  
    new.net <- sym_mat(new.net)
  
  return(new.net)
}















# Main ABM function --------------------

#' @title COVID ABM
#' 
#' @description Simulate COVID transmission across a network of N individuals
#' 
#' @param t.tot total time to run simulation
#' @param dt time step of simulation to run
#' @param pop.ages vector corresponding to age of each individual
#' @param q.probs vector of individual probabilities of quarantining
#' @param n.probs vector of individual probabilities of being in random edge generation
#' @param inf.mat infection status matrix with column 1 filled with initial infection status, N rows, and t.tot/dt columns
#' @param transition.mat transition times matrix (time until moving into next state) with column 1 filled with initial times remaining in starting states, N rows, and t.tot/dt columns
#' @param relation.mat contact network array with dimensions NxN in which each ij entry represents relationship between agent i and agent j   
#' 
#' @return Matrix with columns corresponding to each infection state and rows filled with number people in each state at each time over which the simulation was run
#' @export
#' 

covid_abm <- function(t.tot, dt, 
                      pop.ages, q.probs, n.probs,
                      inf.mat, transition.mat, relation.mat){
  
  out.mat <- matrix(NA, nrow = t.tot/dt, ncol = 9)

# Run simulation
  for(t in 2:(t.tot/dt)){

# Advance transition times
    transition.mat[,t] <- transition.mat[,(t-1)]-dt
  
  # Advance expired states to next state 
    nexts <- which(transition.mat[,t] < 0) # states that expired
    if(length(nexts >0)){
      inf.mat[nexts,t] <- next_state(inf.mat[nexts,(t-1)], pop.ages[nexts]) # function next_state returns new states
      transition.mat[nexts,t] <- t_til_nxt(inf.mat[nexts,t]) # function t_til_nxt returns time spent in new state 
    }
    
# Update network based on epi advances and interventions
  t.net <- relation.mat
  #Quarantine symptomatics   
  if(!is.na(q.probs)){
    t.net <- quarantine_symptomatics(inf.mat[,t], relation.mat, q.probs)  
  }  
  
  # Eliminate school connections if schools closed and increase hh contacts
    if(t.sc <= (t*dt) & (t*dt) <= t.sc.end){
      t.net[which(t.net %in% c("H", "Q"))] <- trans.hh*trans.hh.sc
      t.net[which(t.net == "W")] <- trans.work
      t.net[which(t.net == "S")] <- 0
    # Add random network component to network
      t.net <- add_r_edges(t.net, r.net.prob.sc, n.probs, trans.other)
      
  # Eliminate work and school connections if schools closed and increase hh contacts
    } else if(t.sip <= (t*dt) & (t*dt) <= t.sip.end){
      t.net[which(t.net %in% c("H", "Q"))] <- trans.hh*trans.hh.sip
      t.net[which(t.net == "W")] <- 0
      t.net[which(t.net == "S")] <- 0
    # Add random network component to network
      t.net <- add_r_edges(t.net, r.net.prob.sip, n.probs, trans.other)
      
  # If not in shelter in place or school closure, network remains the same plus random connections    
    } else {
      t.net[which(t.net %in% c("H", "Q"))] <- trans.hh
      t.net[which(t.net == "W")] <- trans.work
      t.net[which(t.net == "S")] <- trans.school
    # Add random network component to network
      t.net <- add_r_edges(t.net, r.net.prob, n.probs, trans.other)
    } 
    
  #store resulting network in network array
    class(t.net) <- "numeric"

#Generate new infections across network 
  # Indices of different types of transmitters
    a.p.transmitters <- which(inf.mat[,t] %in% c("Ip", "Ia"))  
    m.s.transmitters <- which(inf.mat[,t] %in% c("Im", "Imh"))  
   
  # Bernouli trial across all rows times transmitter columns where p(infection)~contact
    # Transmission from pre/asymtpomatic infections with reduction in transmissibility 
      if(length(a.p.transmitters) > 0){
        new.Es1 <- new_infection(t.net, a.p.transmitters, trans.asymp)
      } else {
        new.Es1 <- rep(0, N)
      }

    # Transmissions from mild/severely symptomatic individuals
      if(length(m.s.transmitters) > 0){
        new.Es2 <- new_infection(t.net, m.s.transmitters, 1)
      } else {
        new.Es2 <- rep(0, N)
      }

  # Index of individuls who enter E compartment (new infections)
    new.Es <- which(new.Es1+new.Es2 > 0 & inf.mat[,(t-1)] == "S")
    
  # Update infection status matrix with new Es  
    inf.mat[new.Es,t] <- "E"
    
  # Update transition times matrix for new Es from sample of latent period dist'n
    transition.mat[new.Es,t] <- t_latent(length(new.Es))
  
  # Anyone who hasn't changed infection state remains the same
    sames <- which(is.na(inf.mat[,t]))
    inf.mat[sames,t] <- inf.mat[sames,(t-1)]
      
  # Store infection state summary at time t  
    out.mat[t,] <- sum.inf(inf.mat[,t])
  # On to the next one  
  }
  # Return Epi curve
  return(out.mat)    

}
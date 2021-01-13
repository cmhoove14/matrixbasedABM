library(tidyverse)
library (gridExtra)

#############################################################

# prob that i cases cause j cases
calc_r_ij <- function (i,j,r,k) {
  log_r_ij <- lgamma(j +k*i) - lgamma(j+1) - lgamma(k*i) + 
    k * i * log(k/(r+k)) +
    j * log(r/(r+k))
  r_ij<- exp(log_r_ij)
}

# prob a single introduction results in an extinction
calc_prob_ext <- function(thresh, R, k) {
  clust_size=1:thresh
  s_ij_arr <- calc_r_ij(i=clust_size,j=clust_size-1,R,k)/clust_size
  p_extinct <- sum(s_ij_arr)
}

#############################################################
# Number of introduction per day
#############################################################

# prev <- prevalence
# UNIT_SIZE = number of prisoners
# N_c <- Number of contacts
# SAR <- seconardy attack rate

# N_p = Prev * UNIT_SIZE * N_c * SAR

PREV <- c(1e-4)
#N_O <- 200
UNIT_SIZE <- 1000
N_C_ARR <- seq(2,10,.1)
SAR_ARR <- seq(.01, .05, .001)
#K_P_ARR <- 1e3#c(0.2,1e3)

# Init

res_intro_arr <- tibble(N_c = N_C_ARR) %>% group_by (N_c) %>% do ({
  N_ic <- PREV * UNIT_SIZE * .$N_c[1]
  tibble(SAR = SAR_ARR) %>% group_by (SAR) %>% do ({
    SAR <- .$SAR[1]
    approx_days <- 1/(N_ic*SAR)
    tibble(approx_days = approx_days)
  })
})

g1a <- ggplot(res_intro_arr) +
  scale_fill_distiller(palette = "Spectral") +
  geom_tile(aes(x = N_c, y = SAR, fill = -log(approx_days))) +
#  scale_fill_gradient(low="red", high="blue") +
  xlab("Contacts per prisoner") + ylab("Attack rate") + theme(text = element_text(size=20)) +
  labs(fill = "Average\ndays until\nintroduction\n") + theme(legend.title = element_text(size = 12),legend.text = element_text(size = 12))
(g1a)  

g1b <- ggplot(res_intro_arr %>% filter(SAR %in% seq(0.01, 0.05, 0.01))) +
  geom_point(aes(x = N_c, y = approx_days, col = as.factor(SAR))) +
  xlab("Contacts per prisoner") + ylab("Average days\nuntil introduction") + theme(text = element_text(size=20)) +
  labs(col = "Attack\nrate") + theme(legend.title = element_text(size = 12),legend.text = element_text(size = 12))
(g1b)

#g_intro <- grid.arrange(g1a,g1b)
ggsave('Figs/prob_intro.jpg',plot = g1b)

#############################################################
# Probability of an introduction leading to an outbreak
#############################################################
MAX_CONTAINED <- 20
INTRO_ARR <- c(1,2,5)

R_ARR <- seq (1.5, 3, .1)
K_ARR <- c(0.2, .5, 1, 1e3)

res_prob_ob_arr <- tibble(R = R_ARR) %>% group_by (R) %>% do ({
  R <- .$R[1]
  tibble(k = K_ARR) %>% group_by (k) %>% do ({
    p_extinct <- calc_prob_ext(thresh = MAX_CONTAINED, R = R, k =.$k[1])
    tibble(num_intro = INTRO_ARR,p_ob = 1-p_extinct^INTRO_ARR)
  })
})

#cheat <- tibble(num_intro = INTRO_ARR, label = c('Single introduction','Two introductions','Five introductions'))
#ni.labs <- c('1','2','5')
#names(ni.labs) <- c('Single introduction','Two introductions','Five introductions')
#res_prob_ob_arr <- left_join(res_prob_ob_arr,cheat, by = "num_intro")
g_prob_ob <- ggplot(res_prob_ob_arr) +
  geom_line(aes(x=R, y = p_ob, col = as.factor(k))) +
  facet_wrap(~num_intro) +
  xlab("Reproduction number (R)") + ylab("Outbreak\nprobability") +
  #theme(text = element_text(size=20)) +
  labs(col = "Dispersion\nparameter (k)")
(g_prob_ob)
ggsave('Figs/prob_ob.jpg',plot = g_prob_ob)

#############################################################
# Size of outbreak
#############################################################
# *Size of an outbreak (R, N_p)
#source('prison_seir_hosp.R')

#############################################################
# Impact of decarceration
#############################################################

# Regarding Number introductions / day
PREV <- c(1e-4)
N_O <- 200 # contacts per officer
N_C <- 10 # contacts / per officer 
SAR <- 0.02

# Probability of an outbeak occuring
MAX_CONTAINED <- 20
INTRO_NUM <- 1
K_S <- .5

# Dynamics of outbreak
NUM_INDEX <- 1
UNIT_SIZE <- 1000
PROP_HOSP <- 0.2
TIME_STEP <- .1
INC_TIME <- 4
INF_TIME_MIN <- 4
INF_TIME_MAJ <- 7
HOSP_DUR <- 14
TOT_TIME <- 250

R_ARR <- seq(1.5,3, .5)
CONTROL_ARR <- seq(0,.5,.01)

combo_decarcerate_res <- expand_grid(R = R_ARR, control = CONTROL_ARR) %>% group_by(R,control) %>% do({
  
  # Number infectious contacts for prisoners in 100 days
  adj_num_contacts <- N_C * (1-.$control[1])
  avg_days_for_intro <- 1/(PREV*N_O*adj_num_contacts*SAR)
  number_inefctious_contacts <- 100/avg_days_for_intro
  
  # Prob outbreak in 100 days
  effective_R <- .$R[1] * (1-.$control[1])
  prob_ext <- calc_prob_ext(MAX_CONTAINED, effective_R, K_S)
  prob_ob <- 1 - prob_ext^number_inefctious_contacts
  
  # Average number of infected / hospitalized from an outbreak starting within 100 days
  seir_res <- explore_decarceration(prop_capacity_arr = 1-.$control[1], R_max_arr = .$R[1])
  mean_inf <- prob_ob * seir_res$tot_inf
  mean_hosp <- prob_ob * seir_res$tot_hosp
  
  tibble(mean_inf = mean_inf, mean_hosp = mean_hosp)
})
write.csv(combo_decarcerate_res,"combo_decarcerate_res.csv",row.names = FALSE)
combo_decarcerate_res <- read.csv("combo_decarcerate_res.csv")

simple <- combo_decarcerate_res %>% filter(control == 0) %>% group_by(R) %>% do({
  control_arr <- c(min(CONTROL_ARR), max(CONTROL_ARR))
  simple_inf_arr <- .$mean_inf[1] * (1-control_arr)/ (1-min(CONTROL_ARR))
  tibble(control = control_arr, mean_inf = simple_inf_arr)
})

ggplot(combo_decarcerate_res) +
  geom_line(aes(x=control, y = mean_inf, col = as.factor(R))) +
  geom_line(data = simple, aes(x=control, y = mean_inf, col = as.factor(R)), linetype = "dotted") +
  theme(legend.title = element_text(size = 12),text = element_text(size=20))

ggsave('Figs/combo_impact.jpg')

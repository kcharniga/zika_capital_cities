# This code omits data from all cities with seroprevalence data and saves the results.
# The results are plotted in the "plotting_no_seroprevalence_data.R" script.

library(rstan)
library(ggplot2) #Opening libraries
library(rstan)
library(reshape2)
library(dplyr)
library(bayesplot)
library(loo)
library(rstanarm)
library(rstan)
library(readxl)
library(grid)
library(gridExtra)
library(shinystan)
library(rgl)

# Set up RStan to run on multiple processors
options(mc.cores = parallel::detectCores())

# data
zika <- readRDS("data/capital_cities_data.RDS")
dat <- zika

priors_list <- readRDS("data/priors_list.RDS")
alphaZ <- priors_list[[1]]
gammaZ <- priors_list[[2]]

more_alphas <- rep(1, 24)
more_gammas <- rep(1, 24)

alphaZ <- c(alphaZ, more_alphas)
gammaZ <- c(gammaZ, more_gammas)

# remove all four cities with serological data
dat <- dat[-(1:4),]
alphaZ <- alphaZ[-(1:4)]
gammaZ <- gammaZ[-(1:4)]

stan_data <- list(
  l    = nrow(dat),
  N = dat$pop,
  S = dat$zika_cases,
  NC = dat$neur_cases,
  alphaZ = alphaZ,
  gammaZ = gammaZ,
  Sall = sum(dat$zika_cases),
  NCall = sum(dat$neur_cases),
  Nall = sum(dat$pop))

model_stan  <-  rstan::stanc("Stan_code/main_model.stan")
sm = rstan::stan_model(stanc_ret = model_stan, verbose=FALSE)

#  Run the Model
set.seed(123)
n_iter <- 2000
system.time(fit <- rstan::sampling(sm, 
                                   data=stan_data,
                                   iter = n_iter,
                                   control = list(max_treedepth = 15)))

# Summary statistics
print(fit, probs = c(0.025, 0.975), digits_summary = 8)

for_plotting <- rstan::summary(fit, probs = c(0.025, 0.5, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]

for_plotting_df <- as.data.frame(for_plotting)
library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

NC_df <- df[c(53:76),]
S_df <- df[c(29:52),]
Z_df <- df[c(5:28),]

place <- dat$admin2_name

NC_df$place <- NA
NC_df$place <- place

S_df$place <- NA
S_df$place <- place

Z_df$place <- NA
Z_df$place <- place

removed <- data.frame(type = rep(NA,4), 
                      mean = rep(NA,4), 
                      LL   = rep(NA,4), 
                      UL   = rep(NA,4), 
                      place = c("Sincelejo","Neiva","Cúcuta","Medellín"))

NC_df <- rbind(removed, NC_df)
S_df <- rbind(removed, S_df)
Z_df <- rbind(removed, Z_df)

saveRDS(NC_df, "NC_df_all_four_sero_removed.RDS")
saveRDS(S_df, "S_df_all_four_sero_removed.RDS")
saveRDS(Z_df, "Z_df_all_four_sero_removed.RDS")

# This code omits data one city at a time and saves the results. The results are plotted in 
# the "plotting_capital_cities_sensitivity.R" script

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

# remove Sincelejo
dat <- dat[-(1),]
alphaZ <- alphaZ[-1]
gammaZ <- gammaZ[-1]

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

model_stan  <-  stanc("Stan_code/main_model.stan")
sm = stan_model(stanc_ret = model_stan, verbose=FALSE)

#  Run the Model
set.seed(123)
n_iter <- 2000
system.time(fit <- sampling(sm, 
                            data=stan_data,
                            iter = n_iter,
                            control = list(max_treedepth = 15)))

# Summary statistics
print(fit, probs = c(0.025, 0.975), digits_summary = 8)

for_plotting <- summary(fit, probs = c(0.025, 0.5, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]

for_plotting_df <- as.data.frame(for_plotting)
library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

NC_df <- df[c(59:85),]
S_df <- df[c(32:58),]
Z_df <- df[c(5:31),]

place <- dat$admin2_name

NC_df$place <- NA
NC_df$place <- place

S_df$place <- NA
S_df$place <- place

Z_df$place <- NA
Z_df$place <- place

removed <- data.frame(type = NA, mean = NA, LL = NA, UL = NA, place = "Sincelejo")

NC_df <- rbind(removed, NC_df)
S_df <- rbind(removed, S_df)
Z_df <- rbind(removed, Z_df)

saveRDS(NC_df, "NC_df_SIN_removed.RDS")
saveRDS(S_df, "S_df_SIN_removed.RDS")
saveRDS(Z_df, "Z_df_SIN_removed.RDS")

##################################
##### remove Neiva

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

# remove Neiva
dat <- dat[-(2),]
alphaZ <- alphaZ[-2]
gammaZ <- gammaZ[-2]

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

model_stan  <-  stanc("Stan_code/main_model.stan")
sm = stan_model(stanc_ret = model_stan, verbose=FALSE)

#  Run the Model
set.seed(123)
n_iter <- 2000
system.time(fit <- sampling(sm, 
                            data=stan_data,
                            iter = n_iter,
                            control = list(max_treedepth = 15)))

# Summary statistics
print(fit, probs = c(0.025, 0.975), digits_summary = 8)

for_plotting <- summary(fit, probs = c(0.025, 0.5, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]

for_plotting_df <- as.data.frame(for_plotting)
library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

NC_df <- df[c(59:85),]
S_df <- df[c(32:58),]
Z_df <- df[c(5:31),]

place <- dat$admin2_name

NC_df$place <- NA
NC_df$place <- place

S_df$place <- NA
S_df$place <- place

Z_df$place <- NA
Z_df$place <- place

removed <- data.frame(type = NA, mean = NA, LL = NA, UL = NA, place = "Neiva")

NC_df <- rbind(removed, NC_df)
S_df <- rbind(removed, S_df)
Z_df <- rbind(removed, Z_df)

saveRDS(NC_df, "NC_df_NEI_removed.RDS")
saveRDS(S_df, "S_df_NEI_removed.RDS")
saveRDS(Z_df, "Z_df_NEI_removed.RDS")

#######################################
# remove Cúcuta
# data

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

# remove Cúcuta
dat <- dat[-(3),]
alphaZ <- alphaZ[-3]
gammaZ <- gammaZ[-3]

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

model_stan  <-  stanc("Stan_code/main_model.stan")
sm = stan_model(stanc_ret = model_stan, verbose=FALSE)

#  Run the Model
set.seed(123)
n_iter <- 2000
system.time(fit <- sampling(sm, 
                            data=stan_data,
                            iter = n_iter,
                            control = list(max_treedepth = 15)))

# Summary statistics
print(fit, probs = c(0.025, 0.975), digits_summary = 8)

for_plotting <- summary(fit, probs = c(0.025, 0.5, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]

for_plotting_df <- as.data.frame(for_plotting)
library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

NC_df <- df[c(59:85),]
S_df <- df[c(32:58),]
Z_df <- df[c(5:31),]

place <- dat$admin2_name

NC_df$place <- NA
NC_df$place <- place

S_df$place <- NA
S_df$place <- place

Z_df$place <- NA
Z_df$place <- place

removed <- data.frame(type = NA, mean = NA, LL = NA, UL = NA, place = "Cúcuta")

NC_df <- rbind(removed, NC_df)
S_df <- rbind(removed, S_df)
Z_df <- rbind(removed, Z_df)

saveRDS(NC_df, "NC_df_CUC_removed.RDS")
saveRDS(S_df, "S_df_CUC_removed.RDS")
saveRDS(Z_df, "Z_df_CUC_removed.RDS")

#######################################
# remove Medellin

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

# remove Medellin
dat <- dat[-(4),]
alphaZ <- alphaZ[-4]
gammaZ <- gammaZ[-4]

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

model_stan  <-  stanc("Analysis/Mier_y_Teran_Romero_model/ZIKV/Stan_test_test.stan")
sm = stan_model(stanc_ret = model_stan, verbose=FALSE)

#  Run the Model
set.seed(123)
n_iter <- 2000
system.time(fit <- sampling(sm, 
                            data=stan_data,
                            iter = n_iter,
                            control = list(max_treedepth = 15)))

# Summary statistics
print(fit, probs = c(0.025, 0.975), digits_summary = 8)
for_plotting <- summary(fit, probs = c(0.025, 0.5, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]

for_plotting_df <- as.data.frame(for_plotting)
library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

NC_df <- df[c(59:85),]
S_df <- df[c(32:58),]
Z_df <- df[c(5:31),]

place <- dat$admin2_name

NC_df$place <- NA
NC_df$place <- place

S_df$place <- NA
S_df$place <- place

Z_df$place <- NA
Z_df$place <- place

removed <- data.frame(type = NA, mean = NA, LL = NA, UL = NA, place = "Medellín")

NC_df <- rbind(removed, NC_df)
S_df <- rbind(removed, S_df)
Z_df <- rbind(removed, Z_df)

saveRDS(NC_df, "NC_df_MED_removed.RDS")
saveRDS(S_df, "S_df_MED_removed.RDS")
saveRDS(Z_df, "Z_df_MED_removed.RDS")

#######################
# remove Barranquilla

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

# remove Barranquilla
dat <- dat[-(5),]
alphaZ <- alphaZ[-5]
gammaZ <- gammaZ[-5]

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

model_stan  <-  stanc("Stan_code/main_model.stan")
sm = stan_model(stanc_ret = model_stan, verbose=FALSE)

#  Run the Model
set.seed(123)
n_iter <- 2000
system.time(fit <- sampling(sm, 
                            data=stan_data,
                            iter = n_iter,
                            control = list(max_treedepth = 15)))

# Summary statistics
print(fit, probs = c(0.025, 0.975), digits_summary = 8)

for_plotting <- summary(fit, probs = c(0.025, 0.5, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]

for_plotting_df <- as.data.frame(for_plotting)
library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

NC_df <- df[c(59:85),]
S_df <- df[c(32:58),]
Z_df <- df[c(5:31),]

place <- dat$admin2_name

NC_df$place <- NA
NC_df$place <- place

S_df$place <- NA
S_df$place <- place

Z_df$place <- NA
Z_df$place <- place

removed <- data.frame(type = NA, mean = NA, LL = NA, UL = NA, place = "Barranquilla")

NC_df <- rbind(removed, NC_df)
S_df <- rbind(removed, S_df)
Z_df <- rbind(removed, Z_df)

saveRDS(NC_df, "NC_df_ATL_removed.RDS")
saveRDS(S_df, "S_df_ATL_removed.RDS")
saveRDS(Z_df, "Z_df_ATL_removed.RDS")


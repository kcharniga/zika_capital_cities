# Run model separately for age cats (under 40 and 40+) assuming different hyper-priors.
# This model is run twice, once for the younger age group (old = 0) and once for the older age group (old = 1).
# After each run, the results are saved. Then, they can be loaded back in and plotted together
# at the bottom of this script. 
# The main model code is used for both model runs.

library(rstan)
library(ggplot2)
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
zika <- readRDS("data/capital_cities_age_data.RDS")
dat <- zika

## check order of data so priors match up

###############
old <- 1 ###### Change this to 0 if running the model for younger age group!
###############

# separate data by age
if (old == 1){
  dat <- dplyr::filter(dat, agecat == "40 or more")
} else {
  dat <- dplyr::filter(dat, agecat == "0-39")
}

priors_list <- readRDS("data/priors_list.RDS")
alphaZ <- priors_list[[1]]
gammaZ <- priors_list[[2]]

more_alphas <- rep(1, 24) 
more_gammas <- rep(1, 24) 

if (old == 1){
  alphaZ <- rep(1, 28) # no priors for 40 or more agecat
  gammaZ <- rep(1, 28) 
} else{
  alphaZ <- c(alphaZ, more_alphas)
  gammaZ <- c(gammaZ, more_gammas)
}

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

# extract samples from fit (permuted = TRUE merges all the MCMC chains together)
chains <- extract(fit, permuted = TRUE, inc_warmup = FALSE)

if (old == 1){
  saveRDS(chains, "MCMC_chains_40_or_more.RDS")
} else {
  saveRDS(chains, "MCMC_chains_0_39.RDS")
}

# Print results and prepare them for plotting
print(fit, probs = c(0.025, 0.975), digits_summary = 8)
for_plotting <- summary(fit, probs = c(0.025, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]

for_plotting_df <- as.data.frame(for_plotting)
library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

NC_df <- df[c(61:88),]
S_df <- df[c(33:60),]
Z_df <- df[c(5:32),]

dat2 <- select(dat, admin2_name, agecat) # admin names

NC_df2 <- df[c(90),]
S_df2 <- df[c(89),]
Z_df2 <- df[c(91),]

if (old == 1){
  add_this <- data.frame(admin2_name = "Overall", agecat = "40 or more")
} else {
  add_this <- data.frame(admin2_name = "Overall", agecat = "0-39")
}

NC_df3 <- cbind(NC_df2, add_this)
S_df3 <- cbind(S_df2, add_this)
Z_df3 <- cbind(Z_df2, add_this)

NC_df <- cbind(NC_df, dat2)
S_df <- cbind(S_df, dat2)
Z_df <- cbind(Z_df, dat2)

NC_df <- rbind(NC_df, NC_df3)
S_df <- rbind(S_df, S_df3)
Z_df <- rbind(Z_df, Z_df3)

if (old == 1) {
  saveRDS(NC_df, "NC_df_40_or_more.RDS")
  saveRDS(S_df, "S_df_40_or_more.RDS")
  saveRDS(Z_df, "Z_df_40_or_more.RDS")
} else {
  saveRDS(NC_df, "NC_df_0_39.RDS")
  saveRDS(S_df, "S_df_0_39.RDS")
  saveRDS(Z_df, "Z_df_0_39.RDS")
}

### Re-start R
#### Plot

NC_df_0_39 <- readRDS("NC_df_0_39.RDS")
NC_df_0_39 <- with(NC_df_0_39,  NC_df_0_39[order(admin2_name) , ])
NC_df_0_39$num <- 1:29
NC_df_0_39$num[NC_df_0_39$admin2_name == "Overall"] <- 100

S_df_0_39 <- readRDS("S_df_0_39.RDS")
S_df_0_39 <- with(S_df_0_39,  S_df_0_39[order(admin2_name) , ])
S_df_0_39$num <- 1:29
S_df_0_39$num[S_df_0_39$admin2_name == "Overall"] <- 100

Z_df_0_39 <- readRDS("Z_df_0_39.RDS")
Z_df_0_39 <- with(Z_df_0_39,  Z_df_0_39[order(admin2_name) , ])
Z_df_0_39$num <- 1:29
Z_df_0_39$num[Z_df_0_39$admin2_name == "Overall"] <- 100

NC_df_40_or_more <- readRDS("NC_df_40_or_more.RDS")
NC_df_40_or_more <- with(NC_df_40_or_more,  NC_df_40_or_more[order(admin2_name) , ])
NC_df_40_or_more$num <- 1:29
NC_df_40_or_more$num[NC_df_40_or_more$admin2_name == "Overall"] <- 100

S_df_40_or_more <- readRDS("S_df_40_or_more.RDS")
S_df_40_or_more <- with(S_df_40_or_more,  S_df_40_or_more[order(admin2_name) , ])
S_df_40_or_more$num <- 1:29
S_df_40_or_more$num[S_df_40_or_more$admin2_name == "Overall"] <- 100

Z_df_40_or_more <- readRDS("Z_df_40_or_more.RDS")
Z_df_40_or_more <- with(Z_df_40_or_more,  Z_df_40_or_more[order(admin2_name) , ])
Z_df_40_or_more$num <- 1:29
Z_df_40_or_more$num[Z_df_40_or_more$admin2_name == "Overall"] <- 100

NC_both <- rbind(NC_df_0_39, NC_df_40_or_more)

S_both <- rbind(S_df_0_39, S_df_40_or_more)

Z_both <- rbind(Z_df_0_39, Z_df_40_or_more)

NC_both$admin2_name[NC_both$admin2_name == "San José del Guaviare"] <- "San José"
Z_both$admin2_name[Z_both$admin2_name == "San José del Guaviare"] <- "San José"
S_both$admin2_name[S_both$admin2_name == "San José del Guaviare"] <- "San José"

library(ggplot2)
p1 <- ggplot(NC_both, aes(x=reorder(admin2_name, num), y=mean*10000, color=agecat)) + 
  geom_errorbar(aes(ymin=LL*10000, ymax=UL*10000), width=.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values=c("#F02D3A", "#2E3138")) +
  geom_point(position = position_dodge(width = 0.3)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, color = "black", vjust = 0.5, hjust = 1, size = 10)) +
  theme(axis.text.y = element_text(color = "black", size = 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank()) +
  labs(y = "NC cases reported per\n10,000 ZIKV infections") +
  theme(axis.title.y = element_text(color="black", size=12, face="bold")) +
  theme(legend.title = element_blank())

scaleFUN <- function(x) sprintf("%.2f", x)

p2 <- ggplot(S_both, aes(x=reorder(admin2_name, num), y=mean, color=agecat)) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values=c("#F02D3A", "#2E3138")) +
  geom_point(position = position_dodge(width = 0.3)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, color = "black", vjust = 0.5, hjust = 1, size = 10)) +
  theme(axis.text.y = element_text(color = "black", size = 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank()) +
  labs(y = "Reporting rate") +
  theme(axis.title.y = element_text(color="black", size=12, face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_y_continuous(limits=c(0, 0.03), 
                     breaks = c(0, 0.01, 0.02, 0.03),
                     labels = scaleFUN)

p3 <- ggplot(Z_both, aes(x=reorder(admin2_name, num), y=mean, color=agecat)) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values=c("#F02D3A", "#2E3138")) +
  geom_point(position = position_dodge(width = 0.3)) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, color = "black", vjust = 0.5, hjust = 1, size = 10)) +
  theme(axis.text.y = element_text(color = "black", size = 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank()) +
  labs(y = "Infection attack rate") +
  theme(axis.title.y = element_text(color="black", size=12, face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.title = element_blank())

# put three plots together
egg::ggarrange(p3, p2, p1)

# 8 x6.25 pdf

##############################
# Calculate posterior probabilities (here, males = young and females = old, 
# just being lazy and didn't change the labels)
##############################

chains_m <- readRDS("MCMC_chains_0_39.RDS")
chains_f <- readRDS("MCMC_chains_40_or_more.RDS")

# comparing attack rates
pZall_m <- chains_m$pZall
pZall_f <- chains_f$pZall

pSexM <- rep(NA, 4000)
pSexF <- rep(NA, 4000)

for (i in 1:length(pZall_m)){
  if(pZall_m[i] > pZall_f[i]){
    pSexM[i] <- 1
  } 
  if(pZall_m[i] < pZall_f[i]){
    pSexF[i] <- 1
  } 
}

pSexM_sum <- sum(pSexM, na.rm = TRUE)
pSexF_sum <- sum(pSexF, na.rm = TRUE)

pSexM_sum/4000
pSexF_sum/4000

#####

# comparing reporting rates
pSall_m <- chains_m$pSall
pSall_f <- chains_f$pSall

pSexM <- rep(NA, 4000)
pSexF <- rep(NA, 4000)

for (i in 1:length(pSall_m)){
  if(pSall_m[i] > pSall_f[i]){
    pSexM[i] <- 1
  } 
  if(pSall_m[i] < pSall_f[i]){
    pSexF[i] <- 1
  } 
}

pSexM_sum <- sum(pSexM, na.rm = TRUE)
pSexF_sum <- sum(pSexF, na.rm = TRUE)

pSexM_sum/4000
pSexF_sum/4000

#####

# comparing NC rates
pNCall_m <- chains_m$pNCall
pNCall_f <- chains_f$pNCall

pSexM <- rep(NA, 4000)
pSexF <- rep(NA, 4000)

for (i in 1:length(pNCall_m)){
  if(pNCall_m[i] > pNCall_f[i]){
    pSexM[i] <- 1
  } 
  if(pNCall_m[i] < pNCall_f[i]){
    pSexF[i] <- 1
  } 
}

pSexM_sum <- sum(pSexM, na.rm = TRUE)
pSexF_sum <- sum(pSexF, na.rm = TRUE)

pSexM_sum/4000
pSexF_sum/4000

#################################
# calculate attack rate posterior probabilities for all cities
# (being lazy again, males = young and females = old)
###################

pZm <- chains_m$pZ
pZf <- chains_f$pZ

males_higher <- matrix(NA, nrow(pZm), ncol(pZm))
females_higher <- matrix(NA, nrow(pZm), ncol(pZm))

for (j in 1:ncol(pZm)) {
  for (i in 1:nrow(pZm)) {
    
    if (pZm[i,j] > pZf[i,j]) {
      males_higher[i,j] <- 1
    } 
    if (pZm[i,j] < pZf[i,j]) {
      females_higher[i,j] <- 1
    } 
  }
}

males_higher_sum <- colSums(males_higher, na.rm = TRUE)
females_higher_sum <- colSums(females_higher, na.rm = TRUE)


posterior_prob_males <- vector()
for (k in 1:28) {
  posterior_prob_males[k] <- males_higher_sum[k]/nrow(pZm)
}

dat <- readRDS("data/capital_cities_sex_data.RDS")
dat <- dplyr::filter(dat, sex == "M")

names(posterior_prob_males) <- dat$admin2_name
round(posterior_prob_males[sort(names(posterior_prob_males))], digits = 2)

posterior_prob_females <- vector()
for (kk in 1:28) {
  posterior_prob_females[kk] <- females_higher_sum[kk]/nrow(pZm)
}

names(posterior_prob_females) <- dat$admin2_name
round(posterior_prob_females[sort(names(posterior_prob_females))], digits = 2)

######
# calculate reporting rate posterior probability values for all cities
pSm <- chains_m$pS
pSf <- chains_f$pS

males_higher <- matrix(NA, nrow(pSm), ncol(pSm))
females_higher <- matrix(NA, nrow(pSm), ncol(pSm))

for (j in 1:ncol(pSm)) {
  for (i in 1:nrow(pSm)) {
    
    if (pSm[i,j] > pSf[i,j]) {
      males_higher[i,j] <- 1
    } 
    if (pSm[i,j] < pSf[i,j]) {
      females_higher[i,j] <- 1
    } 
  }
}

males_higher_sum <- colSums(males_higher, na.rm = TRUE)
females_higher_sum <- colSums(females_higher, na.rm = TRUE)


posterior_prob_males <- vector()
for (k in 1:28) {
  posterior_prob_males[k] <- males_higher_sum[k]/nrow(pSm)
}

names(posterior_prob_males) <- dat$admin2_name
round(posterior_prob_males[sort(names(posterior_prob_males))], digits = 2)

posterior_prob_females <- vector()
for (kk in 1:28) {
  posterior_prob_females[kk] <- females_higher_sum[kk]/nrow(pSm)
}

names(posterior_prob_females) <- dat$admin2_name
round(posterior_prob_females[sort(names(posterior_prob_females))], digits = 2)


#######################
# calculate NC rates posterior probabilities for all cities
pNCm <- chains_m$pNC
pNCf <- chains_f$pNC

males_higher <- matrix(NA, nrow(pNCm), ncol(pNCm))
females_higher <- matrix(NA, nrow(pNCm), ncol(pNCm))

for (j in 1:ncol(pNCm)) {
  for (i in 1:nrow(pNCm)) {
    
    if (pNCm[i,j] > pNCf[i,j]) {
      males_higher[i,j] <- 1
    } 
    if (pNCm[i,j] < pNCf[i,j]) {
      females_higher[i,j] <- 1
    } 
  }
}

males_higher_sum <- colSums(males_higher, na.rm = TRUE)
females_higher_sum <- colSums(females_higher, na.rm = TRUE)

posterior_prob_males <- vector()
for (k in 1:28) {
  posterior_prob_males[k] <- males_higher_sum[k]/nrow(pNCm)
}

names(posterior_prob_males) <- dat$admin2_name
round(posterior_prob_males[sort(names(posterior_prob_males))], digits = 2)

posterior_prob_females <- vector()
for (kk in 1:28) {
  posterior_prob_females[kk] <- females_higher_sum[kk]/nrow(pNCm)
}

names(posterior_prob_females) <- dat$admin2_name
round(posterior_prob_females[sort(names(posterior_prob_females))], digits = 2)


################# plot posterior densities by city

library(reshape2)
library(ggplot2)
library(dplyr)

# comparing infection attack rates by department
pZm <- chains_m$pZ
pZf <- chains_f$pZ

# add city names to chains
dat <- readRDS("data/capital_cities_sex_data.RDS")
dat <- dplyr::filter(dat, sex == "M")

colnames(pZm) <- dat$admin2_name
colnames(pZf) <- dat$admin2_name

# wide to long
library(reshape2)
pZm_long <- melt(pZm)
pZf_long <- melt(pZf)

pZm_long$age <- "0-39"
pZf_long$age <- "40 or more"

long_dat <- rbind(pZm_long, pZf_long)

library(ggplot2)

# density plots faceted by department
# With transparency (right)
p1 <- ggplot(data = long_dat, aes(x = value, group = age, 
                                  fill = age, color = age)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic() +
  facet_wrap(~Var2, ncol = 4, scales = "free") +
  scale_fill_manual(values=c("#F02D3A", "#2E3138")) +
  scale_color_manual(values=c("#F02D3A", "#2E3138")) +
  theme(legend.title = element_blank(),
        legend.position = "top")+
  ylab("Density") +
  xlab("Infection attack rate")


p1

# save as png, 700 x 900
##############################

# comparing reporting rates by department
pSm <- chains_m$pS
pSf <- chains_f$pS

# add city names to chains
dat <- readRDS("data/capital_cities_sex_data.RDS")
dat <- dplyr::filter(dat, sex == "M")

colnames(pSm) <- dat$admin2_name
colnames(pSf) <- dat$admin2_name

# wide to long
library(reshape2)
pSm_long <- melt(pSm)
pSf_long <- melt(pSf)

pSm_long$age <- "Male"
pSf_long$age <- "Female"

long_dat <- rbind(pSm_long, pSf_long)

# density plots faceted by department
# With transparency (right)

p2 <- ggplot(data = long_dat, aes(x = value, group = age, 
                                  fill = age, color = age)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic() +
  facet_wrap(~Var2, ncol = 4, scales = "free") +
  scale_fill_manual(values=c("#F02D3A", "#2E3138")) +
  scale_color_manual(values=c("#F02D3A", "#2E3138")) +
  theme(legend.title = element_blank(),
        legend.position = "top")+
  ylab("Density") +
  xlab("Reporting rate")

p2

# save as png, 700 x 900
#########################

# comparing NC cases by department
pNCm <- chains_m$pNC
pNCf <- chains_f$pNC

# add city names to chains
dat <- readRDS("data/capital_cities_sex_data.RDS")
dat <- dplyr::filter(dat, sex == "M")

colnames(pNCm) <- dat$admin2_name
colnames(pNCf) <- dat$admin2_name

# wide to long
library(reshape2)
pNCm_long <- melt(pNCm)
pNCf_long <- melt(pNCf)

pNCm_long$age <- "Male"
pNCf_long$age <- "Female"

long_dat <- rbind(pNCm_long, pNCf_long)

# per 10,000 infections
long_dat$value <- long_dat$value*10000

# density plots faceted by city
# With transparency (right)
p3 <- ggplot(data = long_dat, 
             aes(x = value, group = age, fill = age, color = age)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic() +
  facet_wrap(~Var2, ncol = 4, scales = "free") +
  scale_fill_manual(values=c("#F02D3A", "#2E3138")) +
  scale_color_manual(values=c("#F02D3A", "#2E3138")) +
  theme(legend.title = element_blank(),
        legend.position = "top")+
  ylab("Density") +
  xlab("NC cases reported per 10,000 ZIKV infections") 

p3

# save as png, 700 x 900

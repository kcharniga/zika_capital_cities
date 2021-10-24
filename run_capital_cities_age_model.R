# This code runs the age model

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

# separate data by age
y_dat <- dplyr::filter(dat, agecat == "0-39")
o_dat <- dplyr::filter(dat, agecat == "40 or more")

priors_list <- readRDS("data/priors_list.RDS")
alphaZ <- priors_list[[1]]
gammaZ <- priors_list[[2]]

more_alphas <- rep(1, 24) 
more_gammas <- rep(1, 24) 

alphaZ <- c(alphaZ, more_alphas)
gammaZ <- c(gammaZ, more_gammas)

stan_data <- list(
  l    = nrow(y_dat),
  alphaZ = alphaZ,
  gammaZ = gammaZ,
  
  Nold = o_dat$pop,
  Sold = o_dat$zika_cases,
  NCold = o_dat$neur_cases,
  
  Nyoung = y_dat$pop,
  Syoung = y_dat$zika_cases,
  NCyoung = y_dat$neur_cases,
  
  Sold_all = sum(o_dat$zika_cases),
  NCold_all = sum(o_dat$neur_cases),
  Nold_all = sum(o_dat$pop),
  
  Syoung_all = sum(y_dat$zika_cases),
  NCyoung_all = sum(y_dat$neur_cases),
  Nyoung_all = sum(y_dat$pop))

# 153 params in this model
model_stan <- rstan::stanc("Stan_code/age_model.stan")
sm = rstan::stan_model(stanc_ret = model_stan, verbose=FALSE)

#  Run the Model
set.seed(123)
n_iter <- 2000
system.time(fit <- rstan::sampling(sm, 
                                   data=stan_data,
                                   iter = n_iter,
                                   control = list(max_treedepth = 15,
                                                  adapt_delta = 0.95))) 

# default for delta is 0.8. Increasing it should help with divergent transitions

print(fit, probs = c(0.025, 0.975), digits_summary = 8)

# extract samples from fit (permuted = TRUE merges all the MCMC chains together)
chains <- rstan::extract(fit, permuted = TRUE, inc_warmup = FALSE)

saveRDS(chains, "MCMC_chains_old_and_young_fitted_together.RDS")

for_plotting <- rstan::summary(fit, probs = c(0.025, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]

for_plotting_df <- as.data.frame(for_plotting)
library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

# young
NC_young_df <- df[c(37:64),]
S_young_df <- df[c(9:36),]

# old
NC_old_df <- df[c(93:120),]
S_old_df <- df[c(65:92),]

o_dat2 <- select(o_dat, admin2_name, agecat) # admin names
y_dat2 <- select(y_dat, admin2_name, agecat) # admin names

NC_young_df2 <- df[c(122),] # overall young
S_young_df2 <- df[c(121),]

NC_old_df2 <- df[c(124),] # overall old
S_old_df2 <- df[c(123),]

add_this_old <- data.frame(admin2_name = "Overall", agecat = "40 or more")
add_this_young <- data.frame(admin2_name = "Overall", agecat = "0-39")

NC_old_df3 <- cbind(NC_old_df2, add_this_old) #olds
S_old_df3 <- cbind(S_old_df2, add_this_old)

NC_young_df3 <- cbind(NC_young_df2, add_this_young) #youngs
S_young_df3 <- cbind(S_young_df2, add_this_young)

NC_old_df <- cbind(NC_old_df, o_dat2) # olds
S_old_df <- cbind(S_old_df, o_dat2)

NC_young_df <- cbind(NC_young_df, y_dat2) # youngs
S_young_df <- cbind(S_young_df, y_dat2)

NC_old_df <- rbind(NC_old_df, NC_old_df3) # olds
S_old_df <- rbind(S_old_df, S_old_df3)

NC_young_df <- rbind(NC_young_df, NC_young_df3) # youngs
S_young_df <- rbind(S_young_df, S_young_df3)

saveRDS(NC_old_df, "NC_df_olds.RDS")
saveRDS(S_old_df, "S_df_olds.RDS")

saveRDS(NC_young_df, "NC_df_youngs.RDS")
saveRDS(S_young_df, "S_df_youngs.RDS")

# prepare data for Z
Z_df <- df[c(125:152),]

dat2 <- select(y_dat, admin2_name) # admin names

Z_df2 <- df[c(153),]

add_this <- data.frame(admin2_name = "Overall")

Z_df3 <- cbind(Z_df2, add_this)

Z_df <- cbind(Z_df, dat2)

Z_df <- rbind(Z_df, Z_df3)

saveRDS(Z_df, "Z_df.RDS")

# save hyperpriors too
hyper_df <- df[c(1:8),]
saveRDS(hyper_df, "hyper_df.RDS")

#############
# Re-start R
############

#### Plot

NC_df_youngs <- readRDS("NC_df_youngs.RDS")
NC_df_youngs <- with(NC_df_youngs,  NC_df_youngs[order(admin2_name) , ])
NC_df_youngs$num <- 1:29
NC_df_youngs$num[NC_df_youngs$admin2_name == "Overall"] <- 100

S_df_youngs <- readRDS("S_df_youngs.RDS")
S_df_youngs <- with(S_df_youngs,  S_df_youngs[order(admin2_name) , ])
S_df_youngs$num <- 1:29
S_df_youngs$num[S_df_youngs$admin2_name == "Overall"] <- 100

NC_df_olds <- readRDS("NC_df_olds.RDS")
NC_df_olds <- with(NC_df_olds,  NC_df_olds[order(admin2_name) , ])
NC_df_olds$num <- 1:29
NC_df_olds$num[NC_df_olds$admin2_name == "Overall"] <- 100

S_df_olds <- readRDS("S_df_olds.RDS")
S_df_olds <- with(S_df_olds,  S_df_olds[order(admin2_name) , ])
S_df_olds$num <- 1:29
S_df_olds$num[S_df_olds$admin2_name == "Overall"] <- 100


Z_df <- readRDS("Z_df.RDS")
Z_df <- with(Z_df,  Z_df[order(admin2_name) , ])
Z_df$num <- 1:29
Z_df$num[Z_df$admin2_name == "Overall"] <- 100

NC_both <- rbind(NC_df_youngs, NC_df_olds)

S_both <- rbind(S_df_youngs, S_df_olds)

NC_both$admin2_name[NC_both$admin2_name == "San José del Guaviare"] <- "San José"
S_both$admin2_name[S_both$admin2_name == "San José del Guaviare"] <- "San José"
Z_df$admin2_name[Z_df$admin2_name == "San José del Guaviare"] <- "San José"

## add some fake data to get the legends right
Z_df$agecat <- "All ages"

extra_row <- NC_both[1,]
extra_row$agecat <- "All ages"
extra_row$mean <- NA
extra_row$LL <- NA
extra_row$UL <- NA
extra_row$type <- NA

NC_both <- rbind(NC_both, extra_row)
S_both <- rbind(S_both, extra_row)

extra_row2 <- extra_row
extra_row$agecat <- "0-39"
extra_row2$agecat <- "40 or more"

extra <- rbind(extra_row, extra_row2)

Z_df <- rbind(extra, Z_df)

library(ggplot2)

p1 <- ggplot(NC_both, aes(x=reorder(admin2_name, num), y=mean*10000, color=agecat)) + 
  geom_errorbar(aes(ymin=LL*10000, ymax=UL*10000), width=.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values=c("#F02D3A", "#2E3138","#B3B6B7")) +
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

p2 <- ggplot(S_both, aes(x=reorder(admin2_name, num), y=mean, color=agecat)) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values=c("#F02D3A", "#2E3138","#B3B6B7")) +
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
        axis.ticks.x=element_blank()) 

p3 <- ggplot(Z_df, aes(x=reorder(admin2_name, num), y=mean, color = agecat)) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2, 
                position = position_dodge(width = 0.3)) +
  scale_color_manual(values=c("#F02D3A", "#2E3138","#B3B6B7")) +
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

############################## Re-start R
# Calculate posterior probabilities
# here, older age groups are "m" and younger age groups are "f"

chains <- readRDS("MCMC_chains_old_and_young_fitted_together.RDS")

# comparing reporting rates
pSall_m <- chains$pSold_all
pSall_f <- chains$pSyoung_all

pagecatM <- rep(NA, 4000)
pagecatF <- rep(NA, 4000)

for (i in 1:length(pSall_m)){
  if(pSall_m[i] > pSall_f[i]){
    pagecatM[i] <- 1
  } 
  if(pSall_m[i] < pSall_f[i]){
    pagecatF[i] <- 1
  } 
}

pagecatM_sum <- sum(pagecatM, na.rm = TRUE)
pagecatF_sum <- sum(pagecatF, na.rm = TRUE)

pagecatM_sum/4000
pagecatF_sum/4000

#####

# comparing NC rates
pNCall_m <- chains$pNCold_all
pNCall_f <- chains$pNCyoung_all

pagecatM <- rep(NA, 4000)
pagecatF <- rep(NA, 4000)

for (i in 1:length(pNCall_m)){
  if(pNCall_m[i] > pNCall_f[i]){
    pagecatM[i] <- 1
  } 
  if(pNCall_m[i] < pNCall_f[i]){
    pagecatF[i] <- 1
  } 
}

pagecatM_sum <- sum(pagecatM, na.rm = TRUE)
pagecatF_sum <- sum(pagecatF, na.rm = TRUE)

pagecatM_sum/4000
pagecatF_sum/4000

######
# calculate reporting rate posterior probability values for all cities
dat <- readRDS("data/capital_cities_age_data.RDS")
dat <- dplyr::filter(dat, agecat == "0-39")

pSm <- chains$pSold
pSf <- chains$pSyoung

olds_higher <- matrix(NA, nrow(pSm), ncol(pSm))
youngs_higher <- matrix(NA, nrow(pSm), ncol(pSm))

for (j in 1:ncol(pSm)) {
  for (i in 1:nrow(pSm)) {
    
    if (pSm[i,j] > pSf[i,j]) {
      olds_higher[i,j] <- 1
    } 
    if (pSm[i,j] < pSf[i,j]) {
      youngs_higher[i,j] <- 1
    } 
  }
}

olds_higher_sum <- colSums(olds_higher, na.rm = TRUE)
youngs_higher_sum <- colSums(youngs_higher, na.rm = TRUE)

posterior_prob_olds <- vector()
for (k in 1:28) {
  posterior_prob_olds[k] <- olds_higher_sum[k]/nrow(pSm)
}

names(posterior_prob_olds) <- dat$admin2_name
round(posterior_prob_olds[sort(names(posterior_prob_olds))], digits = 2)

posterior_prob_youngs <- vector()
for (kk in 1:28) {
  posterior_prob_youngs[kk] <- youngs_higher_sum[kk]/nrow(pSm)
}

names(posterior_prob_youngs) <- dat$admin2_name
round(posterior_prob_youngs[sort(names(posterior_prob_youngs))], digits = 2)

#######################
# calculate NC rates posterior probabilities for all cities
pNCm <- chains$pNCold
pNCf <- chains$pNCyoung

olds_higher <- matrix(NA, nrow(pNCm), ncol(pNCm))
youngs_higher <- matrix(NA, nrow(pNCm), ncol(pNCm))

for (j in 1:ncol(pNCm)) {
  for (i in 1:nrow(pNCm)) {
    
    if (pNCm[i,j] > pNCf[i,j]) {
      olds_higher[i,j] <- 1
    } 
    if (pNCm[i,j] < pNCf[i,j]) {
      youngs_higher[i,j] <- 1
    } 
  }
}

olds_higher_sum <- colSums(olds_higher, na.rm = TRUE)
youngs_higher_sum <- colSums(youngs_higher, na.rm = TRUE)

posterior_prob_olds <- vector()
for (k in 1:28) {
  posterior_prob_olds[k] <- olds_higher_sum[k]/nrow(pNCm)
}

names(posterior_prob_olds) <- dat$admin2_name
round(posterior_prob_olds[sort(names(posterior_prob_olds))], digits = 2)

posterior_prob_youngs <- vector()
for (kk in 1:28) {
  posterior_prob_youngs[kk] <- youngs_higher_sum[kk]/nrow(pNCm)
}

names(posterior_prob_youngs) <- dat$admin2_name
round(posterior_prob_youngs[sort(names(posterior_prob_youngs))], digits = 2)

################# plot posterior densities by city

library(reshape2)
library(ggplot2)
library(dplyr)

# comparing reporting rates by city
pSyoung <- chains$pSyoung
pSold <- chains$pSold

# add city names to chains
dat <- readRDS("capital_cities_age_data.RDS")
dat <- dplyr::filter(dat, agecat == "0-39")

colnames(pSyoung) <- dat$admin2_name
colnames(pSold) <- dat$admin2_name

# wide to long
library(reshape2)
pSyoung_long <- melt(pSyoung)
pSold_long <- melt(pSold)

pSyoung_long$agecat <- "0-39"
pSold_long$agecat <- "40 or more"

long_dat <- rbind(pSyoung_long, pSold_long)

# density plots faceted by city
# With transparency (right)
p2 <- ggplot(data = long_dat, aes(x = value, group = agecat, 
                                  fill = agecat, color = agecat)) +
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

# comparing NC cases by city
pNCyoung <- chains$pNCyoung
pNCold <- chains$pNCold

# add city names to chains
dat <- readRDS("data/capital_cities_age_data.RDS")
dat <- dplyr::filter(dat, agecat == "0-39")

colnames(pNCyoung) <- dat$admin2_name
colnames(pNCold) <- dat$admin2_name

# wide to long
library(reshape2)
pNCyoung_long <- melt(pNCyoung)
pNCold_long <- melt(pNCold)

pNCyoung_long$agecat <- "0-39"
pNCold_long$agecat <- "40 or more"

long_dat <- rbind(pNCyoung_long, pNCold_long)

# per 10,000 infections
long_dat$value <- long_dat$value*10000

# density plots faceted by city
# With transparency (right)
p3 <- ggplot(data = long_dat, 
             aes(x = value, group = agecat, fill = agecat, color = agecat)) +
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

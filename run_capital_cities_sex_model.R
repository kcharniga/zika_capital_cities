# This code runs the sex model

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
zika <- readRDS("data/capital_cities_sex_data.RDS")
dat <- zika

# separate data by sex
f_dat <- dplyr::filter(dat, sex == "F")
m_dat <- dplyr::filter(dat, sex == "M")

# Prior distributions for cities with seroprevalence data
priors_list <- readRDS("data/priors_list.RDS")
alphaZ <- priors_list[[1]]
gammaZ <- priors_list[[2]]

# Prior distributions for cities without seroprevalence data
more_alphas <- rep(1, 24) 
more_gammas <- rep(1, 24) 

# Combine prior distributions
alphaZ <- c(alphaZ, more_alphas)
gammaZ <- c(gammaZ, more_gammas)

stan_data <- list(
  l    = nrow(f_dat),
  alphaZ = alphaZ,
  gammaZ = gammaZ,
  
  Nmale = m_dat$pop,
  Smale = m_dat$zika_cases,
  NCmale = m_dat$neur_cases,
  
  Nfemale = f_dat$pop,
  Sfemale = f_dat$zika_cases,
  NCfemale = f_dat$neur_cases,
  
  Smale_all = sum(m_dat$zika_cases),
  NCmale_all = sum(m_dat$neur_cases),
  Nmale_all = sum(m_dat$pop),
  
  Sfemale_all = sum(f_dat$zika_cases),
  NCfemale_all = sum(f_dat$neur_cases),
  Nfemale_all = sum(f_dat$pop))

# 153 params in this model
model_stan <- rstan::stanc("Stan_code/sex_model.stan")
sm = rstan::stan_model(stanc_ret = model_stan, verbose=FALSE)

#  Run the Model
set.seed(123)
n_iter <- 2000
system.time(fit <- rstan::sampling(sm, 
                                   data=stan_data,
                                   iter = n_iter,
                                   control = list(max_treedepth = 15)))

print(fit, probs = c(0.025, 0.975), digits_summary = 8)

# Extract samples from fit (permuted = TRUE merges all the MCMC chains together)
chains <- rstan::extract(fit, permuted = TRUE, inc_warmup = FALSE)

saveRDS(chains, "MCMC_chains_male_and_female_fitted_together.RDS")

for_plotting <- rstan::summary(fit, probs = c(0.025, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]

for_plotting_df <- as.data.frame(for_plotting)
library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

# males
NC_male_df <- df[c(37:64),]
S_male_df <- df[c(9:36),]

# females
NC_female_df <- df[c(93:120),]
S_female_df <- df[c(65:92),]

m_dat2 <- select(m_dat, admin2_name, sex) # admin names
f_dat2 <- select(f_dat, admin2_name, sex) # admin names

NC_male_df2 <- df[c(122),] # overall males
S_male_df2 <- df[c(121),]

NC_female_df2 <- df[c(124),] # overall females
S_female_df2 <- df[c(123),]

add_this_male <- data.frame(admin2_name = "Overall", sex = "M")
add_this_female <- data.frame(admin2_name = "Overall", sex = "F")

NC_male_df3 <- cbind(NC_male_df2, add_this_male) #males
S_male_df3 <- cbind(S_male_df2, add_this_male)

NC_female_df3 <- cbind(NC_female_df2, add_this_female) #females
S_female_df3 <- cbind(S_female_df2, add_this_female)

NC_male_df <- cbind(NC_male_df, m_dat2) # males
S_male_df <- cbind(S_male_df, m_dat2)

NC_female_df <- cbind(NC_female_df, f_dat2) # females
S_female_df <- cbind(S_female_df, f_dat2)

NC_male_df <- rbind(NC_male_df, NC_male_df3) # males
S_male_df <- rbind(S_male_df, S_male_df3)

NC_female_df <- rbind(NC_female_df, NC_female_df3) # females
S_female_df <- rbind(S_female_df, S_female_df3)

saveRDS(NC_male_df, "NC_df_males.RDS")
saveRDS(S_male_df, "S_df_males.RDS")

saveRDS(NC_female_df, "NC_df_females.RDS")
saveRDS(S_female_df, "S_df_females.RDS")

# prepare data for Z
Z_df <- df[c(125:152),]

dat2 <- select(f_dat, admin2_name) # admin names

Z_df2 <- df[c(153),]

add_this <- data.frame(admin2_name = "Overall")

Z_df3 <- cbind(Z_df2, add_this)

Z_df <- cbind(Z_df, dat2)

Z_df <- rbind(Z_df, Z_df3)

saveRDS(Z_df, "Z_df.RDS")

# save hyperpriors too
hyper_df <- df[c(1:8),]
saveRDS(hyper_df, "hyper_df.RDS")

#### Re-start R

#### Plot
NC_df_females <- readRDS("NC_df_females.RDS")
NC_df_females <- with(NC_df_females,  NC_df_females[order(admin2_name) , ])
NC_df_females$num <- 1:29
NC_df_females$num[NC_df_females$admin2_name == "Overall"] <- 100

S_df_females <- readRDS("S_df_females.RDS")
S_df_females <- with(S_df_females,  S_df_females[order(admin2_name) , ])
S_df_females$num <- 1:29
S_df_females$num[S_df_females$admin2_name == "Overall"] <- 100

NC_df_males <- readRDS("NC_df_males.RDS")
NC_df_males <- with(NC_df_males,  NC_df_males[order(admin2_name) , ])
NC_df_males$num <- 1:29
NC_df_males$num[NC_df_males$admin2_name == "Overall"] <- 100

S_df_males <- readRDS("S_df_males.RDS")
S_df_males <- with(S_df_males,  S_df_males[order(admin2_name) , ])
S_df_males$num <- 1:29
S_df_males$num[S_df_males$admin2_name == "Overall"] <- 100

Z_df <- readRDS("Z_df.RDS")
Z_df <- with(Z_df,  Z_df[order(admin2_name) , ])
Z_df$num <- 1:29
Z_df$num[Z_df$admin2_name == "Overall"] <- 100

NC_both <- rbind(NC_df_females, NC_df_males)
NC_both$sex[NC_both$sex == "F"] <- "Female"
NC_both$sex[NC_both$sex == "M"] <- "Male"

S_both <- rbind(S_df_females, S_df_males)
S_both$sex[S_both$sex == "F"] <- "Female"
S_both$sex[S_both$sex == "M"] <- "Male"

# Shorten so plot looks better
NC_both$admin2_name[NC_both$admin2_name == "San José del Guaviare"] <- "San José"
S_both$admin2_name[S_both$admin2_name == "San José del Guaviare"] <- "San José"
Z_df$admin2_name[Z_df$admin2_name == "San José del Guaviare"] <- "San José"

## add some "fake" data to get the legends to look right
Z_df$sex <- "Male and female"

extra_row <- NC_both[1,]
extra_row$sex <- "Male and female"
extra_row$mean <- NA
extra_row$LL <- NA
extra_row$UL <- NA
extra_row$type <- NA

NC_both <- rbind(NC_both, extra_row)
S_both <- rbind(S_both, extra_row)

extra_row2 <- extra_row
extra_row$sex <- "Female"
extra_row2$sex <- "Male"

extra <- rbind(extra_row, extra_row2)

Z_df <- rbind(extra, Z_df)

library(ggplot2)

p1 <- ggplot(NC_both, aes(x=reorder(admin2_name, num), y=mean*10000, color=sex)) + 
  geom_errorbar(aes(ymin=LL*10000, ymax=UL*10000), width=.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values=c("#0FA3B1", "#FF9B42","#B91372")) +
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

p2 <- ggplot(S_both, aes(x=reorder(admin2_name, num), y=mean, color=sex)) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values=c("#0FA3B1", "#FF9B42", "#B91372")) +
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

p3 <- ggplot(Z_df, aes(x=reorder(admin2_name, num), y=mean, color = sex)) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2, 
                position = position_dodge(width = 0.3)) +
  scale_color_manual(values=c("#0FA3B1", "#FF9B42","#B91372")) +
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

chains <- readRDS("MCMC_chains_male_and_female_fitted_together.RDS")

# comparing reporting rates
pSall_m <- chains$pSmale_all
pSall_f <- chains$pSfemale_all

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
pNCall_m <- chains$pNCmale_all
pNCall_f <- chains$pNCfemale_all

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


######
# calculate reporting rate posterior probability values for all cities
dat <- readRDS("data/capital_cities_sex_data.RDS")
dat <- dplyr::filter(dat, sex == "M")

pSm <- chains$pSmale
pSf <- chains$pSfemale

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
pNCm <- chains$pNCmale
pNCf <- chains$pNCfemale

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

# comparing reporting rates by city
pSm <- chains$pSmale
pSf <- chains$pSfemale

# add city names to chains
dat <- readRDS("data/capital_cities_sex_data.RDS")
dat <- dplyr::filter(dat, sex == "M")

colnames(pSm) <- dat$admin2_name
colnames(pSf) <- dat$admin2_name

# wide to long
library(reshape2)
pSm_long <- melt(pSm)
pSf_long <- melt(pSf)

pSm_long$sex <- "Male"
pSf_long$sex <- "Female"

long_dat <- rbind(pSm_long, pSf_long)

# density plots faceted by city
# With transparency (right)

p2 <- ggplot(data = long_dat, aes(x = value, group = sex, 
                                  fill = sex, color = sex)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic() +
  facet_wrap(~Var2, ncol = 4, scales = "free") +
  scale_fill_manual(values=c("#0FA3B1", "#FF9B42")) +
  scale_color_manual(values=c("#0FA3B1", "#FF9B42")) +
  theme(legend.title = element_blank(),
        legend.position = "top")+
  ylab("Density") +
  xlab("Reporting rate")


p2

# save as png, 700 x 900
#########################

# comparing NC cases by city
pNCm <- chains$pNCmale
pNCf <- chains$pNCfemale

# add city names to chains
dat <- readRDS("data/capital_cities_sex_data.RDS")
dat <- dplyr::filter(dat, sex == "M")

colnames(pNCm) <- dat$admin2_name
colnames(pNCf) <- dat$admin2_name

# wide to long
library(reshape2)
pNCm_long <- melt(pNCm)
pNCf_long <- melt(pNCf)

pNCm_long$sex <- "Male"
pNCf_long$sex <- "Female"

long_dat <- rbind(pNCm_long, pNCf_long)

# per 10,000 infections
long_dat$value <- long_dat$value*10000

# density plots faceted by city
# With transparency (right)
p3 <- ggplot(data = long_dat, 
             aes(x = value, group = sex, fill = sex, color = sex)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic() +
  facet_wrap(~Var2, ncol = 4, scales = "free") +
  scale_fill_manual(values=c("#0FA3B1", "#FF9B42")) +
  scale_color_manual(values=c("#0FA3B1", "#FF9B42")) +
  theme(legend.title = element_blank(),
        legend.position = "top")+
  ylab("Density") +
  xlab("NC cases reported per 10,000 ZIKV infections") 

p3

# save as png, 700 x 900

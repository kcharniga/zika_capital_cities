# Capital cities model
# This code runs the main model, prints results, plots MCMC diagnostics, and produces Figure 4
# of the main text

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

# Set up RStan to run on multiple processors
options(mc.cores = parallel::detectCores())

# Epi data
zika <- readRDS("data/capital_cities_data.RDS")
dat <- zika

# Prior distributions for 4 cities with seroprevalence data
priors_list <- readRDS("data/priors_list.RDS")
alphaZ <- priors_list[[1]]
gammaZ <- priors_list[[2]]

# Additional prior distributions for cities without seroprevalence data
more_alphas <- rep(1, 24) # These correspond to Beta(1,1), or uniform between 0 and 1
more_gammas <- rep(1, 24) 
#more_alphas <- rep(2, 24) # These correspond to Beta(2,2), which we tested in sensitivity analysis
#more_gammas <- rep(2, 24) 

# Combine the priors into one vector
alphaZ <- c(alphaZ, more_alphas)
gammaZ <- c(gammaZ, more_gammas)

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

# Plots of diagnostics

###### MCMC traces

# For pretty labels
scaleFUN <- function(x) {sprintf("%.2f", x)}

#pZ
fitArray <- as.array(fit)[,,5:32]
dim(fitArray)
dimnames(fitArray)[[3]] <- dat$admin2_name
color_scheme_get()
color_scheme_set("purple")
d1 <- mcmc_trace(fitArray, facet_args = list(nrow = 8))
d1 + xlab("Iterations") + 
  ggtitle("Infection attack rate") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.spacing.y = unit(0.5, "lines")) +
  ylim(0,1) +
  scale_x_continuous( breaks = c(0, 300, 600, 900),
                      labels = c(0, 300, 600, 900))
# save as pdf 7.5x10

#pS
fitArray <- as.array(fit)[,,33:60]
dim(fitArray)
dimnames(fitArray)[[3]] <- dat$admin2_name
color_scheme_get()
color_scheme_set("green")
d1 <- mcmc_trace(fitArray, facet_args = list(nrow = 8))
d1 + xlab("Iterations") + 
  ggtitle("Reporting rate") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.spacing.y = unit(0.5, "lines")) +
  scale_y_continuous(limits=c(0, 0.04), 
                     breaks = c(0.00, 0.01, 0.02, 0.03, 0.04),
                     labels = scaleFUN) +
  scale_x_continuous( breaks = c(0, 300, 600, 900),
                      labels = c(0, 300, 600, 900))
# save as 7.5 x 10

#pCZ
fitArray <- as.array(fit)[,,61:88]
dim(fitArray)
dimnames(fitArray)[[3]] <- dat$admin2_name
color_scheme_get()
color_scheme_set("pink")
d1 <- mcmc_trace(fitArray, facet_args = list(nrow = 8))
d1 + xlab("Iterations") + 
  ggtitle("NC cases reported per 10,000 ZIKV infections") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.spacing.y = unit(0.5, "lines")) +
  scale_y_continuous(limits=c(0, 1.5e-4), 
                     breaks = c(0, 5e-5, 1e-4, 1.5e-4),
                     labels = c(0.0, 0.5, 1.0, 1.5)) +
  scale_x_continuous( breaks = c(0, 300, 600, 900),
                      labels = c(0, 300, 600, 900))

# save as 7.7x10.5

###### Density plots
#pZ
fitArray <- as.array(fit)[,,5:32]
dim(fitArray)
dimnames(fitArray)[[3]] <- dat$admin2_name
color_scheme_get()
color_scheme_set("purple")
d1 <- mcmc_dens(fitArray, facet_args = list(nrow = 8))
d1 + xlab("Iterations") + 
  ggtitle("Infection attack rate") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.spacing.y = unit(0.5, "lines")) +
  xlim(0,1)
# save as pdf 7.5x10.5

#pS
fitArray <- as.array(fit)[,,33:60]
dim(fitArray)
dimnames(fitArray)[[3]] <- dat$admin2_name
color_scheme_get()
color_scheme_set("green")
d1 <- mcmc_dens(fitArray, facet_args = list(nrow = 8))
d1 + xlab("Iterations") + 
  ggtitle("Reporting rate") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.spacing.y = unit(0.5, "lines")) + 
  scale_x_continuous(limits=c(0, 0.03), 
                     breaks = c(0, 0.01, 0.02, 0.03),
                     labels = scaleFUN)

scaleFUN <- function(x) sprintf("%.2f", x)

#pCZ
fitArray <- as.array(fit)[,,61:88]
dim(fitArray)
dimnames(fitArray)[[3]] <- dat$admin2_name
color_scheme_get()
color_scheme_set("pink")
d1 <- mcmc_dens(fitArray, facet_args = list(nrow = 8))
d1 + xlab("Iterations") + 
  ggtitle("NC cases reported per 10,000 ZIKV infections") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.spacing.y = unit(0.5, "lines")) +
  scale_x_continuous(limits=c(0, 1.5e-4), 
                     breaks = c(0, 5e-5, 1e-4, 1.5e-4),
                     labels = c(0.0, 0.5, 1.0, 1.5))

# Summary statistics

# Acceptance rate
sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
mean_accept_stat <- sapply(sampler_params, function(x) mean(x[,"accept_stat__"]))
print(mean_accept_stat)

# Print model results
print(fit, probs = c(0.025, 0.975), digits_summary = 8)

############
# Figure 4
############

# Extract the results for plotting
for_plotting <- rstan::summary(fit, probs = c(0.025, 0.5, 0.975))$summary
for_plotting <- for_plotting[, c("mean", "2.5%", "97.5%")]
for_plotting_df <- as.data.frame(for_plotting)

library(dplyr)
df <- tibble::rownames_to_column(for_plotting_df, "type")
df <- rename(df, LL = "2.5%", UL = "97.5%")

NC_df <- df[c(61:88,90),]

dat2 <- data.frame(num = 1:28, admin_name = NA)
dat2$admin_name <- dat$admin2_name

overall <- data.frame(num = 29, admin_name = "Overall")

dat3 <- rbind(dat2, overall)

NC_df <- cbind(dat3, NC_df)
NC_df <- with(NC_df,  NC_df[order(admin_name) , ])
NC_df$num <- 1:29
NC_df$num[NC_df$admin_name == "Overall"] <- 100

library(ggplot2)
p1 <- ggplot(NC_df, aes(x=reorder(admin_name, num), y=mean*10000, colour=admin_name)) + 
  geom_errorbar(aes(ymin=LL*10000, ymax=UL*10000), width=.4) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, color = "black", vjust = 0.5, hjust = 1, size = 10)) +
  theme(axis.text.y = element_text(color = "black", size = 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank()) +
  labs(y = "NC cases reported per\n10,000 ZIKV infections") +
  theme(axis.title.y = element_text(color="black", size=12, face="bold"))

#######

S_df <- df[c(33:60,89),]
S_df <- cbind(dat3, S_df)
S_df <- with(S_df,  S_df[order(admin_name) , ])
S_df$num <- 1:29
S_df$num[S_df$admin_name == "Overall"] <- 100

p2 <- ggplot(S_df, aes(x=reorder(admin_name, num), y=mean, colour=admin_name)) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.4) +
  geom_point() +
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

#######

Z_df <- df[c(5:32,91),]
Z_df <- cbind(dat3, Z_df)
Z_df <- with(Z_df,  Z_df[order(admin_name) , ])
Z_df$num <- 1:29
Z_df$num[Z_df$admin_name == "Overall"] <- 100

p3 <- ggplot(Z_df, aes(x=reorder(admin_name, num), y=mean, colour=admin_name)) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.4) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, color = "black", vjust = 0.5, hjust = 1, size = 10)) +
  theme(axis.text.y = element_text(color = "black", size = 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank()) +
  labs(y = "Infection attack rate") +
  theme(axis.title.y = element_text(color="black", size=12, face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# put three plots together (Figure 4)
egg::ggarrange(p3, p2, p1)

# 8x6.25 pdf

# save these for sensitivity analysis
saveRDS(NC_df, "NC_df_none_removed.RDS")
saveRDS(S_df, "S_df_none_removed.RDS")
saveRDS(Z_df, "Z_df_none_removed.RDS")

####### Expected NC per reported ZVD

# extract samples from fit (permuted = TRUE merges all the MCMC chains together)
chains <- rstan::extract(fit, permuted = TRUE, inc_warmup = FALSE)

chains$pNew <- (chains$pNC/chains$pS)*10000

plot_mean <- vector()
plot_LL <- vector()
plot_UL <- vector()
for (kk in 1:28) {
  city <- chains$pNew[,kk]
  plot_mean[kk] <- mean(city)
  plot_LL[kk] <- quantile(city, probs = 0.025)
  plot_UL[kk] <- quantile(city, probs = 0.975)
}

all <- data.frame(plot_mean = plot_mean, plot_LL = plot_LL,
                  plot_UL = plot_UL)

dat <- dplyr::rename(dat, admin_name = admin2_name)
plott <- cbind(all, dat)

plott$point_est <- (plott$neur_cases/plott$zika_cases)*10000

# binomial confidence interval of point estimates
plott$id <- 1:28

LL_CI_point_est <- vector()
UL_CI_point_est <- vector()
for (j in 1:28){
  city <- plott[plott$id == j,]
  ci <- binom.test(x = city$neur_cases, n = city$zika_cases, p = 0.5)
  LL_CI_point_est[j] <- (ci$conf.int[1])*10000
  UL_CI_point_est[j] <- (ci$conf.int[2])*10000
}

CIS <- data.frame(UL_CI_point_est = UL_CI_point_est, 
                  LL_CI_point_est = LL_CI_point_est)

plot_estimates <- plott
plot_estimates <- dplyr::select(plot_estimates,
                                plot_mean,
                                plot_LL,
                                plot_UL, 
                                admin_name)

plot_estimates$type <- "Model estimates"

plott2 <- select(plott, admin_name, point_est)
plott2 <- cbind(CIS, plott2)

plot_point_est <- rename(plott2, 
                         plot_mean = point_est,
                         plot_LL = LL_CI_point_est,
                         plot_UL = UL_CI_point_est)

plot_point_est$type <- "Point estimates"

all <- rbind(plot_estimates, plot_point_est)

p4 <- ggplot(all, aes(x=admin_name, y=plot_mean, colour=type)) + 
  geom_errorbar(aes(ymin=plot_LL, ymax=plot_UL), width=.5,
                position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6)) +
  theme_bw() +
  scale_color_manual(values = c("#058C42", "#020202")) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, color = "black", vjust = 0.5, hjust = 1, size = 10)) +
  theme(axis.text.y = element_text(color = "black", size = 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank()) +
  scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500),
                     labels = c(0,500,1000,1500,2000,2500)) +
  labs(y = "NC cases reported per\n10,000 reported ZVD cases") +
  theme(axis.title.y = element_text(color="black", size=12, face = "bold"))

p4  

# places with point estimates outside CrI:
plott$outside <- NA
plott$outside[plott$point_est < plott$plot_LL|
                plott$point_est > plott$plot_UL] <- 1

# overall
pS_merge <- as.vector(chains$pS)
pCZ_merge <- as.vector(chains$pNC)

totaltotal <- (pCZ_merge/pS_merge)*10000

mean(totaltotal) # 54
quantile(totaltotal, probs = c(0.025,0.975)) # 5-210

# 6.25 x 4 pdf


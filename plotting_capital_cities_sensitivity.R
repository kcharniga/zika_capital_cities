# Plotting sensitivity of main model results to removing seroprevalence data
# from the four cities plus Barranquilla

library(dplyr)

# read in data
NC_none <- readRDS("NC_df_none_removed.RDS")
NC_sin <- readRDS("NC_df_SIN_removed.RDS")
NC_nei <- readRDS("NC_df_NEI_removed.RDS")
NC_cuc <- readRDS("NC_df_CUC_removed.RDS")
NC_med <- readRDS("NC_df_MED_removed.RDS")
NC_atl <- readRDS("NC_df_ATL_removed.RDS")

NC_none$data_removed <- "none"
NC_sin$data_removed <- "Sincelejo"
NC_nei$data_removed <- "Neiva"
NC_cuc$data_removed <- "Cúcuta"
NC_med$data_removed <- "Medellín"
NC_atl$data_removed <- "Barranquilla"

NC_none <- dplyr::filter(NC_none, !(admin_name == "Overall"))
NC_none <- dplyr::rename(NC_none, place= admin_name)

NC_none <- select(NC_none, -num)

NC_all <- rbind(NC_none, NC_sin, NC_nei, NC_cuc, NC_med, NC_atl)
NC_all <- na.omit(NC_all)

NC_all$place[NC_all$place == "San José del Guaviare"] <- "San José"

library(ggplot2)

scaleFUN <- function(x) sprintf("%.2f", x)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- ggstance::position_dodgev(height=0.8) # move them .05 to the left and right

p1 <- ggplot(NC_all, aes(x=mean*10000, y=forcats::fct_rev(place), colour=data_removed)) + 
  geom_errorbarh(aes(xmin=(LL)*10000, xmax=(UL)*10000), height=.9, position = pd) +
  geom_point(position = pd) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(color = "black", vjust = 0.5, size = 10)) +
  theme(axis.text.x = element_text(color = "black", size = 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank()) +
  labs(x = "NC cases reported per\n10,000 ZIKV infections") +
  theme(axis.title.x = element_text(color="black", size=12, face="bold")) +
  labs(color = "Data removed") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

###############

# read in data
S_none <- readRDS("S_df_none_removed.RDS")
S_sin <- readRDS("S_df_SIN_removed.RDS")
S_nei <- readRDS("S_df_NEI_removed.RDS")
S_cuc <- readRDS("S_df_CUC_removed.RDS")
S_med <- readRDS("S_df_MED_removed.RDS")
S_atl <- readRDS("S_df_ATL_removed.RDS")

S_none$data_removed <- "none"
S_sin$data_removed <- "Sincelejo"
S_nei$data_removed <- "Neiva"
S_cuc$data_removed <- "Cúcuta"
S_med$data_removed <- "Medellín"
S_atl$data_removed <- "Barranquilla"

S_none <- dplyr::filter(S_none, !(admin_name == "Overall"))
S_none <- dplyr::rename(S_none, place= admin_name)

S_none <- select(S_none, -num)

S_all <- rbind(S_none, S_sin, S_nei, S_cuc, S_med, S_atl)
S_all <- na.omit(S_all)

S_all$place[S_all$place == "San José del Guaviare"] <- "San José"

library(ggplot2)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- ggstance::position_dodgev(height=0.8) # move them .05 to the left and right

p2 <- ggplot(S_all, aes(x=mean, y=forcats::fct_rev(place), colour=data_removed)) + 
  geom_errorbarh(aes(xmin=(LL), xmax=(UL)), height=.9, position = pd) +
  geom_point(position = pd) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(axis.text.y = element_text(color = "black", vjust = 0.5, size = 10)) +
  theme(axis.text.x = element_text(color = "black", size = 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank()) +
  labs(x = "Reporting rate") +
  theme(axis.title.x = element_text(color="black", size=12, face="bold")) +
  labs(color = "Data removed") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_x_continuous(limits=c(0, 0.03), 
                     breaks = c(0, 0.01, 0.02, 0.03),
                     labels = scaleFUN)



###################################

# read in data
Z_none <- readRDS("Z_df_none_removed.RDS")
Z_sin <- readRDS("Z_df_SIN_removed.RDS")
Z_nei <- readRDS("Z_df_NEI_removed.RDS")
Z_cuc <- readRDS("Z_df_CUC_removed.RDS")
Z_med <- readRDS("Z_df_MED_removed.RDS")
Z_atl <- readRDS("Z_df_ATL_removed.RDS")

Z_none$data_removed <- "none"
Z_sin$data_removed <- "Sincelejo"
Z_nei$data_removed <- "Neiva"
Z_cuc$data_removed <- "Cúcuta"
Z_med$data_removed <- "Medellín"
Z_atl$data_removed <- "Barranquilla"

Z_none <- dplyr::filter(Z_none, !(admin_name == "Overall"))
Z_none <- dplyr::rename(Z_none, place= admin_name)

Z_none <- select(Z_none, -num)

Z_all <- rbind(Z_none, Z_sin, Z_nei, Z_cuc, Z_med, Z_atl)
Z_all <- na.omit(Z_all)

Z_all$place[Z_all$place == "San José del Guaviare"] <- "San José"

library(ggplot2)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- ggstance::position_dodgev(height=0.8) # move them .05 to the left and right

p3 <- ggplot(Z_all, aes(x=mean, y=forcats::fct_rev(place), color=data_removed)) + 
  geom_errorbarh(aes(xmin=(LL), xmax=(UL)), height=.9, position = pd) +
  geom_point(position = pd) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(color = "black", vjust = 0.5, size = 10)) +
  theme(axis.text.x = element_text(color = "black", size = 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank()) +
  labs(x = "Infection attack rate") +
  theme(axis.title.x = element_text(color="black", size=12, face="bold")) +
  labs(color = "Data removed") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01))

# Combine plots
egg::ggarrange(p3, p2, p1, nrow = 1)

# save as pdf 9.5x7 in


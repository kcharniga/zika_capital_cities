# Plotting sensitivity of model results when all serological data is removed

library(dplyr)

# read in data
NC_none <- readRDS("NC_df_none_removed.RDS")
NC_sero <- readRDS("NC_df_all_four_sero_removed.RDS")

NC_none$data_removed <- "None"
NC_sero$data_removed <- "All cities with serological data"

NC_none <- dplyr::filter(NC_none, !(admin_name == "Overall"))
NC_none <- dplyr::rename(NC_none, place= admin_name)

NC_none <- select(NC_none, -num)

NC_all <- rbind(NC_none, NC_sero)
#NC_all <- na.omit(NC_all)

NC_all$place[NC_all$place == "San José del Guaviare"] <- "San José"

library(ggplot2)

scaleFUN <- function(x) sprintf("%.2f", x)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- ggstance::position_dodgev(height=0.8) # move them .05 to the left and right

p1 <- ggplot(NC_all, aes(x=mean*10000, y=forcats::fct_rev(place), colour=data_removed)) + 
  geom_errorbarh(aes(xmin=(LL)*10000, xmax=(UL)*10000), height=.7, position = pd) +
  geom_point(position = pd) +
  theme_bw() +
  scale_color_manual(values = c("#7A28CB","#EC9F05")) +
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
S_sero <- readRDS("S_df_all_four_sero_removed.RDS")

S_none$data_removed <- "None"
S_sero$data_removed <- "All cities with serological data"

S_none <- dplyr::filter(S_none, !(admin_name == "Overall"))
S_none <- dplyr::rename(S_none, place= admin_name)

S_none <- select(S_none, -num)

S_all <- rbind(S_none, S_sero)
#S_all <- na.omit(S_all)

S_all$place[S_all$place == "San José del Guaviare"] <- "San José"

library(ggplot2)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- ggstance::position_dodgev(height=0.8) # move them .05 to the left and right

p2 <- ggplot(S_all, aes(x=mean, y=forcats::fct_rev(place), colour=data_removed)) + 
  geom_errorbarh(aes(xmin=(LL), xmax=(UL)), height=.7, position = pd) +
  geom_point(position = pd) +
  theme_bw() +
  scale_color_manual(values = c("#7A28CB","#EC9F05")) +
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
Z_sero <- readRDS("Z_df_all_four_sero_removed.RDS")

Z_none$data_removed <- "None"
Z_sero$data_removed <- "All cities with serological data"

Z_none <- dplyr::filter(Z_none, !(admin_name == "Overall"))
Z_none <- dplyr::rename(Z_none, place= admin_name)

Z_none <- select(Z_none, -num)

Z_all <- rbind(Z_none, Z_sero)
#Z_all <- na.omit(Z_all)

Z_all$place[Z_all$place == "San José del Guaviare"] <- "San José"

library(ggplot2)

# The errorbars overlapped, so use position_dodge to move them vertically
pd <- ggstance::position_dodgev(height=0.8) # move them up and down

p3 <- ggplot(Z_all, aes(x=mean, y=forcats::fct_rev(place), color=data_removed)) + 
  geom_errorbarh(aes(xmin=(LL), xmax=(UL)), height=.7, 
                 position = pd) +
  geom_point(position = pd) +
  theme_bw() +
  scale_color_manual(values = c("#7A28CB","#EC9F05")) +
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

p3

egg::ggarrange(p3, p2, p1, nrow = 1)

# pdf 9.5x7 in


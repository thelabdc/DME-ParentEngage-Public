


### Packages and imports
library('ggplot2')
library('dplyr')
library("clusterPower")
library(reshape2)

theme_new <- function(base_size = 16, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank(),   
      panel.border = element_rect(fill = NA, colour = "black", size=1),
      panel.background = element_rect(fill = "white", colour = "black"), 
      strip.background = element_rect(fill = NA),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
      )
}


### Schools 
schools_forsim = data.frame(school = c("A", "B", "C",
                       "D", "E"),
           n_approx = c(400, 1240, 202, 600, 600),
           n_clusters= c(6, 21, 4, 6, 6)) %>% #assuming for latter
          mutate(n_clusters_rounded = round(n_approx/70))

schools_forsim
n_treat_clusters = 20
n_control_clusters = 20
cv_clustersize = 0.24

### Function to calculate power for binary outcomes
prop_power <- function(icc, n_clusters = 40,
                       n_percluster = 70,
                       cv_clustersize = 0.24,
                       control_baserate = 0.49,
                       direction = FALSE){
  
    power = crtpwr.2prop(alpha = 0.05, power = 0.8, 
             m = n_clusters, 
             n= n_percluster,
             cv = cv_clustersize,
             p2 = control_baserate,
             icc = icc,
             p1inc = direction)
    return(power)    }


## Iterate over range of icc and calculate power
icc = seq(from = 0, to= 0.2, by = 0.05)
  
mde_ca = lapply(icc, prop_power)
mde_ca_df = data.frame(icc = icc,
                       tx_rate = unlist(mde_ca),
                       base_rate = 0.49) %>%
          mutate(mde = base_rate-tx_rate)

ggplot(mde_ca_df, aes(x = factor(icc), y = mde)) +
  geom_bar(stat = "identity", fill = "#264888",
           alpha = 1) +
  theme_new() +
  xlab("Intra-class correlation coefficient\nfor attendance between students with same teacher") +
  ylab("Minimal detectable effect for\nreduction in chronic absenteeism\nBase rate: 49%\nUnits: percentage points") +
  geom_label(aes(x = factor(icc), y = mde,
                 label = round(mde, 2)))

ggsave("teachertext_poweranalysis_CA.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

## function to calculate power for continuous outcomes
continuous_power <- function(icc, n_clusters = 40,
                       n_percluster = 70,
                       cv_clustersize = 0.24,
                       varw = 0.04){ # set based on variance in historical data; 
                                    # don't have teachers so just avg across schools
  
    power = crtpwr.2mean(alpha = 0.05, power = 0.8, 
             m = n_clusters, 
             n= n_percluster,
             cv = cv_clustersize,
             icc = icc, varw = varw)
    return(power)    }


## Iterate over range of icc and calculate power 
mde_isa = lapply(icc, continuous_power)
mde_isa_df = data.frame(icc = icc,
                       effect_size = unlist(mde_isa))

ggplot(mde_isa_df, aes(x = factor(icc), y = effect_size)) +
  geom_bar(stat = "identity", fill = "#264888",
           alpha = 1) +
  theme_new() +
  xlab("Intra-class correlation coefficient\nfor attendance between students with same teacher") +
  ylab("Minimal detectable effect for\nimprovement in ISA\n\nUnits: percentage points") +
  geom_label(aes(x = factor(icc), y = effect_size,
                 label = round(effect_size, 2)))


ggsave("teachertext_poweranalysis.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)




## Packages and imports
library('ggplot2')
library('dplyr')
library("clusterPower")
library(reshape2)
library(irr)
library(ICC)

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


## Read in analytic sample for RCT
studlevel_results <- fread('data/rct_datasets/dcps_pcs_analytic_sample_for_dataviz.csv') 

## get icc 
icc_fallsem = ICCest(cluster_id, is_ca, data = studlevel_results %>% filter(!is_spring_sem))
lower_icc_fallsem = icc_fallsem$LowerCI
upper_icc_fallsem = icc_fallsem$UpperCI


## Get range of ICC estimates based on observed data
icc_all = ICCest(cluster_id, is_ca, data = studlevel_results)
lower_icc_all = icc_all$LowerCI
upper_icc_all = icc_all$UpperCI

## calculate final number of students and clusters
schools_forsim = studlevel_results %>%
        group_by(school_anon, stud_tx_status) %>%
        summarise(n_students = length(unique(usi)),
                  n_teachers = length(unique(cluster_id))) %>%
        ungroup() %>%
        mutate(is_spring_sem = ifelse(!school_anon %in% c("School A", "School B", "School C"), TRUE, FALSE)) 
  

## power for binary outcomes
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


## fall sem calculation
icc_fall = seq(from = round(lower_icc_fallsem, 2),
               to = round(upper_icc_fallsem, 2), by = 0.01)
n_clusters_fall = sum(schools_forsim$n_teachers[!schools_forsim$is_spring_sem])
n_percluster_fall = mean(studlevel_results %>% filter(!is_spring_sem) %>% group_by(cluster_id) %>% summarise(n_stud = n()) %>% pull(n_stud))
cv_clustersize_fall = sd(studlevel_results %>% filter(!is_spring_sem) %>% group_by(cluster_id) %>% summarise(n_stud = n()) %>% pull(n_stud))/n_percluster_fall
control_baserate_fall = mean(studlevel_results$is_ca[!studlevel_results$is_spring_sem])

  
mde_ca_fall = lapply(icc_fall, prop_power, n_clusters = n_clusters_fall,
                     n_percluster = n_percluster_fall,
                     cv_clustersize = cv_clustersize_fall,
                     control_baserate = control_baserate_fall, direction = FALSE)
mde_ca_df = data.frame(icc = icc_fall,
                       tx_rate = unlist(mde_ca_fall),
                       base_rate = control_baserate) %>%
          mutate(mde = base_rate-tx_rate)

ggplot(mde_ca_df, aes(x = icc, y = mde)) +
  geom_point(size = 3) +
  geom_line() +
  theme_new() +
  xlab("Intra-class correlation coefficient\nfor attendance between students with same teacher") +
  ylab(sprintf("Minimal detectable effect for\nreduction in chronic absenteeism\nBase rate: %s\nUnits: percentage points", round(control_baserate_fall, 2))) +
  scale_y_continuous(breaks = pretty_breaks(n = 20), limits = c(0, 0.3))

ggsave("output/posthoc_teachertext_poweranalysis_CA_fallonly.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

## combined across both semesters  
icc = seq(from = round(lower_icc_all, 2),
               to = round(upper_icc_all, 2), by = 0.01)
n_clusters = sum(schools_forsim$n_teachers)
n_percluster = mean(studlevel_results %>% group_by(cluster_id)
                    %>% summarise(n_stud = n()) %>% pull(n_stud))
cv_clustersize = sd(studlevel_results %>% group_by(cluster_id) 
                    %>% summarise(n_stud = n()) %>% pull(n_stud))/n_percluster_fall
control_baserate = mean(studlevel_results$is_ca)

  
mde_ca_all = lapply(icc, prop_power, n_clusters = n_clusters,
                     n_percluster = n_percluster,
                     cv_clustersize = cv_clustersize,
                     control_baserate = control_baserate, direction = FALSE)
mde_ca_df_all = data.frame(icc = icc,
                       tx_rate = unlist(mde_ca_all),
                       base_rate = control_baserate) %>%
          mutate(mde = base_rate-tx_rate)

ggplot(mde_ca_df_all, aes(x = icc, y = mde)) +
  geom_point(size = 3) +
  geom_line() +
  theme_new() +
  xlab("Intra-class correlation coefficient\nfor attendance between students with same teacher") +
  ylab(sprintf("Minimal detectable effect for\nreduction in chronic absenteeism\nBase rate: %s\nUnits: percentage points", round(control_baserate, 2))) +
  scale_y_continuous(breaks = pretty_breaks(n = 20), limits = c(0, 0.25))

ggsave("output/posthoc_teachertext_poweranalysis_CA_all.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

## isa 
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


## not edited 
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



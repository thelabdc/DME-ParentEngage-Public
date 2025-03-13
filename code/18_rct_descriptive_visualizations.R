#######################################
# Imports and constants
#######################################
rm(list=ls())
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(scales)
library(here)
library(ggrepel)
library(rdd)


# graphing theme
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

# lab col
treatment_col = "#2b4888"
control_col = "#444444"
combined_col = "white"

#######################################
# Read in data
#######################################

# Data used in script 20 to estimate regressions
studlevel_results <- fread('data/rct_datasets/dcps_pcs_analytic_sample_for_dataviz.csv')

# source of dem
table(studlevel_results$school_year)

# Looking at fall results only
studlevel_results_fallsem <- studlevel_results %>% 
  filter(!is_spring_sem) %>%
  left_join(studlevel_results %>% group_by(school_anon, final_status) %>%
              summarise(n_students = n()),
            by= c("school_anon", "final_status")) %>%
  mutate(tx_forgraph = ifelse(final_status == "Tx only",
                              "T: Teachers+Admins",
                              "C: Admins only"))

## attributes to summarize

#######################################
# Examine balance by tx status by school
#######################################

studlevel_results = studlevel_results %>%
              mutate(derived_isblack = ifelse(derived_race == "Black",
                                              TRUE, FALSE),
                     derived_ishispanic = ifelse(derived_race == "Hispanic",
                                              TRUE, FALSE),
                     derived_whiteother = ifelse(derived_race %in% c("White", "Other"),
                                              TRUE, FALSE),
                     derived_homelang_span = ifelse(derived_homelang == "Spanish", TRUE, FALSE),
                     derived_homelang_eng = ifelse(derived_homelang == "English", TRUE, FALSE),
                     derived_homelang_oth = ifelse(!derived_homelang %in% c("Spanish", "English"), 
                                                   TRUE, FALSE))

dem_attributes = c("N_students_taught", "derived_ell", "derived_atrisk", "derived_grade",
                   "derived_isfemale", grep("black|hispanic|other|homelang\\_", 
                                            colnames(studlevel_results), value = TRUE)) 

## group by tx condition only, mean
dem_att_comp_tx = studlevel_results %>% group_by(stud_tx_status) %>%
  summarise_at(.vars = dem_attributes, mean) %>%
  reshape2::melt(., id.vars = c("stud_tx_status")) %>%
  reshape2::dcast(variable ~ stud_tx_status, value.var = "value") %>%
  arrange(variable)

## group by school and tx condition, mean
dem_att_comp_school = studlevel_results %>% group_by(school_anon, stud_tx_status) %>%
          summarise_at(.vars = dem_attributes, mean) %>%
          reshape2::melt(., id.vars = c("school_anon", "stud_tx_status")) %>%
          reshape2::dcast(school_anon + variable ~ stud_tx_status, value.var = "value") %>%
          arrange(variable, school_anon)

## add p-values not grouping by school
dem_att_inf_list = lapply(dem_attributes, 
                     function(x) t.test(formula(sprintf("%s ~ stud_tx_status", x)), 
                          data = studlevel_results)$p.value) 

dem_att_inf_df = data.frame(variable = dem_attributes, p = unlist(dem_att_inf_list))


dem_att_mean_winf = merge(dem_att_comp_tx, dem_att_inf_df, by = "variable") %>%
              mutate(p_clean = case_when(p < 0.0001 ~ "p < 0.0001",
                                      p < 0.001 ~ "p < 0.001",
                                      TRUE ~ as.character(round(p, digits = 4))),
                     control_perc = case_when(!variable %in% c("derived_grade", "N_students_taught") ~ Control_only * 100,
                                              TRUE ~ Control_only),
                     treatment_perc = ifelse(!variable %in% c("derived_grade", "N_students_taught"), Tx_only*100,
                                             Tx_only)) %>%
              dplyr::select(-Control_only, -Tx_only, -p) 

write.csv(dem_att_mean_winf, "analysis_output/dem_comparison_table_fullsamp.csv",
          row.names = FALSE)

## write just means for the school by school breakdown
dem_att_comp_school_towrite = dem_att_comp_school %>%
  mutate(control_perc = ifelse(!variable %in% c("derived_grade", "N_students_taught"), Control_only * 100,
                               Control_only),
         treatment_perc = ifelse(!variable %in% c("derived_grade", "N_students_taught"), Tx_only*100,
                                 Tx_only)) %>%
  dplyr::select(-Control_only, -Tx_only) 

write.csv(dem_att_comp_school_towrite, "analysis_output/dem_comparison_table_byschool.csv",
          row.names = FALSE)

#######################################
# Compare CA to ISA
#######################################

## round isa
studlevel_isa_prop = studlevel_results_fallsem  %>%
          mutate(isa_rounded = round(isa, 2)) %>%
          group_by(isa_rounded, tx_forgraph) %>%
          summarise(num = n()) %>%
          left_join(studlevel_results_fallsem %>%
                      group_by(tx_forgraph) %>%
                      summarise(denom = n())) %>%
          mutate(prop = num/denom)
          


## plot distribution of isa versus ca
ggplot(studlevel_isa_prop, aes(x = isa_rounded, y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~tx_forgraph) +
  theme_new() +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_continuous(breaks = seq(from = 0, to= 1, by = 0.1),
                     limits = c(-0.05, 1)) +
  xlab("In-seat attendance (rounded)") +
  ylab("Proportion of group") +
  geom_vline(aes(xintercept = 0.9), linetype = "dashed", color = "red",
             size = 2) 

ggsave("output/analytical_sample_outputs/isa_threshold_proportions.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

## other view
ggplot(studlevel_results_fallsem %>%
        mutate(ca_descrip = ifelse(is_ca, "Yes", "No")), aes(x = isa,
                                      group = ca_descrip,
                                      fill = ca_descrip)) +
  geom_histogram(bins = 100, color = "black") +
  facet_wrap(~tx_forgraph) +
  theme_new() +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_continuous(breaks = pretty_breaks(n = 20),
                     limits = c(0, 1)) +
  xlab("In-seat attendance") +
  ylab("Count") +
  scale_fill_manual(values = c("No" = "darkgreen",
                               "Yes" = "firebrick")) +
  theme(legend.position = c(0.2, 0.8)) +
  labs(fill = "Chronically\nabsent?") 

ggsave("output/analytical_sample_outputs/isa_threshold_counts.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)


## perform test for sorting around threshold
tx_group = studlevel_results_fallsem %>% filter(final_status == "Tx only")
control_group = studlevel_results_fallsem %>% filter(final_status == "Control only")

DCdensity(tx_group$isa, cutpoint = 0.9)
DCdensity(control_group$isa, cutpoint = 0.9)

            

#######################################
# Visualize: DCPS 
#######################################

## create tx color map
tx_colmap = c("T: Teachers+Admins" = treatment_col,
              "C: Admins only" = control_col,
              "Groups combined" = combined_col) 


######## 1. Visualize chronic absenteeism rates 
#### anacostia and other averages: https://osse.dc.gov/sites/default/files/dc/sites/osse/publication/attachments/2019-20%20Attendance%20Report.pdf
ca_rates_eoy = rbind.data.frame(studlevel_results_fallsem %>%
                                  group_by(final_status) %>%
                                  summarise(ca_rate = mean(is_ca)) %>%
                                  mutate(time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                         ca_pct = ca_rate*100),
                                data.frame(ca_rate = mean(studlevel_results_fallsem$is_ca)) %>%
                                  mutate(final_status = "Combined",
                                         time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                         ca_pct = ca_rate*100) %>%
                                  select(final_status, ca_rate, time_horizon, ca_pct)) 


write_summary_tables <- TRUE
if(write_summary_tables){
  write.csv(ca_rates_eoy, "output/analytical_sample_outputs/carates_long.csv",
            row.names = FALSE)
}


ca_rates_2weeks = rbind.data.frame(studlevel_results_fallsem %>%
                                     group_by(final_status) %>%
                                     summarise(ca_rate = mean(is_ca_2week)) %>%
                                     mutate(time_horizon = "Two weeks after\naccount activation",
                                            ca_pct = ca_rate*100),
                                   data.frame(ca_rate = mean(studlevel_results_fallsem$is_ca_2week)) %>%
                                     mutate(final_status = "Combined",
                                            time_horizon = "Two weeks after\naccount activation",
                                            ca_pct = ca_rate*100) %>%
                                     select(final_status, ca_rate, time_horizon, ca_pct)) 

if(write_summary_tables){
  write.csv(ca_rates_2weeks, "output/analytical_sample_outputs/carates_short.csv",
            row.names = FALSE) 
  
}


## rowbind and visualize both on same plot
ca_rates_bothtime = rbind.data.frame(ca_rates_eoy,
                                     ca_rates_2weeks) %>%
  mutate(tx_forgraph = ifelse(final_status == "Tx only",
                              "T: Teachers+Admins",
                              ifelse(final_status == "Combined",
                                     "Groups combined",
                                     "C: Admins only")))

# Full semester only
ggplot(ca_rates_bothtime %>% 
         filter(grepl('Full semester', time_horizon)),
       aes(x = factor(time_horizon),
           y = ca_pct,
           group = tx_forgraph,
           fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("Time window") +
  ylab("Rate of chronic\nabsenteeism (%)\n(want to decrease)") +
  theme(legend.position = c(0.6, 0.85),
        legend.background = element_blank()) +
  geom_label(aes(x = factor(time_horizon),
                 y = round(ca_pct),
                 group = tx_forgraph,
                 label = paste0(round(ca_pct), '%')),
             fill = "white",
             position = position_dodge(width = 1),
             size = 6) +
  ylim(0, 80) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap)

## Overwrite prev, we want full sem to be in main part so easier to overwrite
ggsave("output/analytical_sample_outputs/ca_rawrates.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

ggplot(ca_rates_bothtime %>% 
         filter(grepl('Full semester', time_horizon) &
                tx_forgraph != "Groups combined"),
       aes(x = factor(time_horizon),
           y = ca_pct,
           group = tx_forgraph,
           fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("Time window") +
  ylab("Rate of chronic\nabsenteeism (%)\n(want to decrease)") +
  theme(legend.position = c(0.7, 0.85),
        legend.background = element_blank(),
        legend.text = element_text(size = 24)) +
  geom_label(aes(x = factor(time_horizon),
                 y = round(ca_pct),
                 group = tx_forgraph,
                 label = paste0(round(ca_pct), '%')),
             fill = "white",
             position = position_dodge(width = 1),
             size = 8) +
  ylim(0, 80) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap)

## Overwrite prev, we want full sem to be in main part so easier to overwrite
ggsave("output/analytical_sample_outputs/ca_rawrates_nocombine.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)


ggplot(ca_rates_bothtime %>% 
         filter(grepl('Two weeks',time_horizon)),
       aes(x = factor(time_horizon),
           y = ca_pct,
           group = tx_forgraph,
           fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("Time window") +
  ylab("Rate of chronic\nabsenteeism (%)\n(want to decrease)") +
  theme(legend.position = c(0.6, 0.85),
        legend.background = element_blank()) +
  geom_label(aes(x = factor(time_horizon),
                 y = round(ca_pct),
                 group = tx_forgraph,
                 label = paste0(round(ca_pct), '%')),
             fill = "white",
             position = position_dodge(width = 1),
             size = 6) +
  ylim(0, 80) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap)

ggsave("output/analytical_sample_outputs/ca_rawrates_2weeks.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

ggplot(ca_rates_bothtime %>% 
         filter(grepl('Two weeks',time_horizon) &
                  tx_forgraph != "Groups combined"),
       aes(x = factor(time_horizon),
           y = ca_pct,
           group = tx_forgraph,
           fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("Time window") +
  ylab("Rate of chronic\nabsenteeism (%)\n(want to decrease)") +
  theme(legend.position = c(0.6, 0.85),
        legend.background = element_blank()) +
  geom_label(aes(x = factor(time_horizon),
                 y = round(ca_pct),
                 group = tx_forgraph,
                 label = paste0(round(ca_pct), '%')),
             fill = "white",
             position = position_dodge(width = 1),
             size = 6) +
  ylim(0, 80) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap)

ggsave("output/analytical_sample_outputs/ca_rawrates_2weeks_nocombine.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)


## add raw rates by school
ca_rates_eoy_byschool = rbind.data.frame(studlevel_results_fallsem %>%
                                  group_by(final_status, school_anon) %>%
                                  summarise(ca_rate = mean(is_ca), 
                                            n_students = unique(n_students)) %>%
                                  ungroup() %>%
                                  mutate(time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                         group_size = sprintf("%s:\nN students in group: %s",
                                                                    school_anon,
                                                                    n_students)),
                                  studlevel_results_fallsem %>%
                                    group_by(school_anon) %>%
                                    summarise(ca_rate = mean(is_ca),
                                              n_students = length(unique(usi))) %>%
                                    ungroup() %>%
                                    mutate(time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                           group_size = sprintf("%s:\nN students in group: %s",
                                                                school_anon,
                                                                n_students),
                                           final_status = "Combined")) %>%
  mutate(tx_forgraph = ifelse(final_status == "Tx only",
                              "T: Teachers+Admins",
                              ifelse(final_status == "Combined",
                                     "Groups combined",
                                     "C: Admins only")), 
         ca_pct = ca_rate*100)

# Round rates
ggplot(ca_rates_eoy_byschool, aes(x = factor(school_anon),
                              y = ca_pct,
                              group = tx_forgraph,
                              fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("School") +
  ylab("Rate of chronic\nabsenteeism (%)\n(want to decrease)") +
  theme(legend.position = c(0.8, 0.85),
        legend.background = element_blank()) +
  geom_label(aes(x = factor(school_anon),
                 y = ca_pct,
                 group = tx_forgraph,
                 label = sprintf("Rate: %s\nN: %s", 
                                 ifelse(ca_pct == 100, '>95%', 
                                        paste0(round(ca_pct), '%')),
                                 n_students)),
             fill = "white",
             position = position_dodge(width = 1),
             size = 4) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap, 
                    breaks = names(tx_colmap)[c(2,3,1)])

ggsave("output/analytical_sample_outputs/ca_rawrates_byschool_eoy.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

ca_rates_2week_byschool = rbind.data.frame(studlevel_results_fallsem %>%
                                           group_by(final_status, school_anon) %>%
                                           summarise(ca_rate = mean(is_ca_2week), n_students = unique(n_students)) %>%
                                           ungroup() %>%
                                           mutate(time_horizon = "Two weeks after\naccount activation",
                                                  group_size = sprintf("%s:\nN students in group: %s",
                                                                       school_anon,
                                                                       n_students)),
                                         studlevel_results_fallsem %>%
                                           group_by(school_anon) %>%
                                           summarise(ca_rate = mean(is_ca_2week),
                                                     n_students = length(unique(usi))) %>%
                                           ungroup() %>%
                                           mutate(time_horizon = "Two weeks after\naccount activation",
                                                  group_size = sprintf("%s:\nN students in group: %s",
                                                                       school_anon,
                                                                       n_students),
                                                  final_status = "Combined")) %>%
  mutate(tx_forgraph = ifelse(final_status == "Tx only",
                              "T: Teachers+Admins",
                              ifelse(final_status == "Combined",
                                     "Groups combined",
                                     "C: Admins only")),
         ca_pct = ca_rate*100)

ggplot(ca_rates_2week_byschool, aes(x = factor(school_anon),
                                  y = ca_pct,
                                  group = tx_forgraph,
                                  fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("School") +
  ylab("Rate of chronic\nabsenteeism (%)\n(want to decrease)") +
  theme(legend.position = c(0.82, 0.85),
        legend.background = element_blank()) +
  geom_label(aes(x = factor(school_anon),
                 y = ca_pct,
                 group = tx_forgraph,
                 label = sprintf("Rate: %s\nN: %s", paste0(round(ca_pct), '%'),
                                 n_students)),
             fill = "white",
             position = position_dodge(width = 1),
             size = 4) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap)

ggsave("output/analytical_sample_outputs/ca_rawrates_byschool_2week.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)


######## 2. Visualize ISA rates
#### anacostia average: https://osse.dc.gov/sites/default/files/dc/sites/osse/publication/attachments/2019-20%20Attendance%20Report.pdf

isa_rates_eoy = rbind.data.frame(studlevel_results_fallsem %>%
                                   group_by(final_status) %>%
                                   summarise(isa = mean(isa)) %>%
                                   mutate(time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                          isa_pct = isa*100),
                                 data.frame(isa = mean(studlevel_results_fallsem$isa)) %>%
                                   mutate(final_status = "Combined",
                                          time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                          isa_pct = isa*100) %>%
                                   select(final_status, isa, time_horizon, isa_pct)) 

if(write_summary_tables){
  write.csv(isa_rates_eoy, "output/analytical_sample_outputs/isarates_long.csv",
            row.names = FALSE)
  
}

isa_rates_2weeks = rbind.data.frame(studlevel_results_fallsem %>%
                                      group_by(final_status) %>%
                                      summarise(isa = mean(isa_2week)) %>%
                                      mutate(time_horizon = "Two weeks after\naccount activation",
                                             isa_pct = isa*100),
                                    data.frame(isa = mean(studlevel_results_fallsem$isa_2week)) %>%
                                      mutate(final_status = "Combined",
                                             time_horizon = "Two weeks after\naccount activation",
                                             isa_pct = isa*100) %>%
                                      select(final_status, isa, time_horizon, isa_pct)) 

if(write_summary_tables){
  write.csv(isa_rates_2weeks, "output/analytical_sample_outputs/isarates_short.csv",
            row.names = FALSE) 
}




## rowbind and visualize both on same plot
isa_rates_bothtime = rbind.data.frame(isa_rates_eoy,
                                      isa_rates_2weeks) %>%
  mutate(tx_forgraph = ifelse(final_status == "Tx only",
                              "T: Teachers+Admins",
                              ifelse(final_status == "Combined",
                                     "Groups combined",
                                     "C: Admins only")))

## full semester isa
ggplot(isa_rates_bothtime %>% 
         filter(grepl("Full sem", time_horizon)), 
       aes(x = factor(time_horizon),
           y = isa_pct,
           group = tx_forgraph,
           fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("Time window") +
  ylab("In-seat attendance rate (%)\n(want to increase)") +
  theme(legend.position = "bottom",
        legend.background = element_blank()) +
  geom_label(aes(x = factor(time_horizon),
                 y = isa_pct,
                 group = tx_forgraph,
                 label = paste0(round(isa_pct),'%')),
             fill = "white",
             position = position_dodge(width = 1),
             size = 6) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap) +
  guides(fill = guide_legend(ncol = 2))

ggsave("output/analytical_sample_outputs/isa_rawrates.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

ggplot(isa_rates_bothtime %>% 
         filter(grepl("Full sem", time_horizon) &
              tx_forgraph != "Groups combined"), 
       aes(x = factor(time_horizon),
           y = isa_pct,
           group = tx_forgraph,
           fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("Time window") +
  ylab("In-seat attendance rate (%)\n(want to increase)") +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.text = element_text(size = 24)) +
  geom_label(aes(x = factor(time_horizon),
                 y = isa_pct,
                 group = tx_forgraph,
                 label = paste0(round(isa_pct),'%')),
             fill = "white",
             position = position_dodge(width = 1),
             size = 8) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap) +
  guides(fill = guide_legend(ncol = 2))

ggsave("output/analytical_sample_outputs/isa_rawrates_nocombine.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)


## ISA two weeks post activation
ggplot(isa_rates_bothtime %>% 
         filter(grepl("Two weeks", time_horizon)), 
       aes(x = factor(time_horizon),
           y = isa_pct,
           group = tx_forgraph,
           fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("Time window") +
  ylab("In-seat attendance rate (%)\n(want to increase)") +
  theme(legend.position = "bottom",
        legend.background = element_blank()) +
  geom_label(aes(x = factor(time_horizon),
                 y = isa_pct,
                 group = tx_forgraph,
                 label = paste0(round(isa_pct),'%')),
             fill = "white",
             position = position_dodge(width = 1),
             size = 6) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap) +
  guides(fill = guide_legend(ncol = 2))

ggsave("output/analytical_sample_outputs/isa_rawrates_2weeks.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

ggplot(isa_rates_bothtime %>% 
         filter(grepl("Two weeks", time_horizon) &
                tx_forgraph != "Groups combined"), 
       aes(x = factor(time_horizon),
           y = isa_pct,
           group = tx_forgraph,
           fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("Time window") +
  ylab("In-seat attendance rate (%)\n(want to increase)") +
  theme(legend.position = "bottom",
        legend.background = element_blank()) +
  geom_label(aes(x = factor(time_horizon),
                 y = isa_pct,
                 group = tx_forgraph,
                 label = paste0(round(isa_pct),'%')),
             fill = "white",
             position = position_dodge(width = 1),
             size = 6) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap) +
  guides(fill = guide_legend(ncol = 2))

ggsave("output/analytical_sample_outputs/isa_rawrates_2weeks_nocombine.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

isa_eoy_byschool = rbind.data.frame(studlevel_results_fallsem %>%
                                           group_by(final_status, school_anon) %>%
                                           summarise(isa_rate = mean(isa), n_students = unique(n_students)) %>%
                                           ungroup() %>%
                                           mutate(time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                                  group_size = sprintf("%s:\nN students in group: %s",
                                                                       school_anon,
                                                                       n_students)),
                                         studlevel_results_fallsem %>%
                                           group_by(school_anon) %>%
                                           summarise(isa_rate = mean(isa),
                                                     n_students = length(unique(usi))) %>%
                                           ungroup() %>%
                                           mutate(time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                                  group_size = sprintf("%s:\nN students in group: %s",
                                                                       school_anon,
                                                                       n_students),
                                                  final_status = "Combined")) %>%
  mutate(tx_forgraph = ifelse(final_status == "Tx only",
                              "T: Teachers+Admins",
                              ifelse(final_status == "Combined",
                                     "Groups combined",
                                     "C: Admins only")),
         isa_pct = isa_rate*100)


ggplot(isa_eoy_byschool, aes(x = factor(school_anon),
                                  y = isa_pct,
                                  group = tx_forgraph,
                                  fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("School") +
  ylab("In-seat attendance rate (%)\n(want to increase)") +
  theme(legend.position = c(0.18, 0.85),
        legend.background = element_blank()) +
  geom_label(aes(x = factor(school_anon),
                 y = isa_pct,
                 group = tx_forgraph,
                 label = sprintf("Rate: %s \nN: %s", paste0(round(isa_pct),'%'),
                                 n_students)),
             fill = "white",
             position = position_dodge(width = 1),
             size = 4) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap)

ggsave("output/analytical_sample_outputs/isa_byschool_eoy.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

isa_2week_byschool = rbind.data.frame(studlevel_results_fallsem %>%
                                      group_by(final_status, school_anon) %>%
                                      summarise(isa_rate = mean(isa_2week), n_students = unique(n_students)) %>%
                                      ungroup() %>%
                                      mutate(time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                             group_size = sprintf("%s:\nN students in group: %s",
                                                                  school_anon,
                                                                  n_students)),
                                    studlevel_results_fallsem %>%
                                      group_by(school_anon) %>%
                                      summarise(isa_rate = mean(isa_2week),
                                                n_students = length(unique(usi))) %>%
                                      ungroup() %>%
                                      mutate(time_horizon = "Full semester window\n(Oct 1 2019-Jan 24 2020)",
                                             group_size = sprintf("%s:\nN students in group: %s",
                                                                  school_anon,
                                                                  n_students),
                                             final_status = "Combined")) %>%
  mutate(tx_forgraph = ifelse(final_status == "Tx only",
                              "T: Teachers+Admins",
                              ifelse(final_status == "Combined",
                                     "Groups combined",
                                     "C: Admins only")),
         isa_pct = isa_rate*100)

ggplot(isa_2week_byschool, aes(x = factor(school_anon),
                             y = isa_pct,
                             group = tx_forgraph,
                             fill = tx_forgraph)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),
           color = "black") +
  theme_new(base_size = 24) +
  xlab("School") +
  ylab("In-seat attendance rate (%)\n(want to increase)") +
  theme(legend.position = c(0.18, 0.85),
        legend.background = element_blank()) +
  geom_label(aes(x = factor(school_anon),
                 y = isa_pct,
                 group = tx_forgraph,
                 label = sprintf("Rate: %s\nN: %s",
                                 paste0(round(isa_pct), '%'),
                                 n_students)),
             fill = "white",
             position = position_dodge(width = 1),
             size = 4) +
  labs(fill = "Offered TeacherText") +
  scale_fill_manual(values = tx_colmap)

ggsave("output/analytical_sample_outputs/isa_2week_eoy.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

###### 3. Visualize absence counts

## visualize cumulative sum of absences
ggplot(studlevel_results_fallsem %>%
         mutate(tx_forgraph = ifelse(final_status == "Tx only",
                                     "T: Teachers+Admins",
                                     "C: Admins only")), aes(x = cumulsum_absent_2week,
                                                             group = tx_forgraph,
                                                             fill = tx_forgraph)) +
  geom_histogram(bins = 12, binwidth = 0.5, position = "dodge",
                 color = "black") +
  xlab("Count of absences in two-weeks\nfollowing school-specific account activation") +
  theme_new(base_size = 24) +
  theme(legend.position = c(0.6, 0.8)) +
  scale_fill_manual(values = tx_colmap) +
  labs(fill = "") +
  ylab("Count of students") +
  scale_x_continuous(breaks = pretty_breaks(n = 12))

ggsave("output/analytical_sample_outputs/twoweek_abscounts.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

ggplot(studlevel_results_fallsem %>%
         mutate(tx_forgraph = ifelse(final_status == "Tx only",
                                     "T: Teachers+Admins",
                                     "C: Admins only")), aes(x = cumulsum_absent,
                                                             group = tx_forgraph,
                                                             fill = tx_forgraph)) +
  geom_histogram(bins = 10, binwidth = 1, position = "dodge",
                 color = "black") +
  xlab("Count of absences between Oct 1 2019\nand end of semester (Jan 24 2020)") +
  theme_new(base_size = 24) +
  theme(legend.position = c(0.3, 0.8)) +
  scale_fill_manual(values = tx_colmap) +
  labs(fill = "") +
  ylab("Count of students") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  facet_wrap(~tx_forgraph)

ggsave("output/analytical_sample_outputs/sem_abscounts_facet.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

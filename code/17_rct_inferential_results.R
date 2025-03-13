#######################################
### Imports and constants ####
#######################################
rm(list=ls())
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(scales)
library(DeclareDesign)
library(xtable)
library(AER)

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

## create tx color map
tx_colmap = c("T: Teachers+Admins" = treatment_col,
              "C: Admins only" = control_col,
              "Groups combined" = combined_col) 

#######################################
# Read in data ####
#######################################

studlevel_all = fread("data/rct_datasets/dcps_pcs_studlevel_dems_clusterid.csv") %>% # No dupe usi after filter
  mutate(school_anon = case_when(school_initrosters == "Anacostia" ~ "School A",
                                 school_initrosters == "Dunbar" ~ "School B",
                                 school_initrosters == "CHEC" ~ "School C",
                                 school_initrosters == "Friendship" ~ "School D",
                                 school_initrosters == "Paul" ~ "School E",
                                 school_initrosters == "Johnson" ~ "School F"))

studlevel_results = studlevel_all %>%
  filter(stud_tx_status %in% c('Tx_only', 'Control_only')) 

## students excluded by treatment status and school
table(studlevel_all$stud_tx_status, studlevel_all$school_anon)

write.csv(studlevel_results, 
          'data/rct_datasets/dcps_pcs_analytic_sample_for_dataviz.csv', 
          row.names = FALSE)

## create summary table to update in-text table
school_level_summary = studlevel_results %>%
            group_by(school_anon, stud_tx_status) %>%
            summarise(n_students = length(unique(usi)),
                      n_teachers = length(unique(cluster_id))) %>%
            ungroup() 

school_level_summary_wide =  data.table::dcast(as.data.table(school_level_summary),
              school_anon ~ stud_tx_status, value.var = c("n_students", "n_teachers")) 

totals = cbind.data.frame(school_anon = "Total",
                          t(colSums(as.data.frame(school_level_summary_wide)[, grep("^n\\_", colnames(school_level_summary_wide),
                                                                   value = TRUE)]))) 

school_level_summary_wide_wN = rbind.data.frame(school_level_summary_wide,
                                                totals) 

write.csv(school_level_summary_wide_wN, "output/countstxcontrol_byschool.csv",
          row.names = FALSE)
  
#######################################
# Control vars ####
#######################################

# Note: FARMS was not included (in DCPS) because everyone had "CEP" 
# different when we add in the rest of the students PCS students. 
# lm_robust does not like lack of variation

# we don't have homelessness data, but the at_risk flag should capture that 
controls_fallsem = c('derived_race', 'derived_isfemale', 'derived_homelang', 
              'derived_ell', 'derived_atrisk', 'derived_grade', 
              'N_students_taught')

controls_bothsem = c(controls_fallsem, "derived_is_charter")
controls_schoolFE = c(controls_fallsem, "school_anon")

## check for no missingness along covars

#######################################
# Formulas ####
#######################################

#######################################
# Fall and spring sem formulas ####
####################################### 

construct_formulas <- function(one_cont, outcome_varname){
  
  if(length(one_cont) == 0){
    one_form = formula(sprintf("%s ~ %s", outcome_varname, "stud_tx_status"))
  } else{
    one_form = formula(sprintf('%s ~ %s + %s', outcome_varname, 'stud_tx_status', paste(one_cont, collapse = "+")))
  }
  return(one_form)
}

## formula lists (tried lapply but weirdness so doing more manually)
formulas_fallsem_eoy = list(nocont_ca = construct_formulas(c(), "is_ca"),
                      cont_noFE_ca = construct_formulas(controls_fallsem, "is_ca"),
                      cont_FE_ca = construct_formulas(controls_schoolFE, "is_ca"),
                      nocont_isa = construct_formulas(c(), "isa"),
                      cont_noFE_isa = construct_formulas(controls_fallsem, "isa"),
                      cont_FE_isa = construct_formulas(controls_schoolFE, "isa")) 


formulas_fallsem_2week = list(nocont_ca_2week = construct_formulas(c(), "is_ca_2week"),
                          cont_noFE_ca_2week = construct_formulas(controls_fallsem, "is_ca_2week"),
                          cont_FE_ca_2week = construct_formulas(controls_schoolFE, "is_ca_2week"),
                          nocont_isa_2week = construct_formulas(c(), "isa_2week"),
                          cont_noFE_isa_2week = construct_formulas(controls_fallsem, "isa_2week"),
                          cont_FE_isa_2week = construct_formulas(controls_schoolFE, "isa_2week")) 

## might end up excluding this since the revised pre-ap mightve specified that 
## for spring-sem we'd only look at two weeks due to covid
formulas_bothsem_eoy = list(nocont_ca = construct_formulas(c(), "is_ca"),
                          cont_noFE_ca = construct_formulas(controls_bothsem, "is_ca"),
                          cont_FE_ca = construct_formulas(controls_schoolFE, "is_ca"),
                          nocont_isa = construct_formulas(c(), "isa"),
                          cont_noFE_isa = construct_formulas(controls_bothsem, "isa"),
                          cont_FE_isa = construct_formulas(controls_schoolFE, "isa")) 


formulas_bothsem_2week = list(nocont_ca_2week = construct_formulas(c(), "is_ca_2week"),
                            cont_noFE_ca_2week = construct_formulas(controls_bothsem, "is_ca_2week"),
                            cont_FE_ca_2week = construct_formulas(controls_schoolFE, "is_ca_2week"),
                         nocont_isa_2week = construct_formulas(c(), "isa_2week"),
                         cont_noFE_isa_2week = construct_formulas(controls_bothsem, "isa_2week"),
                         cont_FE_isa_2week = construct_formulas(controls_schoolFE, "isa_2week")) # leave as gen controls because is_charter cant be incl



#######################################
# Estimating and saving ####
####################################### 

# Save regressions + captions for overleaf
save_onereg <- function(one_reg, name){
  ## caption name$
  caption = gsub("\\_", " ", name)
  table_path = "analysis_output/"
  
  ## filename
  label = gsub("\\s+|[[:punct:]]", "_", caption)
  fname = sprintf("%s%s.tex",
                  table_path,
                  label)
  
  ## save
  print(xtable(tidy(one_reg),
               digits = 4, caption = caption,
               label = sprintf("tab:%s", label)),
        caption.placement = "top",
        file = fname,
        size = "scriptsize")
  return(NULL)
}


# Run models
run_lmrobust <- function(df_to_use, 
                         reg_formula){

 
  reg_results = lm_robust(reg_formula, 
                            data = df_to_use,
                            clusters = cluster_id,
                            se_type = "stata")
  
  return(reg_results)
}



run_models <- function (df_to_use, 
                        include_springsem = FALSE,
                        formula_list){
  
  
  if (include_springsem) {
    df = df_to_use 
    print(sprintf("Running models with following schools: %s", paste(unique(df$school_initrosters),
                                                                     collapse = "; ")))
  } else{
    df = df_to_use %>% filter(!is_spring_sem)
    print(sprintf("Running models with following schools: %s", paste(unique(df$school_initrosters),
                                                                     collapse = "; ")))
  }
  

  # Run for DCPS and PCS, no fixed effects
  #print(sprintf("Running these regs: %s", paste(formula_list, collapse = "-------------------")))
  print(sprintf("Running these regs: %s", paste(formula_list)))
  results = lapply(formula_list, function(x) run_lmrobust(df_to_use = df,
                                                      reg_formula = x))
  
  names(results) = names(formula_list)
  
  
  # Save regressions
  for(i in 1:length(results)){
    save_onereg(results[[i]],
                sprintf("%s_bothsem%s",
                names(results)[i],
                include_springsem))
  }
  return(results)
}

## create lists of formulas outside the function
fallsem_eoy <- run_models(df_to_use = studlevel_results,
                   include_springsem = FALSE,
                   formula_list = formulas_fallsem_eoy)
fallsem_2week <- run_models(df_to_use = studlevel_results,
                          include_springsem = FALSE,
                          formula_list = formulas_fallsem_2week)
bothsem_eoy <- run_models(df_to_use = studlevel_results,
                          include_springsem = TRUE,
                          formula_list = formulas_bothsem_eoy)
bothsem_2week <- run_models(df_to_use = studlevel_results,
                          include_springsem = TRUE,
                          formula_list = formulas_bothsem_2week)

#######################################
# For CACE ####
# notes from pre-ap
# 1. fallsem eoy only
# 2. atleast two messages as definition
# 3. use all three spec (no cont, cont, schoolFE)
# 4. uses aER package
#######################################

## 

## look descriptive at compliance (maybe move to script 21)
fallsem_compliance = studlevel_results %>% filter(!is_spring_sem) %>%
              group_by(stud_tx_status) %>%
              summarise_at(c("atleast_two_msgsreceived_octstart_fallsem", 
                                 "atleast_two_msgsreceived_twoweeks"), mean) %>%
              ungroup() %>%
              reshape2::melt(, id.vars = c("stud_tx_status")) %>%
              mutate(time_window = case_when(grepl("twoweeks", variable) ~ "Two-weeks\nafter activation",
                                             TRUE ~ "Oct 1 2019\nthrough end of first-semester"),
                     tx_descriptive = case_when(stud_tx_status == "Tx_only" ~ "T: Teachers+Admins",
                                                TRUE ~ "C: Admins only"))

ggplot(fallsem_compliance %>% filter(grepl('fallsem', variable)), 
       aes(x = tx_descriptive, y = value, 
          group = tx_descriptive, fill = tx_descriptive)) +
  geom_bar(stat = "identity", color = "black") +
  theme_new() +
  scale_fill_manual(values = tx_colmap) +
  geom_label(aes(x =  tx_descriptive, y = value, 
                 group = tx_descriptive, fill = tx_descriptive,
                 label = round(value, 2)),
             fill = "white") +
  xlab("Treatment status") +
  guides(fill = FALSE) +
  ylab("Proportion of students in group sent 2+ messages by\na treatment teacher\n(restricting to successfully delivered)")

ggsave("output/compliance_barplot.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

ggplot(fallsem_compliance %>% filter(grepl('twoweek', variable)), 
       aes(x = tx_descriptive, y = value, 
           group = tx_descriptive, fill = tx_descriptive)) +
  geom_bar(stat = "identity", color = "black") +
  theme_new() +
  scale_fill_manual(values = tx_colmap) +
  geom_label(aes(x =  tx_descriptive, y = value, 
                 group = tx_descriptive, fill = tx_descriptive,
                 label = round(value, 2)),
             fill = "white") +
  xlab("Treatment status") +
  guides(fill = FALSE) +
  ylab("Proportion of students in group sent 2+ messages by\na treatment teacher\n(restricting to successfully delivered)")

ggsave("output/compliance_barplot_2week.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)



##  function for cace formulas
construct_cace_formulas <-  function(one_cont, outcome_varname, comply_varname = "atleast_two_msgsreceived_activationdate_fallsem"){
  
  if(length(one_cont) == 0){
    one_form = formula(sprintf("%s ~ %s | %s", outcome_varname, "stud_tx_status", comply_varname))
  } else{
    one_form = formula(sprintf('%s ~ %s + %s | %s + %s', outcome_varname, 'stud_tx_status', paste(one_cont, collapse = "+"),
                               comply_varname,
                               paste(one_cont, collapse = "+")))
  }
  return(one_form)
}

run_models_ivreg <- function (df,
                        formula_list){
  
  
  # Run for DCPS and PCS, no fixed effects
  #print(sprintf("Running these regs: %s", paste(formula_list, collapse = "-------------------")))
  print(sprintf("Running these regs: %s", paste(formula_list)))
  results = lapply(formula_list, function(x) 
    iv_robust(x,
              data = df,
              clusters = cluster_id,
              se_type = "stata")  )
  
  names(results) = names(formula_list)
  
  
  # Save regressions
  for(i in 1:length(results)){
    save_onereg(results[[i]],
                names(results)[i])
  }
  return(results)
}

## create the formulas 
formulas_fallsem_cace = list(# Activation date start through end of semester
                             nocont_ca_eoy_cace = construct_cace_formulas(c(), "is_ca"),
                             cont_ca_eoy_cace = construct_cace_formulas(controls_fallsem, "is_ca"),
                             nocont_isa_eoy_cace = construct_cace_formulas(c(), "isa"),
                             cont_isa_eoy_cace = construct_cace_formulas(controls_fallsem, "isa"),
                             # October 1 start through end of semester
                             nocont_ca_eoy_cace_octstart = construct_cace_formulas(c(), "is_ca", 'atleast_two_msgsreceived_octstart_fallsem'),
                             cont_ca_eoy_cace_octstart = construct_cace_formulas(controls_fallsem, "is_ca", 'atleast_two_msgsreceived_octstart_fallsem'),
                             nocont_isa_eoy_cace_octstart = construct_cace_formulas(c(), "isa", 'atleast_two_msgsreceived_octstart_fallsem'),
                             cont_isa_eoy_cace_octstart = construct_cace_formulas(controls_fallsem, "isa", 'atleast_two_msgsreceived_octstart_fallsem'),
                             # Activation date + two weeks
                             nocont_ca_2week_cace = construct_cace_formulas(c(), "is_ca_2week", 'atleast_two_msgsreceived_twoweeks'),
                             cont_ca_2week_cace = construct_cace_formulas(controls_fallsem, "is_ca_2week", 'atleast_two_msgsreceived_twoweeks'),
                             nocont_isa_2week_cace = construct_cace_formulas(c(), "isa_2week", 'atleast_two_msgsreceived_twoweeks'),
                             cont_isa_2week_cace = construct_cace_formulas(controls_fallsem, "isa_2week", 'atleast_two_msgsreceived_twoweeks'))

fallsem_cace <- run_models_ivreg(studlevel_results %>% filter(!is_spring_sem),
                                 formulas_fallsem_cace)



#######################################
# Randomization inference robustness check
#######################################


## From Pre-AP
# Estimate the model specified in Equation \ref{eq:primary_spec}. Obtain each student's residuals from the model.
# Randomize the treatment status $m = 1000$ times, following the randomization procedures detailed for each school
# Use resid from step one to generate observed test statistic from wilcoxon ranked sign test
# Get permuted test statistics

# just conducting for: (1) dcps, (2) covar specification, (3) for now start with eoy 
### randomize treatment status once 
### not doing blocking but do within-school probabilities

## function to construct formulas with the covariates only
## for the residualized outcome and not the treatment status 
run_covaronly_mods <- function(one_cont, outcome_varname, df_to_use){
  
  one_form =  formula(sprintf('%s ~ %s', outcome_varname, paste(one_cont, collapse = "+")))
  reg_results = lm_robust(one_form, 
                          data = df_to_use,
                          clusters = cluster_id,
                          se_type = "stata")
  return(reg_results)
}


## filter to fall-semester schools
fallsem_schools_studlevel = studlevel_results %>% filter(!is_spring_sem)

## iterate over the models and run covar only model
outcome_vector = c("is_ca", "isa", "is_ca_2week", "isa_2week")
models_for_RI = lapply(outcome_vector,
                       run_covaronly_mods, df_to_use = fallsem_schools_studlevel,
                       one_cont = controls_fallsem)
names(models_for_RI) = outcome_vector 

## get teacher-level randomization probabilities
teacher_roster = fallsem_schools_studlevel %>% select(school_anon, cluster_id, stud_tx_status) %>% distinct() %>%
  rename(observed_tx_status = stud_tx_status)
school_txprob = teacher_roster %>%
  group_by(school_anon) %>%
  summarise(teacher_prob_tx = sum(observed_tx_status == "Tx_only")/n())


permute_tx_status <- function(teacher_roster, school_txprob, fallsem_schools_studlevel){
  
  ## storage for school-specific permutations
  permuted_tx_allschools = list() 
  
  ## iterate over schools
  for(one_school in unique(teacher_roster$school_anon)){
    
    ## filter to that school's teacher roster
    rost_one = teacher_roster %>%
      filter(school_anon == one_school) 
    
    ## add permuted tx status using that school's teacher RA probability
    rost_one$permuted_tx_status = sample(c("Tx_only", "Control_only"),
                                         size = nrow(rost_one),
                                         prob = c(school_txprob %>% 
                                                    filter(school_anon == one_school) %>%
                                                    pull(teacher_prob_tx), 
                                                  1 - school_txprob %>% 
                                                    filter(school_anon == one_school) %>%
                                                    pull(teacher_prob_tx)),
                                         replace = TRUE)
    
    ## merge back that permuted tx status onto main data
    stud_wpermute_tx = merge(fallsem_schools_studlevel, rost_one,
                     by = c("school_anon", "cluster_id")) 
    
    ## store that 
    permuted_tx_allschools[[one_school]] = stud_wpermute_tx
    
  }
 
  ## rowbind each school's permuted tx status into one list
  permuted_tx_allschools_df = do.call(rbind.data.frame, permuted_tx_allschools)
  return(permuted_tx_allschools_df)
  
}


## get all permutations
RUN_PERMUTE <- FALSE
if(RUN_PERMUTE){
  set.seed(40484)
  print("conducting permutations")
  all_permutations = replicate(1000, permute_tx_status(teacher_roster = teacher_roster,
                                                       school_txprob = school_txprob,
                                                       fallsem_schools_studlevel = fallsem_schools_studlevel),
                               simplify = FALSE)
  saveRDS(all_permutations, "data/intermediate_objects/permuted_tx_status.RDS")
  
} else{
  print("reading in permutations")
  all_permutations = readRDS("data/intermediate_objects/permuted_tx_status.RDS")
}

## function that takes in one permutation, one model  and gets the obs and permuted test stat
## the observed test stat is the same across permutations; the permuted varies
wilcox_obs_permute <- function(one_permutation, onemodel, one_outcome_name, fallsem_schools_studlevel){
  
  ## merge permuted tx status onto data with that model's residualized outcome
  ## using USI
  df_wpermute = cbind.data.frame(fallsem_schools_studlevel %>%
                               select(usi, school_anon, stud_tx_status, cluster_id, one_outcome_name),
                             pred_y = onemodel$fitted.values) %>%
    mutate(student_resid = !!sym(one_outcome_name) - pred_y) %>% # left join onto one permutation using usi
    left_join(one_permutation %>% select(usi, observed_tx_status, permuted_tx_status), 
              by = "usi") 
  
  ## estimate models
  obs_teststat = wilcox.test(df_wpermute$student_resid ~ df_wpermute$stud_tx_status)
  permut_teststat = wilcox.test(df_wpermute$student_resid ~ df_wpermute$permuted_tx_status)
  return(list(observed_teststat = obs_teststat$statistic,
              permuted_teststat = permut_teststat$statistic))
  
}


## separate permute of just OLS
ols_permute <- function(one_permutation, one_outcome_name, fallsem_schools_studlevel){
  
  ## merge permuted tx status onto data with that model's residualized outcome
  ## using USI
  df_wpermute = fallsem_schools_studlevel %>%
            select(usi, school_anon, stud_tx_status, cluster_id, one_outcome_name, all_of(controls_fallsem)) %>%
    left_join(one_permutation %>% select(usi, observed_tx_status, permuted_tx_status), 
              by = "usi") 
  ## estimate models
  obs_teststat = lm(formula(sprintf("%s ~ %s + %s", one_outcome_name, 
                                    "observed_tx_status", paste(controls_fallsem, collapse = "+"))), data = df_wpermute)
  permut_teststat = lm(formula(sprintf("%s ~ %s + %s", one_outcome_name, 
                                       "permuted_tx_status", paste(controls_fallsem, collapse = "+"))),
                       data = df_wpermute)
  return(list(observed_teststat = obs_teststat,
              permuted_teststat = permut_teststat))
  
}

extract_coefs <- function(one_regobj, name_tx, which_stat = "permuted_teststat"){
  coef = summary(one_regobj[[which_stat]])$coefficients[name_tx, "Estimate"]
  return(coef)
}


### p-value based on here - https://egap.org/resource/10-things-to-know-about-randomization-inference/- two-tailed
process_permute_results <- function(list_permuted_results, name_outcome, type_test = "wilcox"){
  
  ## get vector of permuted test statistics
  if(type_test == "wilcox"){
    vector_permut_teststat = unlist(lapply(list_permuted_results, function(x) x$permuted_teststat))
    ## two tailed comparison- just take first observed since constant across list elements
    ri_p_twotail = sum(abs(vector_permut_teststat) >= abs(list_permuted_results[[1]]$observed_teststat))/length(list_permuted_results)
  } else{
    vector_permut_teststat = unlist(lapply(list_permuted_results, extract_coefs, name_tx = "permuted_tx_statusTx_only"))
    observed_teststat = extract_coefs(list_permuted_results[[1]], name_tx = "observed_tx_statusTx_only", which_stat = "observed_teststat")
    ri_p_twotail = sum(abs(vector_permut_teststat) >= abs(observed_teststat))/length(list_permuted_results)
  }

  
  ## store in a df
  ri_p_summary <- data.frame(outcome = name_outcome,
                             pval = ri_p_twotail)
  return(ri_p_summary)
  
}


## run permutations with 
## wilcoxon 
RUN_RIMODELS <- TRUE 
if(RUN_RIMODELS){
  
  all_permutations_eoy_ca = lapply(all_permutations, wilcox_obs_permute, 
                                   onemodel = models_for_RI$is_ca,
                                   one_outcome_name ="is_ca",
                                   fallsem_schools_studlevel =  fallsem_schools_studlevel)
  saveRDS(all_permutations_eoy_ca,
          "data/intermediate_objects/all_permutations_eoy_ca.RDS")
  print("ran and saved eosem ca")
  all_permutations_eoy_isa = lapply(all_permutations, wilcox_obs_permute, 
                                   onemodel = models_for_RI$isa,
                                   one_outcome_name ="isa",
                                   fallsem_schools_studlevel =  fallsem_schools_studlevel)
  saveRDS(all_permutations_eoy_isa,
          "data/intermediate_objects/all_permutations_eoy_isa.RDS")
  print("ran and saved eosem isa")
  all_permutations_ca2wk = lapply(all_permutations, wilcox_obs_permute, 
                                    onemodel = models_for_RI$is_ca_2week,
                                    one_outcome_name ="is_ca_2week",
                                    fallsem_schools_studlevel =  fallsem_schools_studlevel)
  saveRDS(all_permutations_ca2wk,
          "data/intermediate_objects/all_permutations_ca2wk.RDS")
  print("ran and saved 2wk ca")
  all_permutations_isa2wk = lapply(all_permutations, wilcox_obs_permute, 
                                  onemodel = models_for_RI$isa_2week,
                                  one_outcome_name ="isa_2week",
                                  fallsem_schools_studlevel =  fallsem_schools_studlevel)
  saveRDS(all_permutations_isa2wk,
          "data/intermediate_objects/all_permutations_isa2wk.RDS")
  print("ran and saved 2wk isa")
  
} else{
  
  ## read in
  all_permutations_eoy_ca <- readRDS("data/intermediate_objects/all_permutations_eoy_ca.RDS")
  all_permutations_eoy_isa <- readRDS("data/intermediate_objects/all_permutations_eoy_isa.RDS")
  all_permutations_ca2wk <- readRDS("data/intermediate_objects/all_permutations_ca2wk.RDS")
  all_permutations_isa2wk <- readRDS("data/intermediate_objects/all_permutations_isa2wk.RDS")
}


## run permutations with 
## ols 
RUN_RIMODELS_OLS <- TRUE 
if(RUN_RIMODELS_OLS){
  
  all_permutations_eoy_ca_ols = lapply(all_permutations, ols_permute, 
                                   one_outcome_name ="is_ca",
                                   fallsem_schools_studlevel =  fallsem_schools_studlevel)
  saveRDS(all_permutations_eoy_ca_ols,
          "data/intermediate_objects/all_permutations_eoy_ca_ols.RDS")
  print("ran and saved eosem ca")
  all_permutations_eoy_isa_ols = lapply(all_permutations, ols_permute, 
                                    one_outcome_name ="isa",
                                    fallsem_schools_studlevel =  fallsem_schools_studlevel)
  saveRDS(all_permutations_eoy_isa_ols,
          "data/intermediate_objects/all_permutations_eoy_isa_ols.RDS")
  print("ran and saved eosem isa")
  all_permutations_ca2wk_ols = lapply(all_permutations, ols_permute, 
                                  one_outcome_name ="is_ca_2week",
                                  fallsem_schools_studlevel =  fallsem_schools_studlevel)
  saveRDS(all_permutations_ca2wk_ols,
          "data/intermediate_objects/all_permutations_ca2wk_ols.RDS")
  print("ran and saved 2wk ca")
  all_permutations_isa2wk_ols = lapply(all_permutations, ols_permute, 
                                   one_outcome_name ="isa_2week",
                                   fallsem_schools_studlevel =  fallsem_schools_studlevel)
  saveRDS(all_permutations_isa2wk_ols,
          "data/intermediate_objects/all_permutations_isa2wk_ols.RDS")
  print("ran and saved 2wk isa")
  
} else{
  
  ## read in
  all_permutations_eoy_ca_ols <- readRDS("data/intermediate_objects/all_permutations_eoy_ca_ols.RDS")
  all_permutations_eoy_isa_ols <- readRDS("data/intermediate_objects/all_permutations_eoy_isa_ols.RDS")
  all_permutations_ca2wk_ols <- readRDS("data/intermediate_objects/all_permutations_ca2wk_ols.RDS")
  all_permutations_isa2wk_ols <- readRDS("data/intermediate_objects/all_permutations_isa2wk_ols.RDS")
}
  
## rowbind into a table: wilcox 
summary_ri_results = rbind.data.frame(process_permute_results(all_permutations_eoy_ca, "Chronic absenteeism: end of semester"),
                                      process_permute_results(all_permutations_eoy_isa, "ISA: end of semester"),
                                      process_permute_results(all_permutations_ca2wk, "Chronic absenteeism: two weeks after activation"),
                                      process_permute_results(all_permutations_isa2wk, "ISA: two weeks after activation"))

print(xtable(summary_ri_results), include.rownames = FALSE)



## rowbind into a table: ols 
summary_ri_results_ols = rbind.data.frame(process_permute_results(all_permutations_eoy_ca_ols, "Chronic absenteeism: end of semester", type_test = "ols"),
                                      process_permute_results(all_permutations_eoy_isa_ols, "ISA: end of semester", type_test = "ols"),
                                      process_permute_results(all_permutations_ca2wk_ols, "Chronic absenteeism: two weeks after activation", type_test = "ols"),
                                      process_permute_results(all_permutations_isa2wk_ols, "ISA: two weeks after activation", type_test = "ols"))

print(xtable(summary_ri_results_ols), include.rownames = FALSE)

## plot main outcome
main_ca_permute = data.frame(permute_coef = unlist(lapply(all_permutations_eoy_ca_ols, function(x) summary(x$permuted_teststat)$coefficients["permuted_tx_statusTx_only", "Estimate"])))

ggplot(main_ca_permute, aes(x = permute_coef)) +
  geom_density(fill = "wheat4") +
  xlab("Distributed of permuted coefficients on treatment\n(end-of-semester chronic absenteeism)") +
  ylab("Density of draws (out of 1000)") +
  theme_new() +
  geom_vline(xintercept = 
  summary(all_permutations_eoy_ca_ols[[1]]$observed_teststat)$coefficients["observed_tx_statusTx_only", "Estimate"],
  linetype = "dashed",
  color = "red", size = 2) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y= element_blank())

ggsave("output/analytical_sample_outputs/ri_dist.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)
  

#######################################
# Imports and constants ####
#######################################

rm(list=ls())
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(scales)
library(here)
library(readxl)
library(tidyr)

#######################################
# Read in data ####
####################################### 

# Student-level data output from Script #14, which includes calculated CA and ISA rates
studentlevel_bothtimewindows_newid <- fread(here("data/rct_datasets/dcpspcs_studlevel_foranalysis.csv")) 


# Student-teacher roster data, for merging into student-level data
# There are some students where TeacherName1 and TeacherName 2 are both blank
# Removing these
teacher_studroster_init_dcps <- fread(here('data/rct_datasets/student_roster_sent01312020.csv')) %>%
  mutate(school_initrosters = case_when(grepl("CHEC", InstitutionName) ~ "CHEC", 
                                        grepl("Dunbar", InstitutionName) ~ "Dunbar",
                                        grepl("Anacost", InstitutionName) ~ "Anacostia"), 
         TeacherName1 = trimws(TeacherName1), 
         TeacherEmail1 = trimws(TeacherEmail1)) %>% 
  select(-ReportDateRange, -StudentName, 
         -TeacherName2, -TeacherEmail2, -InstitutionName) %>%
  filter(TeacherName1 != "")

# Load and consolidate PCS rosters- excludes friendship for now
## add from other script
johnson_roster = read.csv(here("data/randomization_status/Johnson MS Rosters for Randomization (1).csv"))
paul_roster = read.csv(here("data/randomization_status/Paul Rosters at Randomization - 2019-09-18 - 2019-09-18.csv"))
friendship_roster = read.csv(here("data/randomization_status/Friendship Rosters at Randomization - 2019-08-28 - 2019-08-29.csv"))
teacher_studroster_init_pcs = rbind.data.frame(johnson_roster %>% select(-StudentName), paul_roster %>% 
                                                 select(-ReportDateRange, -LastName, -FirstName),
                                               friendship_roster %>% select(-ReportDateRange, -LastName, -FirstName)) %>%
              mutate(school_initrosters = case_when(grepl("Paul", InstitutionName) ~ "Paul",
                                                    grepl("Johnson", InstitutionName) ~ "Johnson",
                                                    grepl("Friendship", InstitutionName) ~ "Friendship"),
                     TeacherName1 = trimws(TeacherName1), 
                     TeacherEmail1 = trimws(TeacherEmail1)) %>% 
  select(-TeacherName2, -TeacherEmail2, -InstitutionName) %>%
  filter(TeacherName1 != "")

## rbind fall and spring stem
teacher_studroster_init = rbind.data.frame(teacher_studroster_init_dcps %>% mutate(group = "DCPS"),
                                           teacher_studroster_init_pcs %>% mutate(group = "PCS"))

# Count number of students taught by each other
teacher_Nstudents_init <- teacher_studroster_init %>% 
  group_by(TeacherName1, TeacherEmail1) %>% 
  summarise(N_students_taught = n_distinct(StudentID))

## there's one teacher who has one name but two emails (within same school)
## and causes later duplication issues so assuming that we should sum the n students
## across her emails
teacher_Nstudents <- teacher_Nstudents_init %>%
              mutate(N_students_taught = case_when(TeacherName1 == teacher_aggregate ~ 
                                                     sum(teacher_Nstudents_init$N_students_taught[teacher_Nstudents_init$TeacherName1 == teacher_aggregate]),
                                                   TRUE ~ N_students_taught)) %>%
              filter((TeacherName1 != teacher_aggregate) | 
                    (TeacherName1 == teacher_aggregate &
                  !grepl("disabled", TeacherEmail1)))
  
# Merge N students taught back to teacher_stud roster
teacher_studroster <- merge(teacher_studroster_init, 
                            teacher_Nstudents, 
                            how = 'left', 
                            on = c('TeacherName1', 'TeacherEmail1'), 
                            all.x = TRUE) %>%
            mutate(N_students_taught = case_when(TeacherName1 == teacher_aggregate ~ 
                                                   sum(teacher_Nstudents_init$N_students_taught[teacher_Nstudents_init$TeacherName1 == teacher_aggregate]),
                                                 TRUE ~ N_students_taught))


#######################################
# Clean teacher data for merge to student data ####
####################################### 

# Teacher pilot status from Script 03 - list of which are treatment vs 
# control teachers and their strata ID
teacher_pilot_status_dcps <- fread(here('data/randomization_status/fallsemschools_summarizeRA.csv'))

# Manually correct names that do not line up with the teacher roster information
# The corrections came from doing an initial merge on emails and names, checking to see 
# which teachers in the teacher_pilot_status did not exist in the full merge, and 
# then creating an alternate id to merge these teachers on. 
# This process is commented out but moved to the bottom of this script


# dealing with disabled emails 
manual_teacher_pilot_status_fix <- read.csv(here("data/randomization_status/manual_teacher_pilot_status_fix.csv"))

#######################################
# Merge to teachers' student roster data ####
####################################### 

# add a fake id for extra security/checks
teacher_studroster_wid <- cbind(id = rownames(teacher_studroster %>% filter(group == "DCPS")), 
                                teacher_studroster %>% filter(group == "DCPS"))
 
## Merge the teacher-student rosters to the pilot statuses 
# Merge by teacher name first
teacher_ids_name_merge <- merge(teacher_studroster_wid,
                                manual_teacher_pilot_status_fix,
                                how = 'left',
                                by.x = 'TeacherName1',
                                by.y = 'teacher_id_alt') %>% 
  mutate(teacher_id_alt = TeacherName1)

# THen merge by teacher email
teacher_ids_email_merge <- merge(teacher_studroster_wid, 
                                 manual_teacher_pilot_status_fix, 
                                 how = 'left',
                                 by.x = 'TeacherEmail1',
                                 by.y = 'teacher_id_alt') %>% 
  mutate(teacher_id_alt = TeacherEmail1)

# Rowbind the name-based merge and the email-based merge together
teacher_studroster_wpilot_status_init = rbind(teacher_ids_name_merge, 
                                              teacher_ids_email_merge)
  
# Check that there aren't any duplicate rows
teacher_studroster_wpilot_status_init %>% 
  select(id) %>% 
  n_distinct() == nrow(teacher_studroster_wpilot_status_init)

# Check that we have we're not missing any teachers in the initial merge to the 
# student-level data. This should be empty
manual_teacher_pilot_status_fix %>%
  filter(!teacher_id_alt %in% teacher_studroster_wpilot_status_init$teacher_id_alt)

# Rowbind the ones that didn't have a teacher match
teacher_studroster_wpilot_status_dcps <- 
  rbind(teacher_studroster_wpilot_status_init, # teachers that were randomized
        teacher_studroster_wid %>% # teachers not randomized
          filter(!id %in% teacher_studroster_wpilot_status_init$id), 
        fill = TRUE)  %>% 
  # Define tx and ctrl groups based on treatment column
  mutate(teacher_pilot_status = 
           case_when(treatment == 'Pilot' ~ 'Tx', 
                     treatment == 'Not pilot' ~ 'Ctrl', 
                     treatment == 'No pilot' ~ 'Ctrl', 
                     TRUE ~ 'outside_sample')) %>% 
  select(-id, -treatment) # no longer needed this

#######################################
# Repeat process for PCS teachers 
####################################### 

### johnson was 6 teachers
johnson_rastatus = read_excel(here("data/randomization_status/johnson_rastatus.xlsx"))
johnson_rastatus_torbind = johnson_rastatus %>%
            select(teacher_id, treatment, block.id) %>%
            mutate(strata_id = sprintf("johnson_strata_%s", block.id)) %>%
            select(-block.id)

## paul
all_branch = grep("branch", unique(paul_roster$TeacherEmail1), value = TRUE)
all_phil = grep("phillips", unique(paul_roster$TeacherEmail1), value = TRUE)
### paul was just two and no strata- branch is T, phillip is control
paul_rastatus_torbind <- data.frame(teacher_id = c(all_branch, all_phil),
                                    treatment = c("Pilot", "Not pilot"),
                                    strata_id = c("paul_strata_1", "paul_strata_2"))

## friendship- reading in file with cleaned names
friendship_rastatus_torbind = read.csv(here("data/randomization_status/friendship_teacherpilotstatus_mergeable.csv")) %>%
            select(TeacherName1, treatment, block.id) %>%
            mutate(strata_id = sprintf("friendship_strata_%s", block.id)) %>%
            select(-block.id) %>%
            rename(teacher_id = TeacherName1) # doesnt have email so using teacher name


teacher_pilot_status_pcs = rbind.data.frame(johnson_rastatus_torbind,
                                           paul_rastatus_torbind,
                                           friendship_rastatus_torbind)

## do merge with rosters
## for johnson and paul we have emails
## for friendship, we have names and also need to restrict to advisory, so do that separately
teacher_studroster_wpilot_status_pj = merge(teacher_studroster %>% filter(group == "PCS" &
                                                                school_initrosters != "Friendship"),
                                             teacher_pilot_status_pcs,
                                             by.x = "TeacherEmail1",
                                             by.y = "teacher_id",
                                             all.x = TRUE) %>%
  mutate(teacher_pilot_status = 
           case_when(treatment == 'Pilot' ~ 'Tx', 
                     treatment == 'Not pilot' ~ 'Ctrl', 
                     TRUE ~ 'outside_sample')) %>% 
  select(-treatment) # 


teacher_studroster_wpilot_status_f = merge(teacher_studroster %>% filter(group == "PCS" &
                                                                           school_initrosters == "Friendship") %>%
                                          filter(grepl("College & Career Prep", ClassName)),
                                           teacher_pilot_status_pcs,
                                           by.x = "TeacherName1",
                                           by.y = "teacher_id",
                                           all.x = TRUE) %>%
  mutate(teacher_pilot_status = 
           case_when(treatment == 'Pilot' ~ 'Tx', 
                     treatment == 'Not pilot' ~ 'Ctrl', 
                     TRUE ~ 'outside_sample')) %>% 
  select(-treatment) # 

## rowbind the pcs and dcps--- addi
teacher_studroster_wpilot_status = rbind.data.frame(teacher_studroster_wpilot_status_pj %>% mutate(teacher_id = TeacherName1,
                                                                                           teacher_id_alt = TeacherName1),
                                                    teacher_studroster_wpilot_status_f %>% mutate(teacher_id = TeacherName1,
                                                                                                  teacher_id_alt = TeacherName1),
                                                    teacher_studroster_wpilot_status_dcps)


#######################################
# Merge to student-level data + check tx statuses ####
####################################### 

# Merge results (includes CA and ISA rates) from script 14 to roster data
studentlevel_wteacher_id_allperiods = merge(studentlevel_bothtimewindows_newid, 
                                            teacher_studroster_wpilot_status, 
                                            by.x = c("lea_student_id", 
                                                     "school_initrosters"),
                                            by.y = c("StudentID", 
                                                     "school_initrosters"),
                                            all.x = TRUE)


# Count the number of treatment and control teachers a student may have
treatment_status_by_student <- studentlevel_wteacher_id_allperiods %>% 
  #assume we don't want to keep the ones outside analytical sample
  filter(teacher_pilot_status != 'outside_sample') %>% 
  #select(lea_student_id, teacher_id_alt, strata_id, teacher_pilot_status) %>% 
  select(lea_student_id,teacher_id_alt, teacher_pilot_status) %>% 
  distinct() %>% 
  mutate(count_var = 1) %>%
  tidyr::spread(teacher_pilot_status, fill = 0, count_var) %>%
  group_by(lea_student_id) %>% 
  summarise(n_tx_teachers = sum(Tx), 
            n_ctrl_teachers = sum(Ctrl)) %>%
  mutate(stud_tx_status = case_when(
    (n_tx_teachers == 0 & n_ctrl_teachers == 1) ~ 'Control_only',
    n_tx_teachers == 1 & n_ctrl_teachers == 0 ~ 'Tx_only',
    n_tx_teachers > 1 & n_ctrl_teachers == 0 ~ 'Multiple_tx_teachers', 
    n_tx_teachers == 0 & n_ctrl_teachers > 1 ~ 'Multiple_control_teachers',
    TRUE ~ 'Other')) 

stopifnot(nrow(treatment_status_by_student) == length(treatment_status_by_student$lea_student_id))

# Check to see if there is a mix of control + treatment
# Looks like there's no cross between 
table(treatment_status_by_student$stud_tx_status)

# Merge back to teacher names for clustered SE
tx_status_by_student_wcluster_id_init = 
  merge(treatment_status_by_student, 
        studentlevel_wteacher_id_allperiods %>% 
          filter(teacher_pilot_status != 'outside_sample') %>% 
          select(lea_student_id,teacher_id_alt, strata_id, N_students_taught) %>% 
          distinct(), 
        how = 'left',
        by = 'lea_student_id')

# Update cluster (teacher) and strata data, 
# for those who have multiple of either, rename to multiple
tx_status_by_student_wcluster_id = 
  tx_status_by_student_wcluster_id_init %>%
  mutate(cluster_id = 
           case_when(grepl('only', stud_tx_status) ~ teacher_id_alt, 
                     grepl('Multiple', stud_tx_status) ~ stud_tx_status)) %>% 
  select(-n_tx_teachers, -n_ctrl_teachers, -strata_id,
         -teacher_id_alt) %>%
  distinct()

## check duplicates - see that they stem from cases
## of multiple_tx_teachers and multiple_control
## which makes sense since duplicatse comes from
## different N students for those teachers 
## leaving in but should get filtered out later when 
## we remove multiple assignments from analytic sample
duplicates_postidmerge = tx_status_by_student_wcluster_id %>%
                filter(lea_student_id %in% (tx_status_by_student_wcluster_id %>%
                              group_by(lea_student_id) %>%
                              filter(n() > 1) %>%
                              pull(lea_student_id)))

table(duplicates_postidmerge$cluster_id)



# Merge back the strata/treatment statuses data to results data
studentlevel_bothtimewindows_clusterid =
  merge(studentlevel_bothtimewindows_newid,
        tx_status_by_student_wcluster_id, 
        how = 'left', 
        by= 'lea_student_id', 
        all.x = TRUE) %>%
  distinct() 

## there are still duplicates for those with multiple tx or cont
## teachers but checking to make sure that none ofthe single assignments
## are duplicated
duplicates_postoutmerge = studentlevel_bothtimewindows_clusterid %>%
            filter(!grepl("Multiple", stud_tx_status) &
                  lea_student_id %in% 
                    (studentlevel_bothtimewindows_clusterid %>%
                       group_by(lea_student_id) %>%
                       filter(n() > 1) %>%
                       pull(lea_student_id)))

stopifnot(nrow(duplicates_postoutmerge) == 0) # checking that all 


#######################################
# Merge in student demographics ####
#######################################

stud_dems = fread(here('data/analysis_data/osse_data_formatching_withdem_all6_qlik.csv')) %>%
  distinct()

# When we originally did this, we said to merge on the latest SY data we have first
# all duplicates not in multiple are from people changing schools
## so we drop the school name since using school where they attendended
## at time of randomization (replacement for the school_initrosters)
stud_dem_merge_SY1920 <- 
  merge(studentlevel_bothtimewindows_clusterid, 
        stud_dems %>% filter(school_year == 'SY1920') %>% select(-enrollment_date,
                                                                 -withdrawal_date,
                                                                 -school_name, -ward), 
        how = 'left',
        by = c('usi', 'lea_student_id'), 
        all.x = TRUE) %>%
  distinct() 

# duplicates are from people changing schools 
# there was one duplicated due to ward but we're not using that covar
stopifnot(nrow((stud_dem_merge_SY1920 %>% filter(!grepl("Multiple", stud_tx_status)) %>% group_by(usi) %>% filter(n() >1))) == 0)

# IDs that didn't make the initial SY1920 merge
# using gender since confirmed no NAs in stud_dems data
stud_missing_dems <- stud_dem_merge_SY1920 %>% 
  filter(is.na(gender)) %>% select(usi) %>% pull()

# re-merge based on SY1819
stud_dem_merge_SY1819 <-
  merge(studentlevel_bothtimewindows_clusterid %>% 
          filter(usi %in% stud_missing_dems),
        stud_dems %>% filter(school_year == 'SY1819')  %>% select(-enrollment_date,
                                                                  -withdrawal_date,
                                                                  -school_name, 
                                                                  -ward), 
        how = 'left', 
        by = c('usi', 'lea_student_id')) 

crosswalk_ids  = fread(here("data/raw/hash_realid_crosswalk.csv"))
replace_usi = crosswalk_ids %>% filter(hashed_id == "c79d842dda1820629cefa90ead816118b9551fc04ab112da4eda987c2c7181c1")

# rowbind the data with dems from 1819 and 1920
stud_level_wdems <- rbind(stud_dem_merge_SY1920 %>% 
                            filter(!usi %in% stud_missing_dems), 
                          stud_dem_merge_SY1819) %>% 
  # seemed easy enough to do a manual edit for this one student
  mutate(middle_name = 
           ifelse(usi == replace_usi$usi & middle_name == '-', 
                  'K', middle_name))  %>% 
  distinct()  



##########################################
# Students' home language from msg data 
##########################################
tt_messaging_data = fread('data/analysis_data/translated_msgs_wrole_ids_02032021.csv')


tt_language <- tt_messaging_data %>%
  filter(broad_type == 'school_sent') %>%
  select(usi, lea_student_id, receiver_full_name, language) %>% 
    distinct() %>%
  filter(!is.na(usi)) %>% 
  mutate(usi = as.character(usi))

## group by student and paste together their languages
## then, code based on presence in any
tt_language_bystud = tt_language %>%
          group_by(usi) %>%
          summarise(all_lang = paste(unique(language), collapse = "; ")) %>%
          ungroup() 

## seems that all of them except for one english;french
## are english spanish
table(tt_language_bystud$all_lang)

                                                 
stud_level_wdems_wlang <- merge(stud_level_wdems %>%
                                  mutate(usi = as.character(usi)), 
                                tt_language_bystud, 
                                how = 'left', 
                                on = c('usi', 'lea_student_id'),
                                all.x = TRUE) %>%
  rename(home_language_raw = all_lang)



##########################################
# Code covariates to form used in regression
##########################################

covars_touse <- c("home_language_raw",
                  "ell",
                  "race",
                  "grade",
                  "at-risk") 

lapply(stud_level_wdems_wlang[, ..covars_touse], function(x) table(x, useNA = 'always'))


stud_level_wdems_wlang <- stud_level_wdems_wlang %>%
  
          ## code as spanish if any spanish
          mutate(derived_homelang = case_when(grepl("Spanish", home_language_raw) ~ "Spanish",
                                              grepl("English", home_language_raw) ~ "English",
                                              TRUE ~ "Other or missing"),
                 derived_homelang_morecat = case_when(grepl("Spanish", home_language_raw) ~ "Spanish",
                                                      grepl("English", home_language_raw) ~ "English",
                                                      is.na(home_language_raw) ~ "Missing",
                                                      TRUE ~ "Other"),
                 derived_ell = ell == "YES",
                 derived_race = case_when(race == "BL" ~ "Black",
                                          race == "HI" ~ "Hispanic",
                                          race == "WH" ~ "White",
                                          TRUE ~ "Other"),
                 grade_numeric = as.numeric(grade),
                 
                 ## coding grade as +1 if SY1819 dem data
                 derived_grade =  case_when(school_year == "SY1819" ~ grade_numeric +1,
                                            TRUE ~ grade_numeric),
                 derived_atrisk = `at-risk` == "YES",
                 derived_isfemale = ifelse(gender %in% c("F", "Female"), TRUE, FALSE),
                 ## currently only charter is johnson but update if needed
                 derived_is_charter = ifelse(school_initrosters %in% c("Friendship", "Paul"), TRUE, FALSE),
                 is_spring_sem = ifelse(school_initrosters %in% c("Friendship", "Paul", "Johnson"), TRUE, FALSE))


#######################################
# Add atleast_two_msgs_received flag fall_sem_only ####
#######################################
# We will define “compliers” as students
# whose parents received at least two messages from a teacher, which reflects use beyond the
# initial introductory message most teachers sent at the training session.


# Get messaging data from older scripts 
# filtered through end of semester + teachers only
msg_data <- tt_messaging_data %>% #msg data from earlier
  filter(broad_type == 'school_sent' &
         school_merge %in% c('CHEC', 'Anacostia', 'Dunbar') &
         role == 'Teacher' &
         date_dt_corrected <= as.Date("2020-01-24")) %>%
  select(usi, lea_student_id, school_merge, role, 
         sender_full_name, sender_role_id, date_dt_corrected,
         sms_delivery_status, email_delivery_status, content) %>% 
  mutate(activation_date = case_when(
                      grepl("CHEC", school_merge) ~ as.Date("2019-08-29"),
                      grepl("Ana", school_merge) ~ as.Date("2019-09-12"),
                      grepl("Dunbar", school_merge) ~ as.Date("2019-09-11")),
        two_weeks_post_activation = activation_date + days(14), 
        delivered = ifelse(sms_delivery_status == 'delivered'|email_delivery_status == 'delivered', 
                       1, 0))

# Check delivery flags
msg_data %>% 
  select(sms_delivery_status, email_delivery_status, delivered) %>% 
  distinct() #blanks resulting from type of msg sent, i.e. if only sms sent, email will be blank


# teacher info from earlier in this current script
# but filtered for Tx + DCPS only
tx_dcps_teachers <- teacher_studroster_wpilot_status %>% 
  filter(teacher_pilot_status == 'Tx' &
           group == 'DCPS') %>% 
  select(TeacherEmail1, TeacherName1, teacher_id, teacher_id_alt, 
         school_initrosters) %>%
  distinct()

# Merge tx teachers to messaging data
msg_data_tx_teachers <- merge(msg_data, 
                              tx_dcps_teachers, 
                              how = 'left', 
                              by.x = 'sender_full_name', 
                              by.y = 'TeacherName1', 
                              all.x = TRUE) # want all.x to check what didn't merge

# Manual visual check that the names are a result of not being a tx teacher 
# and not due to misspellings
# Note: sample of obviously will vary based on timing of data, 
# and teacher compliance, i.e. potentially never sent a text but still Tx teacher
# They will be taken care of once we merge to rest of dataset

#View(msg_data_tx_teachers %>% filter(is.na(teacher_id)) %>% 
#       select(sender_full_name, school_merge) %>% distinct())
tx_dcps_teachers %>% select(TeacherName1, school_initrosters) %>% 
  filter(!TeacherName1 %in% msg_data$sender_full_name)


# Flag based on activation date start through end of semester
activation_date_msg_count <- msg_data_tx_teachers %>% 
  filter(!is.na(teacher_id) &
           date_dt_corrected >= activation_date &
           delivered == 1) %>%
  group_by(usi) %>% 
  summarise(n_msgs = n()) %>% 
  mutate(atleast_two_msgsreceived_activationdate_init = ifelse(n_msgs >= 2, 1, 0),
         usi = as.character(usi)) 

# Flag based on Oct start through end of semester
oct_start_msg_count <- msg_data_tx_teachers %>% 
  filter(!is.na(teacher_id) &
           date_dt_corrected >= as.Date("2019-10-01") & 
           delivered == 1) %>%
  group_by(usi) %>% 
  summarise(n_msgs = n()) %>% 
  mutate(atleast_two_msgsreceived_octstart_init = ifelse(n_msgs >= 2, 1, 0),
         usi = as.character(usi)) 

# Two week post activation flag
two_wk_activation_date_msg_count <- msg_data_tx_teachers %>% 
  filter(!is.na(teacher_id) &
           date_dt_corrected >= activation_date & 
           date_dt_corrected <= two_weeks_post_activation &
           delivered == 1) %>%
  group_by(usi) %>% 
  summarise(n_msgs = n()) %>% 
  mutate(atleast_two_msgsreceived_activationdate_2weeks_init = ifelse(n_msgs >= 2, 1, 0),
         usi = as.character(usi)) 

# Get the unique usis in the messaging data
msg_usis <- msg_data_tx_teachers %>% 
  mutate(usi = as.character(usi)) %>% 
  select(usi) %>% 
  distinct()

flags_by_usi <- Reduce(function(x, y) 
                       merge(x, y, how = left, by = 'usi', all=TRUE), 
                       list(msg_usis, 
                            activation_date_msg_count,
                            oct_start_msg_count, 
                            two_wk_activation_date_msg_count)) 

# Check that the numbers match
table(flags_by_usi$atleast_two_msgsreceived_activationdate_init)
table(activation_date_msg_count$atleast_two_msgsreceived_activationdate_init)

#######################################
# Merge to data ####
#######################################
# stud_level_wflag_init <- merge(stud_level_wdems_wlang,
#                                activation_date_msg_count %>% 
#                                  select(-n_msgs),
#                                how = 'left', 
#                                on = 'usi', 
#                                all.x = TRUE)

stud_level_wflag <- merge(stud_level_wdems_wlang %>%
                            select(-atleast_two_msgreceived),
                          flags_by_usi %>% 
                            select(-n_msgs, -n_msgs.x, -n_msgs.y),
                          how = 'left', 
                          on = 'usi', 
                          all.x = TRUE) %>%
  # For those with a treatment but missing info, fill in with 0 since they wouldn't
  # be included in the earlier summarization
  mutate(atleast_two_msgsreceived_activationdate_fallsem = 
           ifelse(is.na(atleast_two_msgsreceived_activationdate_init),
                  0, 
                  atleast_two_msgsreceived_activationdate_init),
         atleast_two_msgsreceived_octstart_fallsem = 
           ifelse(is.na(atleast_two_msgsreceived_octstart_init),
                  0, 
                  atleast_two_msgsreceived_octstart_init),
         atleast_two_msgsreceived_twoweeks = 
           ifelse(is.na(atleast_two_msgsreceived_activationdate_2weeks_init),
                  0, 
                  atleast_two_msgsreceived_activationdate_2weeks_init))


#######################################
# Write data for evaluation results ####  
#######################################
file_to_write = stud_level_wflag %>%
  select(-atleast_two_msgsreceived_activationdate_init, -atleast_two_msgsreceived_octstart_init, 
         -atleast_two_msgsreceived_activationdate_2weeks_init)

write.csv(file_to_write, 
          'data/rct_datasets/dcps_pcs_studlevel_dems_clusterid.csv', 
          row.names = FALSE)






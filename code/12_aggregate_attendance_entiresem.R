


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
library(readxl)

# names of raw data aggregated from day -> period level
RAW_DATA_AGG1 = "aggregation_stdhs.csv"
RAW_DATA_AGG2 = "aggregation_stdhscp.csv"

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

## load info with schools and
## create school-specific two-weeks after
## activation dates
### CHEC: 08.29
### Anacostia: 09.12
### Dunbar: 09.11

anonymized_schoolinfo = fread(here("data/raw/anonymizedattend_3dcps.csv")) %>% 
            select(hashed_id, final_status, 
                   school_initrosters,
                   appears_multiple,
                   missing_osse,
                   contains("atleast")) %>%
            distinct() %>%
            mutate(is_joined_after_initRA = ifelse(school_initrosters == "", TRUE, 
                    FALSE),
                   start_tally_schoolspecific = case_when(
                    grepl("CHEC", school_initrosters) ~ as.Date("2019-08-29"),
                    grepl("Ana", school_initrosters) ~ as.Date("2019-09-12"),
                    grepl("Dunbar", school_initrosters) ~ as.Date("2019-09-11")),
                  end_tally_schoolspecific = start_tally_schoolspecific + days(14))

sprintf("There are %s students overall in attendance df for schools,
        %s students once we filter to students present on rosters at time of RA",
        length(unique(anonymized_schoolinfo$hashed_id)),
        length(unique(anonymized_schoolinfo$hashed_id[!anonymized_schoolinfo$is_joined_after_initRA])))

## left join onto attendance daily 
### agg1 is the one where presence at period level occurs if any present
### during that period (due to multiple courses on same date/period)
### agg2 is the one that double counts multiple courses on same 
### date/period -- no that small diff at day level; dont yet know impact 
### on window-level summary stat
agg_1 <- fread(sprintf("data/raw/%s", RAW_DATA_AGG1)) %>%
        left_join(anonymized_schoolinfo,
                  by = c("student_id" = "hashed_id")) %>%
        select(-V1) %>% # remove rownum
        mutate(is_present = ifelse(pfinal == 1, TRUE, FALSE)) %>%
        arrange(student_id, Date) 
agg_2 <- fread(sprintf("data/raw/%s", RAW_DATA_AGG2)) %>%
  left_join(anonymized_schoolinfo,
            by = c("student_id" = "hashed_id")) %>%
  select(-V1) %>% # remove rownum
  mutate(is_present = ifelse(pfinal == 1, TRUE, FALSE)) %>%
  arrange(student_id, Date) 

write.csv(agg_1, 
          'data/rct_datasets/agg1.csv', 
          row.names = FALSE)

clean_data_dcps <- function(df){
  
  # first, construct primary outcome of 10-01
  # through the end of the semester-- standard across schools
  ## filter  to relevant time window 
  df_fullsem <- df %>%
    ## filter to 10-01 or later and ending at end of sem
    filter(Date >= as.Date("2019-10-01") &
          Date <= as.Date("2020-01-24")) 
  
  ## then, within that range, running tally of school
  ## attendance days
  df_fullsem$within_stud_daycount <- as.numeric(ave(df_fullsem$student_id, 
                                            df_fullsem$student_id, 
                                            FUN=seq_along)) 
  
  ## then, create different indicators
  ### 1. running sum of present days (absent is converse)
  ### 2. isa (present days/school days)
  ### 3. abs prop (absent days/school days)
  ### 4. chronic absenteeism (abs perc >= 0.1)
  df_fullsem_wc = df_fullsem %>%
    group_by(student_id) %>%
    mutate(cumulsum_present = cumsum(is_present), 
           cumulsum_absent = within_stud_daycount-cumulsum_present,
           isa = cumulsum_present/within_stud_daycount,
           abs_prop = cumulsum_absent/within_stud_daycount,
           is_ca = ifelse(abs_prop >= 0.1, TRUE, FALSE)) 
  
  ## finally, take status on last observed day (most its 01.24; earlier
  ## for those who leave)
  df_eos <- df_fullsem_wc %>%
    group_by(student_id) %>%
    arrange(desc(Date), .by_group = TRUE) %>%
    slice(1) 
  
  # repeat for two weeks following activation
  ## filtering
  df_twoweek <- df %>%
    ## filter using school specific tallies
    filter(Date >= start_tally_schoolspecific &
             Date <= end_tally_schoolspecific) 
  
  ## then, within that range, running tally of school
  ## attendance days
  df_twoweek$within_stud_daycount <- as.numeric(ave(df_twoweek$student_id, 
                                                df_twoweek$student_id, 
                                                FUN=seq_along)) 
  
  
  ## then, create various indicators
  df_twoweek_wc <- df_twoweek %>%
    group_by(student_id) %>%
    mutate(cumulsum_present_2week = cumsum(is_present), 
           cumulsum_absent_2week = within_stud_daycount-cumulsum_present_2week,
           isa_2week = cumulsum_present_2week/within_stud_daycount,
           abs_prop_2week = cumulsum_absent_2week/within_stud_daycount,
           is_ca_2week = ifelse(abs_prop_2week >= 0.1, TRUE, FALSE)) 
  
  df_eo2week = df_twoweek_wc %>%
    group_by(student_id) %>%
    arrange(desc(Date), .by_group = TRUE) %>%
    slice(1) 
 
  ### make sure we didn't lose or gain ids
  stopifnot(length(unique(df_eos$student_id)) == 
              length(unique(df_fullsem_wc$student_id))) 
  
  stopifnot(length(unique(df_eo2week$student_id)) == 
              length(unique(df_twoweek_wc$student_id))) 
  
  ## return a list--- full daylevel count,
  ## end of sem status, two-week day level count (begins before sem starts)
  ## end of two week status
  return(list(stud_day = df_fullsem_wc, stud_eoy = df_eos,
              stud_day_twoweek = df_twoweek_wc,
              stud_eo2week = df_eo2week))
}

#######################################
# Clean and construct measures
#######################################

## first perform on agg1
agg1_clean = clean_data_dcps(agg_1)

## then perform on agg2
agg2_clean = clean_data_dcps(agg_2)

## first merge end of sem data to see how statuses
## differ
colnames(agg2_clean$stud_eoy) <- sprintf("%s_cp",
                           colnames(agg2_clean$stud_eoy))
bothagg_eos = merge(agg1_clean$stud_eoy,
                    agg2_clean$stud_eoy,
                    by.x = "student_id",
                    by.y = "student_id_cp") %>%
              mutate(is_diff_isa = ifelse(isa != isa_cp, TRUE, FALSE),
                     is_diff_ca = ifelse(is_ca != is_ca_cp, TRUE, FALSE))



## only 4 differ for binary CA so take one
## that shows is_present for any of the period's courses (agg1)
table(bothagg_eos$is_diff_isa)
table(bothagg_eos$is_diff_ca)

#######################################
# Visualize: DCPS 
#######################################

## create tx color map
tx_colmap = c("T: Teachers+Admins" = treatment_col,
  "C: Admins only" = control_col,
  "Groups combined" = combined_col) 

## construct analytic sample as tx control
analytic_samp_eoy = agg1_clean$stud_eoy %>%
          filter(final_status %in% c("Tx only", "Control only") &
                  !is_joined_after_initRA)

analytic_samp_2week = agg1_clean$stud_eo2week%>%
  filter(final_status %in% c("Tx only", "Control only") &
           !is_joined_after_initRA) %>% 
  
  # filter out students who attrited from school
  # before start of semester-long window (since we dont)
  # observe semester-long attendance outcomes for them
  # and want to keep analytic sample consistent
  filter(student_id %in% analytic_samp_eoy$student_id)

stopifnot(identical(analytic_samp_eoy$student_id,
                    analytic_samp_2week$student_id))





#######################################
# Repeat process for PCS schools
# currently including Friendship code
# but commenting out
#######################################


## Load in spring-schools daylevel data
springschool_daylevel = fread(here("data/raw/DayLevelSpring2020.csv"))

## first, convert date to datetime and
## define start and end of the semester
## friendship- https://www.friendshipschools.org/wp-content/uploads/2019/06/FPCS-Calendar-SY-2019-20-FINAL-1.pdf
## start of sem - jan 22
## end of sem- may 29th (deviated from claendar - here https://www.friendshipschools.org/news-list/)
## start of account activation (when training occurred)- march 5th



## johnson is dcps so- https://dcps.dc.gov/sites/default/files/dc/sites/dcps/publication/attachments/2019-2020_School_Year_Calendar-updated_04-29-20.pdf
## start - jan 29th
## end - may 29th
## start of account activation- march 5th

## paul 
## start- jan 27
## end - couldnt find end date so assuming may 29th
## start of account activation - 1-30 (some trained later)




## augmenting data with the different time windows
springschool_daylevel <- springschool_daylevel %>%
  mutate(date_dt = as.Date(date, 
                           format = "%m/%d/%Y"),
         
         ## coding dates for semester-long window
         date_startsem = case_when(grepl("Friendship", school) ~ as.Date("2020-01-22"),
                                   grepl("Paul", school) ~ as.Date("2020-01-27"),
                                   grepl("Johnson", school) ~ as.Date("2020-01-29")),
         date_endsem = as.Date("2020-05-29"),
         
         ## coding dates for 2 weeks following account window
         start_tally_schoolspecific = case_when(grepl("Friendship", school) ~ as.Date("2020-03-05"),
                                                grepl("Paul", school) ~ as.Date("2020-03-05"),
                                                grepl("Johnson", school) ~ as.Date("2020-01-30")),
         end_tally_schoolspecific = start_tally_schoolspecific + days(14)) %>%
  rename(student_id = studentid,
         Date = date_dt) %>% # rename to change fewer things in code from script 14
  mutate(is_present = ifelse(present == "Present", TRUE, FALSE)) 


## similar function but only agg1
clean_data_pcs <- function(df){
  
  # first, construct primary outcome of semester-long window
  ## in the pcs case this varies by school
  df_fullsem <- df %>%
    ## filter to 10-01 or later and ending at end of sem
    filter(Date >= date_startsem &
             Date <= date_endsem) 
  
  ## then, within that range, running tally of school
  ## attendance days for each student
  df_fullsem$within_stud_daycount <- as.numeric(ave(df_fullsem$student_id, 
                                                    df_fullsem$student_id, 
                                                    FUN=seq_along)) 
  
  ## then, create different indicators
  ### 1. running sum of present days (absent is converse)
  ### 2. isa (present days/school days)
  ### 3. abs prop (absent days/school days)
  ### 4. chronic absenteeism (abs perc >= 0.1)
  df_fullsem_wc = df_fullsem %>%
    group_by(student_id) %>%
    mutate(cumulsum_present = cumsum(is_present), 
           cumulsum_absent = within_stud_daycount-cumulsum_present,
           isa = cumulsum_present/within_stud_daycount,
           abs_prop = cumulsum_absent/within_stud_daycount,
           is_ca = ifelse(abs_prop >= 0.1, TRUE, FALSE)) 
  
  ## finally, take status on last observed day (most it should be end of sem; earlier
  ## for those who leave)
  df_eos <- df_fullsem_wc %>%
    group_by(student_id) %>%
    arrange(desc(Date), .by_group = TRUE) %>%
    slice(1) 
  
  # repeat for two weeks following activation
  ## filtering
  df_twoweek <- df %>%
    ## filter using school specific tallies
    filter(Date >= start_tally_schoolspecific &
             Date <= end_tally_schoolspecific) 
  
  ## then, within that range, running tally of school
  ## attendance days
  df_twoweek$within_stud_daycount <- as.numeric(ave(df_twoweek$student_id, 
                                                    df_twoweek$student_id, 
                                                    FUN=seq_along)) 
  
  
  ## then, create various indicators
  df_twoweek_wc <- df_twoweek %>%
    group_by(student_id) %>%
    mutate(cumulsum_present_2week = cumsum(is_present), 
           cumulsum_absent_2week = within_stud_daycount-cumulsum_present_2week,
           isa_2week = cumulsum_present_2week/within_stud_daycount,
           abs_prop_2week = cumulsum_absent_2week/within_stud_daycount,
           is_ca_2week = ifelse(abs_prop_2week >= 0.1, TRUE, FALSE)) 
  
  df_eo2week = df_twoweek_wc %>%
    group_by(student_id) %>%
    arrange(desc(Date), .by_group = TRUE) %>%
    slice(1) 
  
  ### make sure we didn't lose or gain ids
  stopifnot(length(unique(df_eos$student_id)) == 
              length(unique(df_fullsem_wc$student_id))) 
  
  stopifnot(length(unique(df_eo2week$student_id)) == 
              length(unique(df_twoweek_wc$student_id))) 
  
  ## return a list--- full daylevel count,
  ## end of sem status, two-week day level count (begins before sem starts)
  ## end of two week status
  return(list(stud_day = df_fullsem_wc, stud_eoy = df_eos,
              stud_day_twoweek = df_twoweek_wc,
              stud_eo2week = df_eo2week))
}

#######################################
# Clean and construct measures for spring-sem schools
#######################################

## first perform on agg1
agg1_clean_pcs = clean_data_pcs(springschool_daylevel)


## look at diff between charter school and dcps fall sem cols
setdiff(colnames(agg1_clean$stud_eoy), colnames(agg1_clean_pcs$stud_eoy))


## cols to code in PCS data:
## final_status, school_initrosters, appears_multiple, is_joined_after_initRA

## ones to leave blank for now since come from elsewhere and not clear
## we'll do compliance analysis with PCS
## atleast_one_msgsent, atleast_one_msgreceived, atleast_two_msgreceived,


## paul
paul_roster = read.csv(here("data/randomization_status/Paul Rosters at Randomization - 2019-09-18 - 2019-09-18.csv"))

## on that original roster,
## aggregate to student level and construct indicator for 
## tx, control, neither
paul_roster_studlevel = paul_roster %>%
  group_by(StudentID) %>%
  summarise(all_teachers = paste(TeacherName1, collapse = ";"),
            school_initrosters = "Paul") %>%
  ungroup() 


## check for branch and phillips- make sure only one
all_branch = grep("Branch", unique(paul_roster$TeacherName1), value = TRUE)
all_phil = grep("Phillips", unique(paul_roster$TeacherName1), value = TRUE)

## construct tx status
paul_roster_studlevel = paul_roster_studlevel %>%
  mutate(final_status = case_when(grepl(all_branch, all_teachers) & grepl(all_phil, all_teachers) ~ "Mixed",
                                  grepl(all_branch, all_teachers) ~ "Tx only",
                                  grepl(all_phil, all_teachers) ~ "Control only",
                                  TRUE ~ "Outside analytic"))

## look at presence in original roster to make sure high outside analytic is true reflection
students_branch = unique(paul_roster$StudentID[paul_roster$TeacherName1 == all_branch])
students_phil = unique(paul_roster$StudentID[paul_roster$TeacherName1 == all_phil])
students_both = intersect(students_branch, students_phil)
stopifnot(length(students_both) == table(paul_roster_studlevel$final_status)["Mixed"])


## create a paul data based on attendance data
## taking the union of students in two week and eoy (ends up we can use the student day)
inclusive_students = intersect(unique(agg1_clean_pcs$stud_eo2week$student_id), unique(agg1_clean_pcs$stud_eoy$student_id))
check_studday = setdiff(inclusive_students, 
                        agg1_clean_pcs$stud_day$student_id) ## see that if we want most inclusive set use distinct stud_day

paul_status = agg1_clean_pcs$stud_day %>% filter(grepl("Paul", school)) %>% select(student_id) %>% distinct() %>%
  left_join(paul_roster_studlevel %>% select(StudentID, final_status, school_initrosters),
            by = c("student_id" = "StudentID")) %>%
  mutate(is_joined_after_initRA = ifelse(is.na(school_initrosters), TRUE, FALSE)) 



## johnson
## from emails - we initially did advisory but final RA status
## was the 6 math and science teachers; copied that output 
## from the notebook
johnson_rastatus = read_excel(here("data/randomization_status/johnson_rastatus.xlsx"))
johnson_roster = read.csv(here("data/randomization_status/Johnson MS Rosters for Randomization (1).csv"))

johnson_txteach = unique(johnson_rastatus$teacher_id[johnson_rastatus$treatment == "Pilot"])
johnson_controlteach = unique(johnson_rastatus$teacher_id[johnson_rastatus$treatment == "Not pilot"])

## aggregate and code matches
johnson_roster_studlevel = johnson_roster %>%
  group_by(StudentID) %>%
  summarise(all_teachers = paste(TeacherEmail1, collapse = "; "),
            school_initrosters = "Johnson") %>%
  ungroup() %>%
  mutate(final_status =  case_when(grepl(paste(johnson_txteach, collapse = "|"), all_teachers) & 
                                     grepl(paste(johnson_controlteach, collapse = "|"), all_teachers) ~ "Mixed",
                                   grepl(paste(johnson_txteach, collapse = "|"), all_teachers) ~ "Tx only",
                                   grepl(paste(johnson_controlteach, collapse = "|"), all_teachers) ~ "Control only",
                                   TRUE ~ "Outside analytic"))

johnson_status = agg1_clean_pcs$stud_day %>% filter(grepl("Johnson", school)) %>% select(student_id) %>% distinct() %>%
  left_join(johnson_roster_studlevel %>% select(StudentID, final_status, school_initrosters),
            by = c("student_id" = "StudentID")) %>%
  mutate(is_joined_after_initRA = ifelse(is.na(school_initrosters), TRUE, FALSE)) 

## add friendship
##friendship- commented out for now
# due to advisory issue
friendship_rastatus = read.csv(here("data/randomization_status/friendship_teacherpilotstatus.csv"))

## original roster at time of randomization
friendship_roster = read.csv(here("data/randomization_status/Friendship Rosters at Randomization - 2019-08-28 - 2019-08-29.csv"))

## new roster provided
friendship_roster_doublecheck = read.csv(here("data/randomization_status/Student roster per course - 2019-08-26 - 2020-06-30.csv"))

friendship_txteach_init = unique(friendship_rastatus$teacher[friendship_rastatus$treatment == "Pilot"])
friendship_controlteach_init = unique(friendship_rastatus$teacher[friendship_rastatus$treatment == "Not pilot"])

## within each vector, reorder the names since roster has first name last name, the vectors have
## last name, first name
split_names_tx <- lapply(as.character(friendship_txteach_init), function(x) strsplit(x, ", "))
friendship_txteach <- unlist(lapply(split_names_tx, function(x) paste(x[[1]][2],  x[[1]][1])))
split_names_control <- lapply(as.character(friendship_controlteach_init), function(x) strsplit(x, ", "))
friendship_controlteach <- unlist(lapply(split_names_control, function(x) paste(x[[1]][2],  x[[1]][1])))

## make sure all appear on rosters
### Katherine payne missing but none similar appears
### all control teachers appear
setdiff(friendship_txteach, unique(friendship_roster$TeacherName1))
setdiff(friendship_txteach, unique(friendship_roster_doublecheck$TeacherName1)) # also missing from newer roster shared
setdiff(friendship_controlteach, unique(friendship_roster$TeacherName1))
intersect(friendship_txteach, unique(friendship_roster$TeacherName1))
intersect(friendship_controlteach, unique(friendship_roster$TeacherName1))
# 

## merge them to the course prep section
friendship_tx_control = rbind.data.frame(data.frame(TeacherName1 = friendship_txteach,
                                                    teacher_lastfirst = friendship_txteach_init,
                                                    tx_status = rep("Treatment", length(friendship_txteach))),
                                         data.frame(TeacherName1 = friendship_controlteach,
                                                    teacher_lastfirst = friendship_controlteach_init,
                                                    tx_status = rep("Control", length(friendship_controlteach)))) %>%
                  left_join(friendship_rastatus %>% select(teacher, block.id, treatment),
                            by = c("teacher_lastfirst" = "teacher")) 

## create a version to write to use for later that has the Last name, first name syntax
write.csv(friendship_tx_control, 
          here("data/randomization_status/friendship_teacherpilotstatus_mergeable.csv"),
          row.names = FALSE)


friendship_roster_wteach = merge(friendship_roster,
                                 friendship_tx_control,
                                 by = "TeacherName1",
                                 all.x = TRUE) %>%
                      filter(grepl("College & Career Prep", ClassName)) # advisory period only

## double check to see if new roster has different courses
## but they're the same college/career prep with the tx/control teachers,
## so using the original one

## get students lost in the filter (2 students) due to no advisory period
## to add back in as outside_analytic 
students_noadv = setdiff(friendship_roster$StudentID, friendship_roster_wteach$StudentID)


friendship_roster_studlevel = rbind.data.frame(friendship_roster_wteach %>%
  group_by(StudentID) %>%
  summarise(all_teachers = paste(TeacherName1, collapse = "; "),
            school_initrosters = "Friendship") %>%
  ungroup() %>%
  mutate(final_status =  case_when(grepl(paste(friendship_txteach, collapse = "|"), all_teachers) & 
                                     grepl(paste(friendship_controlteach, collapse = "|"), all_teachers) ~ "Mixed",
                                   grepl(paste(friendship_txteach, collapse = "|"), all_teachers) ~ "Tx only",
                                   grepl(paste(friendship_controlteach, collapse = "|"), all_teachers) ~ "Control only",
                                   TRUE ~ "Outside analytic")),
  data.frame(StudentID = students_noadv, 
             all_teachers = rep("", length(students_noadv)),
             school_initrosters = rep("Friendship", length(students_noadv)),
             final_status = rep("Outside analytic", length(students_noadv)))) 

stopifnot(length(unique(friendship_roster_studlevel$StudentID)) == length(unique(friendship_roster$StudentID)))

friendship_status = agg1_clean_pcs$stud_day %>% filter(grepl("Friendship", school)) %>% select(student_id) %>% distinct() %>%
  left_join(friendship_roster_studlevel %>% select(StudentID, final_status, school_initrosters),
            by = c("student_id" = "StudentID")) %>%
  mutate(is_joined_after_initRA = ifelse(is.na(school_initrosters), TRUE, FALSE)) 

## first, rowbind paul and johnson
pcs_status = rbind.data.frame(paul_status,
                              johnson_status,
                              friendship_status)


## inner join onto attendance outcomes when creating analytic samples
## in order to drop Paul
analytic_samp_eoy_spring = agg1_clean_pcs$stud_eoy %>%
  mutate(school_initrosters = case_when(grepl("Paul", school) ~ "Paul",
                                        TRUE ~ school)) %>%
  inner_join(pcs_status, by = c("student_id", "school_initrosters")) %>% 
  filter(final_status %in% c("Tx only", "Control only") &
           !is_joined_after_initRA)

## check that rows remaining are equal to tx + control
stopifnot(nrow(analytic_samp_eoy_spring) == sum(table(pcs_status$final_status)["Control only"],
                                                table(pcs_status$final_status)["Tx only"]))

analytic_samp_2week_spring = agg1_clean_pcs$stud_eo2week%>%
  mutate(school_initrosters = case_when(grepl("Paul", school) ~ "Paul",
                                        TRUE ~ school)) %>%
  inner_join(pcs_status, by = c("student_id", "school_initrosters")) %>%
  filter(final_status %in% c("Tx only", "Control only") &
           !is_joined_after_initRA) %>% 
  
  # filter out students who attrited from school
  # before start of semester-long window (since we dont)
  # observe semester-long attendance outcomes for them
  # and want to keep analytic sample consistent
  filter(student_id %in% analytic_samp_eoy_spring$student_id)



#######################################
# Write outputs for next steps 
#######################################

## merge two student level datasets
## and then save that +2 day level as csvs 

### merged end dataset
overlap_cols  = intersect(colnames(analytic_samp_2week),
          colnames(analytic_samp_eoy))


## merge and remove overlap cols except for student id
studentlevel_bothtimewindows = merge(analytic_samp_eoy,
                                  analytic_samp_2week[,
                                   c("student_id",
                                     setdiff(colnames(analytic_samp_2week),
                                    overlap_cols))],
                                  by = "student_id",
                                  all.x = TRUE) %>%
                  filter(!is_joined_after_initRA)

## use crosswalk to merge back their original ids from the hashed ones
crosswalk_ids  = fread(here("data/raw/hash_realid_crosswalk.csv"))

## merge usi and lea_student_id
studentlevel_bothtimewindows_newid = merge(studentlevel_bothtimewindows %>%
                                            rename(hashed_id = student_id),
                                           crosswalk_ids %>%
                                          dplyr::select(hashed_id, usi, lea_student_id),
                                           by = "hashed_id",
                                          all.x = TRUE)

## write file
write.csv(studentlevel_bothtimewindows_newid, 
          here("data/rct_datasets/dcps_studlevel_foranalysis.csv"),
          row.names = FALSE) 


### add the two pcs and write data including those
overlap_cols_spring = intersect(colnames(analytic_samp_2week_spring),
                                colnames(analytic_samp_eoy_spring))
studentlevel_bothtimewindows_spring = merge(analytic_samp_eoy_spring,
                                     analytic_samp_2week_spring[,
                                                         c("student_id",
                                                           setdiff(colnames(analytic_samp_2week),
                                                                   overlap_cols))],
                                     by = "student_id",
                                     all.x = TRUE) %>%
            filter(!is_joined_after_initRA)

## rowbind pcs and non-pcs
## look at differences in cols
## key ones are: usi, lea_student_id,
## the atleast_onemsg
setdiff(colnames(studentlevel_bothtimewindows_newid),
        colnames(studentlevel_bothtimewindows_spring))
setdiff(colnames(studentlevel_bothtimewindows_spring),
        colnames(studentlevel_bothtimewindows_newid)) ## to do to reconcile:
## rename student_id -> lea_student_id (need usi)
## and add usi
## put the following colsas NA: hasehd_id (drop), atleast_one_msgreceived, 
## atleast_two_msgreceived, atleast_one_msgsent 
## get rid of extra attendance cols in spring sem
## keep hashed_id but set to NA

## read in qlik data to get crosswalk between local id and USI to add
stud_dems = fread(here('data/analysis_data/osse_data_formatching_withdem_all6_qlik.csv')) 
stud_ids_pcs = stud_dems %>%
        filter(school_year == "SY1920") %>%
        select(usi, lea_student_id) %>%
        distinct() 

studentlevel_bothtimewindows_spring_newid = studentlevel_bothtimewindows_spring %>%
            left_join(stud_ids_pcs, by = c("student_id" = "lea_student_id")) %>%
            rename(lea_student_id = student_id) %>%
            mutate(missing_osse = ifelse(is.na(usi), TRUE, FALSE)) # rename local id to lea_student_id 

## columnbinding an empty df for the other missing cols 
cols_add_pcs = c("appears_multiple", "atleast_one_msgsent",
                 "atleast_one_msgreceived", "atleast_two_msgreceived")
df_add_pcs <- data.frame(matrix(NA, nrow = nrow(studentlevel_bothtimewindows_spring_newid),
                                ncol = length(cols_add_pcs))) 
colnames(df_add_pcs) = cols_add_pcs
studentlevel_spring_withcommoncols = cbind.data.frame(studentlevel_bothtimewindows_spring_newid,
                                                      df_add_pcs)

cols_keep_pcs_dcps = intersect(colnames(studentlevel_bothtimewindows_newid),
                               colnames(studentlevel_spring_withcommoncols))

studentlevel_bothtime_allschools = rbind.data.frame(studentlevel_bothtimewindows_newid[, cols_keep_pcs_dcps],
                                                    studentlevel_spring_withcommoncols[, cols_keep_pcs_dcps])

write.csv(studentlevel_bothtime_allschools, 
          here("data/rct_datasets/dcpspcs_studlevel_foranalysis.csv"),
          row.names = FALSE) 


## subset to those columns and rowbind

#### to add later: same viz above including the PCS schools

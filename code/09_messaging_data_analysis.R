




## packages and theme

library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(scales)
library(stringr)
library(survival)
library(lubridate)

## general theme
theme_oes <- function(base_size = 24){
  theme_bw(base_size = base_size) %+replace%
    theme(
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.background = element_rect(fill = NA),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.ticks.x = element_blank(),
      axis.line = element_line(colour = "black",
                               size = 0.5),
      legend.title = element_text(size= base_size,
                                  face = "italic"),
      legend.text = element_text(size = 12),
      legend.background = element_blank()
    )
}

## pha-specific color map (change to schools)
col_map = c("School C" =  "#F9834A",
            "School B" = "#2E9AC4",
            "School A" = "#F2C446")

yes_green = "#12312b"

################################################# Load raw data ######################################3

msg_wtranslate_init = read.csv("../data/analysis_data/msgs_wdem_wtrans.csv")
osse_samp = read.csv("../data/analysis_data/analyticsamp_wosse.csv")


nonparent = Sys.getenv("MESSAGE_IDEXCLUDE")
msg_wtranslate = msg_wtranslate_init %>%
              # blank school merge is due to some row nonalignment we can figure out later (might be in orig data)
              filter(school_merge != "" & StudentID != nonparent &
                    content_upper != "NAN") %>% # blank content become NAN character for content_upper
  
              ## could also filter out ones with rawmsg_len under threshold (eg 3)
              ## e.g. id_16048 seems to just have weird special chars but when actually using the 
              ## text messages for atm we'll do preprocessing so leaving in for now
              
              mutate(date_time = as.POSIXct(as.character(date_time)),
                     school_anon = case_when(school_merge == "CHEC" ~ "School C",
                                             school_merge == "Anacostia" ~ "School A",
                                             school_merge == "Dunbar" ~ "School B",
                                             TRUE ~ "Other"),
                     ## general but only analyze outgoing messages in this so means student or parent name
                     uses_either_name = ifelse(uses_student_name == 1|
                                              uses_receiver_name == 1, 1, 0)) %>%
              dplyr::select(-withdrawal_date,
                            -enrollment_date, -overlaps_timeofRA) %>% # causes dup and fields arent relevant
              distinct()


############################################# Some checks #############################


## students multiple schools- only two based on student ID
studs_multschools = msg_wtranslate %>% group_by(StudentID) %>%
              summarise(n_schools = length(unique(school_anon))) %>%
              filter(n_schools > 1) %>% pull(StudentID) 

## from viewing, dates seem far enough apart that leaving both in
## for histogram; will contribute to values for the 
## first school by order of date and time
## for reg, similar first message
View(msg_wtranslate %>% filter(StudentID %in% studs_multschools) %>% 
       dplyr::select(StudentID,
               student_name, school_anon,
               content_upper, date_time) %>%
       arrange(student_name, date_time))

## indicator for outgoing using student or parent name correct
View(msg_wtranslate_wdedup %>% 
       filter(broad_type == "school_sent" & uses_either_name == 1) %>% 
       sample_n(20) %>% dplyr::select(content_upper, student_name, parent_full_name))


############################################# Fig 1: group by student and total count of outgoing #############################

msg_wtranslate_bystud = msg_wtranslate %>%
          filter(broad_type == "school_sent") %>%
          group_by(StudentID, sender_role) %>%
          dplyr::select(StudentID, content_upper, date_time, school_anon,
                        sender_role) %>%
          distinct() %>%
          arrange(date_time) %>%
          summarise(total_outgoing = n(),
                    school_first = school_anon[1], .groups = "drop_last") %>%
          ungroup() %>%
          mutate(school_role = sprintf("%s: %ss", 
                                       school_first, sender_role))

scaleFUN <- function(x) sprintf("%.0f", x)
my_breaks <- function(x) {
  if (max(x) < 10) c(1, 3, 5, 10) 
  else c(1, 5, 10, 20, 30, 40, 50)}

## to do: clean up box aesthetics
## see if can standardize axes more or mention verbally
ggplot(msg_wtranslate_bystud, aes(x = total_outgoing,
                                  fill = school_first)) +
  geom_histogram(bins = 10, color = "black") +
  theme_oes() +
  facet_wrap(~ school_role, scales = "free",
             ncol = 2)  +
  scale_x_continuous(labels = scaleFUN,
                     breaks = my_breaks) +
  theme_oes(base_size = 24) +
  scale_fill_manual(values = col_map) +
  guides(fill = FALSE) +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size = 16)) +
  ylab("Count of students") +
  xlab("Count of school messages to student-family dyads\n(Aug 29 2019-Jan 31 2020;\nstudents with 1+ message)") +
  scale_y_continuous(breaks = pretty_breaks(n = 3))

ggsave("../output/histogram_outgoing.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)




############################################# Fig 2 and 3: variation in uniqueness #############################

## create new mssage ids based on translated message
translated_dedup = msg_wtranslate %>% dplyr::select(content_noreceiverstudentname_translated) %>%
              distinct()

translated_dedup$id_content_noreceiverstudentname_translated = sprintf("id_%s", 1:nrow(translated_dedup))


msg_wtranslate_wdedup = merge(msg_wtranslate,
                              translated_dedup,
                              by = "content_noreceiverstudentname_translated",
                              all.x = TRUE)

## dataframe of repeated messages (non-unique id)
## use threshold of 2+ students bc others seem arbitrary
## see that some are only a few students but seems to be wierd messages
## like id_1039 for new translate ids that just reads "copy"
msgs_repeated = msg_wtranslate_wdedup %>% 
            filter(broad_type == "school_sent") %>%
            group_by(id_content_noreceiverstudentname_translated) %>% 
            arrange(date_time) %>%
            summarise(count_id = n(),
                  role = unique(sender_role), school = school_anon[1]) %>%
            filter(count_id > 1)

## create dataframe for message as repeated across students
msg_wtranslate_wdedup = msg_wtranslate_wdedup %>%
              mutate(repeated_message = ifelse(id_content_noreceiverstudentname_translated %in%
                                                 msgs_repeated$id_content_noreceiverstudentname_translated, 
                                               1, 0))

## get number of non-repeated messages by student (if all students'
## messages are repeated messages, they'll be missing from this df)
msg_wtranslate_bystud_norep = msg_wtranslate_wdedup %>%
  filter(broad_type == "school_sent" & repeated_message == 0) %>%
  group_by(StudentID, sender_role) %>%
  dplyr::select(StudentID, content_upper, date_time, school_anon,
                sender_role) %>%
  distinct() %>%
  arrange(date_time) %>%
  summarise(total_outgoing_norepeat = n()) %>%
  ungroup() 

## left join onto count of messages by students- na on total_outgoing_norepeat are 
## 0 non-repeated messages
msg_wtranslate_bystud_wuniq = merge(msg_wtranslate_bystud,
                                   msg_wtranslate_bystud_norep,
                                   by = c("StudentID", "sender_role"),
                                   all.x = TRUE) %>%
                          mutate(total_outgoing_norepeat = ifelse(is.na(total_outgoing_norepeat), 0, 
                                                                  total_outgoing_norepeat))


ggplot(msg_wtranslate_bystud_wuniq, aes(x = total_outgoing, y = total_outgoing_norepeat,
                                        color = school_first)) +
  geom_point(size = 3, alpha = 0) +
  facet_wrap(~sender_role) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 2) +
  theme_oes() +
  xlab("Total messages sent to that family/student") +
  ylab("Of those, total messages that were\nunique to that student") +
  scale_color_manual(values = col_map) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 24)) +
  labs(color = "")

ggsave("../output/totalvunique_outgoing_blank.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)


ggplot(msg_wtranslate_bystud_wuniq, aes(x = total_outgoing, y = total_outgoing_norepeat,
                                       color = school_first)) +
  geom_point(size = 3, alpha = 0.5) +
  facet_wrap(~sender_role) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", size = 2) +
  theme_oes() +
  xlab("Total messages sent to that family/student") +
  ylab("Of those, total messages that were\nunique to that student") +
  scale_color_manual(values = col_map) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 24)) +
  labs(color = "")

ggsave("../output/totalvunique_outgoing.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

## repeat process but looking at use of name in outgoing message
msg_wtranslate_bystud_usename = msg_wtranslate_wdedup %>%
  filter(broad_type == "school_sent" & uses_either_name == 1) %>%
  group_by(StudentID, sender_role) %>%
  dplyr::select(StudentID, content_upper, date_time, school_anon,
                sender_role) %>%
  distinct() %>%
  arrange(date_time) %>%
  summarise(total_outgoing_usesname = n()) %>%
  ungroup() 

msg_wtranslate_bystud_wname = merge(msg_wtranslate_bystud,
                                   msg_wtranslate_bystud_usename,
                                   by = c("StudentID", "sender_role"),
                                   all.x = TRUE) %>%
  mutate(total_outgoing_usesname = ifelse(is.na(total_outgoing_usesname), 0, 
                                          total_outgoing_usesname))

ggplot(msg_wtranslate_bystud_wname, aes(x = total_outgoing, y = total_outgoing_usesname,
                                        color = school_first)) +
  geom_point(size = 3, alpha = 0.5) +
  facet_wrap(~sender_role) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", size = 2) +
  theme_oes() +
  xlab("Total messages sent to that family/student") +
  ylab("Of those, total messages that use\nstudent or parent name") +
  scale_color_manual(values = col_map) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 24)) +
  labs(color = "")

ggsave("../output/totalvname_outgoing.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)


############################################# Fig 4: parent response #############################

## first, subset to first outgoing message to a student
## exclude undelivered messages though might want to revisit
## since part of the pathway through which statuses like 
## at risk might lead to lower response rates
## is through lower likelihood that a message is delivered
## (i need to revisit direct effect v indirect effect)
msg_wtranslate_wdedup_firstmsg = msg_wtranslate_wdedup %>%
                        mutate(end_window = date_time + days(7)) %>%
                        filter(broad_type == "school_sent" &
                              msgtofam_delivered_somemode == 1) %>%
                        group_by(StudentID) %>%
                        arrange(date_time) %>%
                        slice(1) %>%
                        ungroup() # filters to first regardless of teacher or admin

## confirm that ~50 students lost are those whose messages never were 
## delivered
never_deliv = setdiff(msg_wtranslate_wdedup$StudentID,
                      msg_wtranslate_wdedup_firstmsg$StudentID)
table(msg_wtranslate_wdedup$msgtofam_delivered_somemode[msg_wtranslate_wdedup$StudentID %in% never_deliv])

## check a few dates + end window
View(msg_wtranslate_wdedup_firstmsg %>% dplyr::select(date_time, end_window) %.% sample_n(20))

## look at students whose first message was after mid-october
## see that it's almost all dunbar that had late rollout/low
## use in general
summary(msg_wtranslate_wdedup_firstmsg$date_time)
late_msg = msg_wtranslate_wdedup_firstmsg %>% filter(date_time > ymd_hms("2019-11-01 00:00:00"))
table(late_msg$sender_role, late_msg$school_anon)
table(late_msg$final_status) # see most are outside analytic sample so maybe joined school late
View(msg_wtranslate_wdedup %>% filter(StudentID %in% (late_msg %>% pull(StudentID))) %>%
                                   arrange(StudentID, date_time)) # keep as is, for final might crosscheck against osse enrollment



sprintf("There are %s messages for %s students",
        nrow(msg_wtranslate_wdedup_firstmsg),
        length(unique(msg_wtranslate_wdedup_firstmsg$StudentID)))

## pool of possible messages that could be responses
pool_possible = msg_wtranslate_wdedup %>%
            filter(broad_type != "school_sent") %>%
            rename(parent_whoresponded = sender_full_name,
                   teacher_sentto = receiver_full_name,
                   date_time_response = date_time) # rename to make less confusing

## prep for translation versus auto coding
table(msg_wtranslate_wdedup_firstmsg$translated, msg_wtranslate_wdedup_firstmsg$language)
table(msg_wtranslate_wdedup_firstmsg$translated) # only 2 translated 

## left join all responses and then create indicator for whether 
## response is in within window
msg_wtranslate_wresp = merge(msg_wtranslate_wdedup_firstmsg,
                             pool_possible %>% dplyr::select(date_time_response,
                                                             StudentID,
                                                             teacher_sentto,
                                                             parent_whoresponded) %>%
                                            distinct(),
                             by.x = c("StudentID",
                                    "sender_full_name", # teacher/admin = sender
                                    "receiver_full_name"), # parent = receiver
                             by.y = c("StudentID",
                                      "teacher_sentto", # teacher who incoming is sent to
                                      "parent_whoresponded"), # parent who is sending things
                             all.x = TRUE) %>%
            mutate(resp_inwindow = 
                  case_when(is.na(date_time_response) ~ "Never respond to that teacher/admin",
                            date_time_response > end_window ~ "Sent back but > 7 from focal",
                            date_time_response <= end_window ~ "Responded within 7 days",
                            TRUE ~ "Other"),
                  resp_binary = ifelse(resp_inwindow == "Responded within 7 days", 1, 0),
                  swd_clean = relevel(factor(ifelse(!grepl("Level", highest_swd_level), 
                                     "No IEP",
                                     "Any IEP")), ref = "No IEP"),
                  ell_clean = ifelse(ell == "NO" | ell == "", 0,
                                     1),
                  gender_clean = ifelse(gender %in% c("F", "Female"), 
                                  "Female", 
                                ifelse(gender %in% c("M", "Male"),
                                       "Male", "Missing")),
                  atrisk_clean = ifelse(at.risk == "YES", 1, 0),
                  school_anon = relevel(factor(school_anon),
                                        ref = "School C"),
                  trans_categories = 
                  case_when(language != "English" & translated == 0 ~ "Auto-translated",
                            TRUE ~ "Native language"), # since only two written in native language combine translate == 1 
                                                        # with non-translate english
                  time_since_systart = rescale(as.numeric(difftime(date_time,
                                                ymd_hms("2019-08-26 00:00:00"),
                                                units = "days")), to = c(0, 1)),
                  topic_ref = relevel(factor(topic),
                                      ref = "Uncategorized")) # since messages sent when parnets/schools knew each other more might be diff than beg of year

table(msg_wtranslate_wresp$trans_categories, msg_wtranslate_wresp$ell_clean)

## quick model comparing demographics
dem_cols = c("swd_clean", "ell_clean", "atrisk_clean",
             "gender_clean", "school_anon", "sender_role")
msg_cols = c("repeated_message", "trans_categories", "uses_either_name",
             "time_since_systart")


## save and plot
demonly = lm(formula(sprintf("resp_binary ~ %s", paste(dem_cols, collapse = "+"))),
             data = msg_wtranslate_wresp)
textonly = lm(formula(sprintf("resp_binary ~ %s", paste(msg_cols, collapse = "+"))),
              data = msg_wtranslate_wresp)
demtext = lm(formula(sprintf("resp_binary ~ %s", paste(c(dem_cols, msg_cols), collapse = "+"))),
             data = msg_wtranslate_wresp)


## clean coef and ci
tidyreg <- function(one_obj){
  
  coef_data = as.data.frame(summary(one_obj)$coefficients)
  coef_data$varname = rownames(coef_data) 
  coef_data = coef_data %>%
            mutate(var_init = gsub("\\_clean|school\\_anon|swd", "", varname),
                   lower = Estimate - 1.96*`Std. Error`,
                   upper = Estimate + 1.96*`Std. Error`)
  return(coef_data)
  
}

demonly_tidy = tidyreg(demonly) %>%
        mutate(model = "Family dem.\n+ school")  
textonly_tidy = tidyreg(textonly) %>%
  mutate(model = "Text") 
both_tidy = tidyreg(demtext) %>%
  mutate(model = "Both")

all_reg_init = rbind.data.frame(demonly_tidy,
                           textonly_tidy,
                           both_tidy) %>%
          filter(!grepl("Intercept", var_init)) 

## create renaming dictionary
all_reg = all_reg_init %>%
          mutate(var_clean1  = 
              gsub("topic\\_ref", "Msg. label: ", #in case we add back in the hand labels
                   var_init),
              var_clean = 
              case_when(grepl("time\\_since", var_clean1) ~ "Days into SY (rescaled)",
                        grepl("Teacher", var_clean1) ~ "Teacher sent (ref: admin)",
                        grepl("either\\_name", var_clean1) ~ "Uses parent or stud.name",
                        grepl("Native", var_clean1) ~ "Native lang.(ref: auto-translate)",
                        grepl("ell", var_clean1) ~ "English language learner",
                        grepl("Male", var_clean1) ~ "Male stud.(ref: female)",
                        grepl("atrisk", var_clean1) ~ "At-risk funding cat.",
                        grepl("repeated", var_clean1) ~ "Msg. repeated across students",
                        grepl("genderMissing", var_clean1) ~ "Gen. missing (ref: female)",
                        TRUE ~ var_clean1),
              color_text = 
              case_when(
                grepl("Msg. label", var_clean) ~ "#8B7E66",
                var_init %in% msg_cols ~ yes_green,
                TRUE ~ "#ff4249")) %>% # didnt work i think bc of facets
              arrange(desc(Estimate)) 

## next: 
all_reg %>% dplyr::select(var_clean, color_text)


ggplot(all_reg, aes(x = reorder(var_clean, Estimate), y = Estimate,
                    color = model)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper,
                    color = model)) +
  facet_wrap(~factor(model,
                     levels = c("Family dem.\n+ school",
                                "Text",
                                "Both"),
                     ordered = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("") +
  ylab("Estimate(linear reg; positive =\nhigher chance of response)") +
  coord_flip() +
  theme_oes() +
  scale_color_manual(values = c("white", "red", "white")) +
  guides(color = FALSE) 

ggsave("../output/resp_dem.pdf",
       plot = last_plot(),
       device = "pdf",
       width= 12,
       height = 8)

ggplot(all_reg, aes(x = reorder(var_clean, Estimate), y = Estimate,
                    color = model)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper,
                    color = model)) +
  facet_wrap(~factor(model,
                     levels = c("Family dem.\n+ school",
                                "Text",
                                "Both"),
                     ordered = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("") +
  ylab("Estimate(linear reg; positive =\nhigher chance of response)") +
  coord_flip() +
  theme_oes() +
  scale_color_manual(values = c("white", "red", "blue")) +
  guides(color = FALSE)

ggsave("../output/resp_demtext.pdf",
       plot = last_plot(),
       device = "pdf",
       width= 12,
       height = 8)

ggplot(all_reg, aes(x = reorder(var_clean, Estimate), y = Estimate,
                    color = model)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper,
                    color = model)) +
  facet_wrap(~factor(model,
                     levels = c("Family dem.\n+ school",
                                "Text",
                                "Both"),
                     ordered = TRUE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("") +
  ylab("Estimate(linear reg; positive =\nhigher chance of response)") +
  coord_flip() +
  theme_oes() +
  scale_color_manual(values = c("purple", "red", "blue")) +
  guides(color = FALSE)

ggsave("../output/resp_all.pdf",
       plot = last_plot(),
       device = "pdf",
       width= 12,
       height = 8)


############################################# Ad-hoc school dem #############################


## see that osse is missing more school, 
## probably due to crosswalk
## try to get school
osse_samp_cleaner = osse_samp %>%
              mutate(StudentID = as.character(StudentID),
                     school_anon = case_when(school_merge == "CHEC" ~ "School C",
                               school_merge == "Anacostia" ~ "School A",
                               school_merge == "Dunbar" ~ "School B",
                               TRUE ~ "Other")) %>%
              left_join(msg_wtranslate %>% dplyr::select(StudentID,
                                                         school_anon) %>%
                          distinct() %>%
                          rename(school_alternate = school_anon),
                        by = "StudentID") %>%
              mutate(school_final = 
                    case_when(school_anon == "Other" ~ school_alternate,
                              TRUE ~ school_anon),
                    swd_clean = relevel(factor(ifelse(!grepl("Level", highest_swd_level), 
                                                      "No IEP",
                                                      sprintf("IEP_%s", highest_swd_level))), ref = "No IEP"),
                    ell_clean = ifelse(ell == "NO" | ell == "", 0,
                                       1),
                    gender_clean = ifelse(gender %in% c("F", "Female"), 
                                          "Female", 
                                          ifelse(gender %in% c("M", "Male"),
                                                 "Male", "Missing")),
                    atrisk_clean = ifelse(at.risk == "YES", 1, 0))

## left join with msg_bystudent to get median when we include
## those with zero
osse_samp_cleaner_wmsg = osse_samp_cleaner %>% dplyr::select(StudentID, school_final) %>%
                  left_join(msg_wtranslate_bystud %>% group_by(StudentID) %>%
                          summarise(total_bothroles = sum(total_outgoing)), by = "StudentID") %>%
                  mutate(total_bothrole_clean = ifelse(is.na(total_bothroles), 0, 
                                                       total_bothroles))

osse_samp_cleaner_wmsg %>% group_by(school_final) %>% summarise(median(total_bothrole_clean))

## summarise demographics

library(xtable)
binary_vars = osse_samp_cleaner %>%
  group_by(school_final) %>%
  summarise_at(vars("atrisk_clean", "ell_clean"),
               mean) 

xtable(t(binary_vars))

osse_samp_cleaner %>%
  group_by(school_final, swd_clean) %>%
  summarise(count = n()) %>%
  left_join(osse_samp_cleaner %>% group_by(school_final) %>%
              summarise(denom = n())) %>%
  mutate(prop_iep = count/denom,
         opp = 1-prop_iep)

## get distribution of parents/guardians by school
relationships_byschool = msg_wtranslate_wdedup %>%
                dplyr::select(school_anon,
                              relationship, 
                              StudentID) %>%
                distinct() %>%
                mutate(relate_broad = 
                      case_when(
                        relationship == "" ~ "Unknown",
                        relationship %in% c("Mother", "Parent", 
                        "Father") ~ "Parent", 
                        TRUE ~ "Other")) # students can appear >1 time for multiple parents so will sum > 1

## add in students wh
relationships_byschool %>%
  group_by(school_anon, relate_broad) %>%
  summarise(num = n()) %>%
  left_join(relationships_byschool %>%
            group_by(school_anon) %>%
            summarise(denom = n())) %>%
  mutate(prop = num/denom)

############################################# Fig 6 labels versus free text #############################
        
## first, show missingness of labels
labels_bytype = msg_wtranslate_wdedup %>%
            group_by(topic, broad_type) %>%
            summarise(num = n()) %>%
            left_join(msg_wtranslate_wdedup %>%
                      group_by(broad_type) %>% summarise(denom = n())) %>%
            ungroup() %>%
            mutate(perc = (num/denom)*100,
                   type_clean = 
                  ifelse(grepl("parent",
                  broad_type), 
                  "Parent to school",
                  "School to parent"),
                  uncat = 
                  ifelse(topic == "Uncategorized",
                  "Uncat",
                  "Cat")) 

        
ggplot(labels_bytype, aes(x = reorder(topic, perc), y = perc,
                          fill = uncat)) +
  geom_bar(stat = "identity") +
  xlab("") +
  coord_flip() +
  theme_oes(base_size = 24) +
  facet_grid(~type_clean, scales = "free") +
  ylab("Percent of messages of that type") +
  geom_label(aes(x = reorder(topic, perc), y = perc,
                 label = round(perc, 1)),
             fill = "white", size = 8) +
  scale_fill_manual(values = 
                  c("Uncat" = "wheat4",
                    "Cat" =yes_green)) +
  guides(fill = FALSE) +
  ylim(-5, 105) 
  
ggsave("../output/categories_bytype.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)        
    
## second, create keyword list to seed model; just lowercase for each
topic_list = lapply(as.list(unique(msg_wtranslate_wdedup$topic)), function(x) unlist(strsplit(tolower(x), "\\s+")))
names(topic_list) = unique(msg_wtranslate_wdedup$topic)

## see if all duplicated messages have same topic assigned
## they do
topic_permsg = msg_wtranslate_wdedup %>%
        filter(broad_type == "school_sent") %>%
        group_by(id_content_noreceiverstudentname_translated) %>%
        summarise(all_topics = paste(unique(topic, collapse = "; ")))

## third, filter to unique msg after
## student/parent name removal and translation
msg_uniqueaftertrans = msg_wtranslate_wdedup %>%
  dplyr::select(contains("noreceiverstudentname_translated")) %>%
  distinct() 

msg_uniqueaftertrans$row_id = sprintf("text%s", 1:nrow(msg_uniqueaftertrans))

library(quanteda) # call here bc it masks View(df) so probably need to call utils::view after
library(keyATM)
msg_tokens = tokens(as.character(msg_uniqueaftertrans$content_noreceiverstudentname_translated),
                      remove_numbers = TRUE,
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_url = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("English"), "chec", "panorama", "anacostia", "dunbar", "dcps", "bit.ly")) %>%
  tokens_select(min_nchar = 3) 

msg_dfm = dfm(msg_tokens) %>%
  dfm_trim(min_termfreq =  10, # term appears 10+ times
           min_docfreq = 3) # in at least 3 messages

## remove ones with 0 words from both
## notes and original df
rows_zeroterms = rowSums(msg_dfm)[rowSums(msg_dfm) == 0]
sprintf("Removing %s out of %s messages", length(rows_zeroterms), 
        length(unique(msg_uniqueaftertrans$content_noreceiverstudentname_translated)))
msg_dfm_afterproc = dfm_subset(msg_dfm,
                                 ntoken(msg_dfm) > 0)


msg_df_afterproc = msg_uniqueaftertrans %>%
  filter(!row_id %in% names(rows_zeroterms))

## put into keyatm class
keyATM_msgprep = keyATM_read(texts = msg_dfm_afterproc)

## none of tt keywords appear in docs so create our own keywords
manual_absences = c("absent", "attend", "present", "attendance", "sick", "excused", "note", "late")
manual_acad = c("english", "algebra", "history", "geography", "math", "biology")
manual_inperson = c("visit", "coffee")
manual_polite = c("hello", "thank", "thanks", "appreciate", "welcome", "please") 
manual_keywords = list(absences = manual_absences, academics = manual_acad,
                       meeting = manual_inperson, polite = manual_polite)


key_viz = visualize_keywords(docs = keyATM_msgprep,
                             keywords = manual_keywords)


## fit model without covariates
keyatm_out = keyATM(docs = keyATM_msgprep,
                    no_keyword_topics = length(manual_keywords),
                    keywords = manual_keywords,
                    model = "base",
                    options = list(seed = 91988, iterations = 1500))

saveRDS(keyatm_out, "../intermediate_outputs/keyatm_manualtop.RDS")

tw_keyatm = top_words(keyatm_out, show_keyword = FALSE, n = 15)
colnames(tw_keyatm) = gsub("^[0-9]|\\_", "", colnames(tw_keyatm))

tw_keyatm_forgraph = tw_keyatm %>%
  dplyr::select(Other2,
                absences) %>%
  mutate(word_num = 1:nrow(tw_keyatm)) %>%
  reshape2::melt(, id.vars = "word_num") %>%
  mutate(keyword = ifelse(value %in% unique(unlist(topic_list)), "App label",
                          "Not app label"),
         word_clean = gsub("\\s+\\[.*", "", value),
         broad_topic = ifelse(grepl("absences", variable),
                              "Absences\n(general; courses)",
                              "Consequences"),
         inv_rank = 1/word_num)  

ggplot(tw_keyatm_forgraph, aes(x = reorder(factor(word_clean), inv_rank), 
                               y = inv_rank,
                               fill = keyword)) +
  geom_bar(stat = "identity") +
  theme_oes(base_size = 24) +
  facet_wrap(~broad_topic,
             scales = "free_y") +
  coord_flip() +
  ylab("Word rank (inverse)") +
  xlab("Top words in topic") +
  scale_fill_manual(values = c("App label" = "wheat4",
                               "Not app label" = "#12312b")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 24)) +
  labs(fill = "") 

ggsave("../output/keywords_tt.pdf",
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)


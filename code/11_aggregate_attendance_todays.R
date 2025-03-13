

#######################################
# Imports and constants
#######################################
rm(list=ls())
library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(data.table)

RAW_DATA_NAME = "anonymizedattend_3dcps.csv"
TIMETABLE_NAME = "student_timetable.csv"
PLOT = FALSE
WRITE_TIMETABLE = FALSE

#######################################
# Data cleaning and loading
#######################################

#' Read data and clean
df_init<-fread(here("data", "raw", RAW_DATA_NAME))

print(sprintf("Initial data dim: %s rows; %s col", dim(df_init)[1],
              dim(df_init)[2]))                                          # 1,083,229 observations; 27 variables

## Filter out cols not using or cause duplicates
df_init <-as.data.frame(df_init)[,  setdiff(colnames(df_init), 
                           c("usi","lea_student_id","enrollment_date",
                            "withdrawal_date","attendance_merge_status"))]      

## Rename hashed_id as student_id
colnames(df_init)[colnames(df_init)=="hashed_id"]<-"student_id"
stopifnot(length(grep("student_id", colnames(df_init), value = TRUE)) >= 1)

## Drop pure duplicates
summary(duplicated(df_init))                           # 1,051,829 unique observations with 31,400 pure duplicates
df_init$dup<-duplicated(df_init)
df_init <-df_init[df_init$dup==F,]
df_init <-df_init[,!(colnames(df_init)=="dup")] 
dim(df_init)                                           # 1,051,829 observations; 22 variables

## Convert date to dt format,
## Drop data b after 01/24/2020
## Keeping all before data to be able
## to do the 2-weeks after account
## activation analysis
df_init <- df_init %>%
        mutate(date_dt = ymd(Date)) 

df_timewindow <- df_init %>%
        filter(date_dt <= "2020-01-24")                     

sprintf("After filtering to end of sem, we go from %s to %s student-period-day obs",
        nrow(df_init),
        nrow(df_timewindow))

## plot distribution of days per student overtime
if(PLOT){
  
  df_stud_perday = df_timewindow %>%
          mutate(date_pos = as.POSIXct(date_dt)) %>%
          group_by(date_pos) %>%
          summarise(unique_students = length(unique(student_id)))

  ## based on plot, seems to skip weekend days
  ggplot(df_stud_perday,
       aes(x = date_pos, 
           y = unique_students)) +
      geom_bar(stat = "identity") +
      scale_x_datetime(date_breaks = "1 week") +
    coord_flip() 
} 


#######################################
# Create summary of windows of analytic sample
# to create timetable file to write
#######################################
data <- df_timewindow

# 1. Number of days for each students
length(unique(data$student_id))                     # 2,299 students
length(unique(data$Date))                           
dim(unique(data[c("student_id","Date")]))[1]

sum(is.na(data$Date))                               # No missing value in Date
timetable<-matrix(nrow=length(unique(data$student_id)),ncol=3)
timetable<-cbind(sort(unique(data$student_id)),timetable)

## iterate over students and:
### in second col of timetable, put number of observed
### school attendance days
### in third col, min date
### in fourth col, max date
for(i in 1:length(unique(data$student_id))){
  timetable[i,2]<-length(sort(unique(data[which(data$student_id==timetable[i,1]),]$Date)))
  timetable[i,3]<-sort(unique(data[which(data$student_id==timetable[i,1]),]$Date))[1]
  timetable[i,4]<-sort(unique(data[which(data$student_id==timetable[i,1]),]$Date))[length(unique(data[which(data$student_id==timetable[i,1]),]$Date))]
}
colnames(timetable)<-c("student_id","days","from","to")
timetable<-as.data.frame(timetable)
dim(timetable)
summary(as.factor(timetable$days))                                  # 2,174 out of 2,299 (95%) students have records for all 66 days


# 2. Check continuity of dates attended for students with multiple attended schools (i.e. are subject to transfer)
length(unique(data$student_id))                                     # 2,299 unique student_ids
dim(unique(data[c("student_id","school_attendancedf")]))[1]         # 2,307 unique student_id and school_attendancedf combinations

repeatedid<-unique(data[c("student_id","school_attendancedf")])[which(duplicated(unique(data[c("student_id","school_attendancedf")])$student_id)==T),]$student_id 

dim(unique(data[c("student_id","school_attendancedf")])[which(unique(data[c("student_id","school_attendancedf")])$student %in% repeatedid),])

if(WRITE_TIMETABLE){
  table<-matrix(nrow=16,ncol=5)
  
  #for lm: this or loop line return an error bc 16 hard coded; is it length repeated_id? if so use that
  table<-cbind(unique(data[c("student_id","school_attendancedf")])[which(unique(data[c("student_id","school_attendancedf")])$student %in% repeatedid),],table)
  
  for(i in 1:16){ 
    table[i,3]<-unique(data[which(data$student_id==table[i,1]),]$school_initrosters)
    table[i,4]<-length(sort(unique(data[which(data$student_id==table[i,1]),]$Date)))
    table[i,5]<-length(sort(unique(data[which(data$student_id==table[i,1] & data$school_attendancedf==table[i,2]),]$Date)))
    table[i,6]<-sort(unique(data[which(data$student_id==table[i,1] & data$school_attendancedf==table[i,2]),]$Date))[1]
    table[i,7]<-sort(unique(data[which(data$student_id==table[i,1] & data$school_attendancedf==table[i,2]),]$Date))[length(unique(data[which(data$student_id==table[i,1] & data$school_attendancedf==table[i,2]),]$Date))]
  }
  colnames(table)<-c("student_id","school_attendancedf","school_initrosters","total_days","days","from","to")
  table<-as.data.frame(table)
  table<-table[order(table$student_id,table$from,table$to),]
  
  ## create indicator flag in timetable for if student switches schools 
  timetable <- timetable %>%
    mutate(is_diff_initattend = ifelse(student_id %in% unique(table$student_id), TRUE, FALSE))
  write.csv(timetable, sprintf("data/intermediate/student_timetable.csv"),
            row.names = FALSE)
}


#######################################
# Create two forms of aggregation from period-level statuses
# to day-level statuses
#######################################

# 3. Student-day-highschool (st-d-hs) aggregation approach
# 3.1. Attendance at student-day-school level (st-d-hs approach)
summary(as.factor(data$AttendanceStatus))
data[which(data$AttendanceStatus==""),]$AttendanceStatus<- "Missing"
dim(unique(data[c("student_id","Date","school_attendancedf")]))[1]    # 147,780
dim(unique(data[c("student_id","Date")]))[1]                          # 147,654
attd<-matrix("",nrow=dim(unique(data[c("student_id","Date","school_attendancedf")]))[1],ncol=7)
attd<-cbind(unique(data[c("student_id","Date","school_attendancedf")]),attd)
colnames(attd)<-c("student_id","Date","school_attendancedf","cp","cpnumber","status","NAS","A","P","PL")

for(i in 1:dim(attd)[1]){
  attd$cp[i]<-length(unique(data[which(data$student_id==attd$student_id[i] & data$Date==attd$Date[i] & data$school_attendancedf==attd$school_attendancedf[i]),]$ClassPeriod))
  attd$cpnumber[i]<-paste(data[which(data$student_id==attd$student_id[i] & data$Date==attd$Date[i] & data$school_attendancedf==attd$school_attendancedf[i]),]$ClassPeriod,collapse = ";")
  attd$status[i]<-paste(data[which(data$student_id==attd$student_id[i] & data$Date==attd$Date[i] & data$school_attendancedf==attd$school_attendancedf[i]),]$AttendanceStatus,collapse = ";")
  print(i)
}
attd$cp<-as.numeric(attd$cp)

attd$cpcheck<-0
attd$statuscheck<-0
attd$cpncheck<-0
for(i in 1:dim(attd)[1]){
  attd$cpcheck[i]<-length(unique(strsplit(attd$cpnumber[i],";")[[1]]))
  attd$cpncheck[i]<-length(strsplit(attd$cpnumber[i],";")[[1]])
  attd$statuscheck[i]<-length(strsplit(attd$status[i],";")[[1]])
  print(i)
}
summary(attd$cp-attd$cpcheck)
summary(attd$cpncheck-attd$statuscheck)
sum(attd$cpncheck)
sum(attd$statuscheck)

for(i in 1:dim(attd)[1]){
  attd$NAS[i]<-sum(strsplit(attd$status[i],";")[[1]]=="Missing")
  attd$A[i]<-sum(strsplit(attd$status[i],";")[[1]]=="A")
  attd$P[i]<-sum(strsplit(attd$status[i],";")[[1]]=="P")
  attd$PL[i]<-sum(strsplit(attd$status[i],";")[[1]]=="P/L")
  print(i)
}
attd$NAS<-as.numeric(attd$NAS)
attd$A<-as.numeric(attd$A)
attd$P<-as.numeric(attd$P)
attd$PL<-as.numeric(attd$PL)
sum(attd$NAS+attd$A+attd$P+attd$PL)
summary(attd$NAS+attd$A+attd$P+attd$PL-attd$cpncheck)

attd$present<-attd$P+attd$PL
attd$prate<-attd$present/attd$cp
summary(attd$prate)

attd$pstatus<-0
attd[which(attd$prate>=0.8),]$pstatus<-1
dim(attd[which(attd$prate>1),])                                     # 3,158 out of 147,780 (2%) student-day-school level observations have present rate higher than 1 

# 3.2. Attendance at student-day level (st-d-hs approach)
dim(unique(data[c("student_id","Date")]))[1]  
studentday<-matrix("",nrow=dim(unique(data[c("student_id","Date")]))[1],ncol=1)
studentday<-cbind(unique(data[c("student_id","Date")]),studentday)
colnames(studentday)<-c("student_id","Date","pstatus")

for(i in 1:dim(studentday)[1]){
  studentday$pstatus[i]<-paste(attd[which(attd$student_id==studentday$student_id[i] & attd$Date==studentday$Date[i]),]$pstatus,collapse = ";")
  print(i)
}
studentday$pfinal<-grepl("1",studentday$pstatus)
studentday[which(studentday$pfinal==T),]$pfinal<-1
studentday[which(studentday$pfinal==F),]$pfinal<-0
xtabs(~studentday$pfinal+studentday$pstatus,exclude=NULL,na.action=na.pass)

aggregation_stdhs<-studentday[,!(colnames(studentday)=="pstatus")]

write.csv(attd,here("data", "intermediate", "attd.csv"))
write.csv(studentday,here("data", "intermediate","studentday.csv"))
write.csv(aggregation_stdhs,here("data", "intermediate",
                                 "aggregation_stdhs.csv"))

# 4. Student-day-highschool-classperiod (st-d-hs-cp) aggregation approach
# 4.1. Attendance at student-day-school-classperiod level (st-d-hs-cp approach)
dim(unique(data[c("student_id","Date","school_attendancedf","ClassPeriod")]))[1]    # 694,633
attd2<-matrix("",nrow=dim(unique(data[c("student_id","Date","school_attendancedf","ClassPeriod")]))[1],ncol=7)
attd2<-cbind(unique(data[c("student_id","Date","school_attendancedf","ClassPeriod")]),attd2)
colnames(attd2)<-c("student_id","Date","school_attendancedf","ClassPeriod","present","pcheck","status","NAS","A","P","PL")
dim(attd2)
for(i in 1:dim(attd2)[1]){
  attd2$status[i]<-paste(data[which(data$student_id==attd2$student_id[i] & data$Date==attd2$Date[i] & data$school_attendancedf==attd2$school_attendancedf[i] & data$ClassPeriod==attd2$ClassPeriod[i]),]$AttendanceStatus,collapse = ";")
  print(i)
}
for(i in 1:dim(attd2)[1]){
  attd2$NAS[i]<-sum(strsplit(attd2$status[i],";")[[1]]=="NA")
  attd2$A[i]<-sum(strsplit(attd2$status[i],";")[[1]]=="A")
  attd2$P[i]<-sum(strsplit(attd2$status[i],";")[[1]]=="P")
  attd2$PL[i]<-sum(strsplit(attd2$status[i],";")[[1]]=="P/L")
  print(i)
}
attd2$NAS<-as.numeric(attd2$NAS)
attd2$A<-as.numeric(attd2$A)
attd2$P<-as.numeric(attd2$P)
attd2$PL<-as.numeric(attd2$PL)
sum(attd2$NAS+attd2$A+attd2$P+attd2$PL)
attd2$pcheck<-grepl("P",attd2$status)
attd2$present<-0
attd2[which(attd2$P>0|attd2$PL>0),]$present<-1
xtabs(~attd2$pcheck+attd2$present,exclude=NULL,na.action=na.pass)

# 4.2. Attendance at student-day-school level (st-d-hs-cp approach)
sdhs<-matrix("",nrow=dim(unique(attd2[c("student_id","Date","school_attendancedf")]))[1],ncol=3)
sdhs<-cbind(unique(attd2[c("student_id","Date","school_attendancedf")]),sdhs)
colnames(sdhs)<-c("student_id","Date","school_attendancedf","cp","cpnumber","status")

for(i in 1:dim(sdhs)[1]){
  sdhs$cp[i]<-length(unique(attd2[which(attd2$student_id==sdhs$student_id[i] & attd2$Date==sdhs$Date[i] & attd2$school_attendancedf==sdhs$school_attendancedf[i]),]$ClassPeriod))
  sdhs$cpnumber[i]<-paste(attd2[which(attd2$student_id==sdhs$student_id[i] & attd2$Date==sdhs$Date[i] & attd2$school_attendancedf==sdhs$school_attendancedf[i]),]$ClassPeriod,collapse = ";")
  sdhs$status[i]<-paste(attd2[which(attd2$student_id==sdhs$student_id[i] & attd2$Date==sdhs$Date[i] & attd2$school_attendancedf==sdhs$school_attendancedf[i]),]$present,collapse = ";")
  print(i)
}
sdhs$cp<-as.numeric(sdhs$cp)
sum(sdhs$cp)

sdhs$cpcheck<-0
sdhs$statuscheck<-0
for(i in 1:dim(sdhs)[1]){
  sdhs$cpcheck[i]<-length(strsplit(sdhs$cpnumber[i],";")[[1]])
  sdhs$statuscheck[i]<-length(strsplit(sdhs$status[i],";")[[1]])
  print(i)
}
summary(sdhs$cp-sdhs$cpcheck)
summary(sdhs$cp-sdhs$statuscheck)
summary(sdhs$cpcheck-sdhs$statuscheck)

sdhs$present<-0
sdhs$absent<-0

for(i in 1:dim(sdhs)[1]){
  sdhs$present[i]<-sum(strsplit(sdhs$status[i],";")[[1]]=="1")
  sdhs$absent[i]<-sum(strsplit(sdhs$status[i],";")[[1]]=="0")
  print(i)
}
summary(sdhs$present+sdhs$absent-sdhs$cp)
sum(sdhs$present+sdhs$absent)

sdhs$prate<-sdhs$present/sdhs$cp
summary(sdhs$prate)
sdhs$pstatus<-0
sdhs[which(sdhs$prate>=0.8),]$pstatus<-1
xtabs(~sdhs$prate+sdhs$pstatus,exclude=NULL,na.action=na.pass)

# 4.3. Attendance at student-day level (st-d-hs-cp approach)
studentday2<-matrix("",nrow=dim(unique(data[c("student_id","Date")]))[1],ncol=1)
studentday2<-cbind(unique(data[c("student_id","Date")]),studentday2)
colnames(studentday2)<-c("student_id","Date","pstatus")

for(i in 1:dim(studentday2)[1]){
  studentday2$pstatus[i]<-paste(sdhs[which(sdhs$student_id==studentday2$student_id[i] & sdhs$Date==studentday2$Date[i]),]$pstatus,collapse = ";")
  print(i)
}

studentday2$pfinal<-grepl("1",studentday2$pstatus)
studentday2[which(studentday2$pfinal==T),]$pfinal<-1
studentday2[which(studentday2$pfinal==F),]$pfinal<-0
xtabs(~studentday2$pfinal+studentday2$pstatus,exclude=NULL,na.action=na.pass)

aggregation_stdhscp<-studentday2[,!(colnames(studentday2)=="pstatus")]

write.csv(attd2,here("data", "intermediate", "attd2.csv"))
write.csv(studentday2,here("data", "intermediate","studentday2.csv"))
write.csv(aggregation_stdhscp,here("data", "intermediate",
                                 "aggregation_stdhscp.csv"))
write.csv(sdhs, here("data", "intermediate", "sdhs.csv"))


# 5. Check consistency between two approaches
check<-rbind(aggregation_stdhs,aggregation_stdhscp)
dim(unique(aggregation_stdhs[c("student_id","Date")]))[1]
dim(unique(aggregation_stdhscp[c("student_id","Date")]))[1]
dim(unique(check[c("student_id","Date")]))[1]
dim(unique(check[c("student_id","Date","pfinal")]))[1]                         # 111 rows with same student_id and date but different present status

check2<-cbind(aggregation_stdhs,aggregation_stdhscp)
colnames(check2)<-c("id1","date1","pfinal1","id2","date2","pfinal2")
check2$idcheck<-check2$id1==check2$id2
check2$datecheck<-check2$date1==check2$date2
summary(check2$idcheck)
summary(check2$datecheck)
check2$pcheck<-check2$pfinal1==check2$pfinal2
summary(check2$pcheck)
length(unique(check2[which(check2$pcheck==F),]$id1))                           # Inconsistency happen to 62 out of 2,299 (2.7%) students
unique(check2[which(check2$pcheck==F),]$pfinal1)
unique(check2[which(check2$pcheck==F),]$pfinal2)                               # Inconsistency cases: the st-d-hs approach records present while the st-d-hs-cp approach records absent

print("finished script")
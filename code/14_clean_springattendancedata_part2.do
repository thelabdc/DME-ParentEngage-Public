cd ..\Data

use "..\Data\CombinedSpring2020.dta"

**Convert AttendanceStatus from string to numeric variable

gen AttendanceStatus2 = 1 if AttendanceStatus == "P"
replace AttendanceStatus2 = 2 if AttendanceStatus == "A"
replace AttendanceStatus2 = 3 if AttendanceStatus == "P/L"
replace AttendanceStatus2 = 4 if AttendanceStatus == "N/A"

label define attsta_lbl 1 "P" 2 "A" 3 "P/L" 4 "N/A"
label values AttendanceStatus2 attsta_lbl

drop AttendanceStatus

save "..\Data\CombinedSpring2020_edits.dta", replace

**Look at when N/A attendance status was most frequent

histogram Date if AttendanceStatus2 == 4, frequency

**Looks like there's some days with lots of n/a observations at the beginning of the school year (maybe unenrollment) and then it drops and peaks again in April/May/June (maybe virtual learning)

**Creat new binary attendance variable

gen present = 1 if AttendanceStatus2 == 1
replace present = 0 if AttendanceStatus2 == 2
replace present = 1 if AttendanceStatus2 == 3

label define pres_lbl 0 "Absent" 1 "Present" 
label values present pres_lbl

**drop unnecessary variables

drop LastName
drop FirstName
drop ClassName
drop ClassPeriod
drop LostInstructionalTime
drop ReasonForAbsenteeism
drop StudentGradeLevel
drop AttendanceStatus2

** Quick cleaning

rename Date date
rename StudentID studentid

label var studentid "Student ID"
label var present "Present or Absent"
label var school "School Name"

save "..\Data\CombinedSpring2020_edits.dta", replace


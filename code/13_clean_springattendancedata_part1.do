

**Converting all excel files to dta so they are easier to combine

import excel "..\Data\Friendship Charter Spring 2020 Term Attendance Data.xlsx", sheet("Sheet1") firstrow

gen school = 1

save "..\Data\Friendship Charter Spring 2020 Term Attendance Data.dta"

clear

import excel "..\Data\Johnson Middle School Spring 2020 Term Attendance Data.xlsx", sheet("Sheet1") firstrow

gen school = 2

save "..\Data\Johnson Middle School Spring 2020 Term Attendance Data.dta"

clear

import excel "..\Data\Paul International Charter School Spring 2020 Term Attendance Data.xlsx", sheet("Sheet1") firstrow

gen school = 3

save "..\Data\Paul International Charter School Spring 2020 Term Attendance Data.dta"

clear

import excel "..\Data\Paul Public Charter Middle School Spring 2020 Term Attendance Data.xlsx", sheet("Sheet1") firstrow

gen school = 4

save "..\Data\Paul Public Charter Middle School Spring 2020 Term Attendance Data.dta"

clear

**combining dta files to create one combined data set

use "..\Data\Friendship Charter Spring 2020 Term Attendance Data.dta"

append using "Johnson Middle School Spring 2020 Term Attendance Data.dta", force

append using "Paul International Charter School Spring 2020 Term Attendance Data.dta", force

append using "Paul Public Charter Middle School Spring 2020 Term Attendance Data.dta", force

label define school_lbl 1 "Friendship" 2 "Johnson" 3 "Paul International" 4 "Paul Public"
label values school school_lbl

save "..\Data\CombinedSpring2020.dta", replace

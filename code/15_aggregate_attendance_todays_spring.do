cd ..\Data

use "..\Data\CombinedSpring2020_edits.dta"

** collapse data up

gen present2 = present
replace present2 = 0 if present2 == .

collapse (count) present2 (sum) present, by(date studentid school)

rename present2 totalperiods

rename present totalpresent

**calculating day-level present status

gen presentratio = totalpresent/totalperiods

gen present = 1 if presentratio >= .8
replace present = 0 if presentratio < .8

label define present_lbl 1 "Present" 0 "Absent" 
label values present present_lbl

save "..\Data\DayLevelSpring2020.dta", replace

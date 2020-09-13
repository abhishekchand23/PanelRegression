import excel "scho_data_new.xlsx" ,sheet("Sheet3") firstrow

summarize
describe

twoway scatter Enrolment Scholarship || lfit Enrolment Scholarship 
graph save Enrolment_ST_ols

encode DISTRICT , gen(DISTRICT1)
xtset DISTRICT1 YEAR 
xtline Enrolment

gen log_enrolment = log(Enrolment)
xtline log_enrolment
graph save Enrolment_ST
xtline log_enrolment, overlay
graph save Enrolment_ST_Overlay

bysort DISTRICT1 :egen Enrolment_mean = mean(Enrolment)
twoway scatter Enrolment DISTRICT1 , msymbol(circle_hollow) || connected Enrolment_mean DISTRICT1 , msymbol(diamond)|| , xlabel(1 "Almora" 2 "Bageshwar" 3 "Chamoli" 4 "Champawat" 5 "DehraDun" 6 "Haridwar" 7 "Nainital" 8 "Pauri" 9 "Pithoragarh" 10 "Rudraprayag" 11 "Tehri" 12 "USN" 13 "Uttarkashi")
graph save Enrolment_ST_het_dist

bysort YEAR  :egen Enrolment_mean_Y = mean(Enrolment)
twoway scatter Enrolment YEAR  , msymbol(circle_hollow) || connected Enrolment_mean_Y YEAR , msymbol(diamond)|| , xlabel(2013(1) 2017)
graph save Enrolment_ST_het_year

 regress Enrolment Scholarship 
 estimates store ols
 
 xi: regress Enrolment  Scholarship  i.DISTRICT
 estimates store ols_dum
 estimates table ols ols_dum, star stats(N)
 
  xtreg Enrolment Scholarship , fe
  estimate store fixed
  areg Enrolment Scholarship ,absorb(DISTRICT)
  
  xtreg Enrolment Scholarship ,re
  estimate store random
  xtreg Enrolment Scholarship Altitude  ,re
  estimate store random1
  
  hausman fixed random
  hausman fixed random1

  xtreg Enrolment Scholarship ,re
  xttest0
  xtreg Enrolment Scholarship Altitude  ,re
  xttest0
  
  
   xtreg Enrolment Scholarship , fe
   *xttest2 singular residuals
   
   xtreg Enrolment Scholarship , fe
    xtcsd, pesaran abs
	
xtscc Enrolment Scholarship ,fe
xtscc Enrolment Scholarship ,re

xtscc Enrolment Scholarship Altitude  ,re
	  
  xtreg Enrolment Scholarship i.YEAR ,fe
	testparm i.YEAR
	
	xtscc Enrolment Scholarship i.YEAR ,fe

	xtreg Enrolment Scholarship ,re robust
	xtreg Enrolment Scholarship Altitude,re robust







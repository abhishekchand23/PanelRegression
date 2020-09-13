import excel "scho_data_new.xlsx" ,sheet("Sheet1") firstrow

twoway scatter Enrolment Scholarship || lfit Enrolment Scholarship 
encode DISTRICT , gen(DISTRICT1)
xtset DISTRICT1 YEAR 

twoway scatter Enrolment DISTRICT1 , msymbol(circle_hollow) || connected Enrolment_mean DISTRICT1 , msymbol(diamond)|| , xlabel(1 "Almora" 2 "Bageshwar" 3 "Chamoli" 4 "Champawat" 5 "DehraDun" 6 "Haridwar" 7 "Nainital" 8 "Pauri" 9 "Pithoragarh" 10 "Rudraprayag" 11 "Tehri" 12 "USN" 13 "Uttarkashi")

*preparing GHS from Nigeria 2016

ma drop all
set more off
set seed 23081980

local using= "${gsdData}/NGA-GHS2016-HHData.dta"
capture confirm file "`using'"
if _rc != 0 {
	*data directory
	local sData = "${gsdDataBox}/NGA-GHS2016"
		
	*Consolidating food expenditure data using post harvest food expenditure data: (Post harvest data is used as that is the latest data available.)
	use "`sData'/sect10a_harvestw3.dta", clear 
	gen item_cd_oh = item_cd
	labmask item_cd_oh, val(item_cd) decode
	drop item_cd
	label drop item_cd
	append using "`sData'/sect10b_harvestw3.dta"
	*Manually cleaning food items list:
	*Dropping food items with description "other":
	drop if inlist(item_cd,23,66,79,82,96,107,115,133,155,164)
	*Cleaning core variables:
	replace item_cd = item_cd_oh if missing(item_cd)
	labvalcombine item_cd_oh item_cd, lblname(item_cd_c)
	label values item_cd item_cd_c
	drop item_cd_oh
	merge m:1 hhid using "`sData'/HHTrack.dta", keep(match) keepusing(wt_wave3) nogen
	ren wt_wave3 weight
	gen value = s10aq2
	replace value = s10bq4 if missing(value)
	*Aggregating by item
	collapse (sum) value, by(hhid item_cd weight)
	ren (item_cd value) (foodid xfood) 
	fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
	save "${gsdTemp}/NGA-HHFoodItems.dta", replace

	*non-food
	use "`sData'/sect11a_harvestw3.dta", clear
	gen item_recall = 7
	gen item_cd_a = item_cd
	labmask item_cd_a, val(item_cd) decode
	drop item_cd
	label drop item_cd
	append using "`sData'/sect11b_harvestw3.dta"
	replace item_recall = 30 if missing(item_recall)
	gen item_cd_b = item_cd
	labmask item_cd_b, val(item_cd) decode
	drop item_cd
	label drop item_cd
	append using "`sData'/sect11c_harvestw3.dta"
	replace item_recall = 180 if missing(item_recall)
	gen item_cd_c = item_cd
	labmask item_cd_c, val(item_cd) decode
	drop item_cd
	label drop item_cd
	append using "`sData'/sect11d_harvestw3.dta"
	replace item_recall = 365 if missing(item_recall)
	gen item_cd_d = item_cd
	labmask item_cd_d, val(item_cd) decode
	drop item_cd
	label drop item_cd
	append using "`sData'/sect11e_harvestw3.dta"
	replace item_recall = 365 if missing(item_recall)
	gen item_cd_e = item_cd
	labmask item_cd_e, val(item_cd) decode
	drop item_cd
	label drop item_cd
	labvalcombine item_cd_a item_cd_b item_cd_c item_cd_d item_cd_e, lblname(item_cd_c)
	gen item_cd = .
	foreach x in a b c d e {
		replace item_cd = item_cd_`x' if missing(item_cd)
	}
	label values item_cd item_cd_c
	gen value = s11aq2
	replace value = s11bq4 if missing(value)
	replace value = s11cq6 if missing(value)
	replace value = s11dq8 if missing(value)
	replace value = s11eq10 if missing(value)
	merge m:1 hhid using "`sData'/HHTrack.dta", keep(match) keepusing(wt_wave3) nogen
	ren wt_wave3 weight
	collapse (sum) value, by (hhid item_cd weight)
	ren (item_cd value) (nonfoodid xnonfood) 
	fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
	save "${gsdTemp}/NGA-HHNonFoodItems.dta", replace

	*Calculating hhsize:
	use "`sData'/sect1_harvestw3.dta", clear
	*Drop individuals who are not living in the household.
	drop if s1q4a == 2
	ren s1q4 age
	gen age_child = age<15 if age<.
	gen age_adult = inrange(age,15,64) if age<.
	gen age_senior = age>64 if age<.
	*get head variables
	gen head_age = age if s1q3==1
	gen head_sex = s1q2 if s1q3==1
	collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior head_age head_sex, by(hhid)
	gen pchild = nchild / hhsize
	gen psenior = nsenior / hhsize
	*add dwelling characteristics
	merge 1:1 hhid using "`sData'/sect11_plantingw3.dta", keepusing(ea zone state sector s11q6 s11q7 s11q8 s11q9 s11q10 s11q11 s11q17b s11q28b s11q31 s11q33a s11q36 s11q38) keep(match) nogen
	ren (s11q6 s11q7 s11q8 s11q9 s11q10 s11q11 s11q17b s11q28b s11q31 s11q33a s11q36 s11q38) (hhwall hhroof hhfloor hhrooms hhlight hhcook hhelectr hhgen hhphone hhdrink hhtoilet hhwaste)
	*wall
	recode hhwall (0/5=1) (7/8=1) (6 9=2) (11 .=1)
	label define lwall 1 "Grass, Mudd, Wood, Bricks" 2 "Concrete or Cement", replace
	label values hhwall lwall
	*roof
	recode hhroof (1 5 6 7 .=1) (3 4 = 3)
	label define lroof 1 "Grass, Plastic, Asbestos" 2 "Iron" 3 "Concrete", replace
	label values hhroof lroof
	*floor
	recode hhfloor (1 2 6 .=1) (3/5 = 2)
	label define lfloor 1 "Mud" 2 "Concrete, Wood, Tiles", replace
	label values hhfloor lfloor
	*rooms
	recode hhrooms (0 1 . =1) (3/35 = 3)
	label define lrooms 1 "1 room" 2 "2 rooms" 3 ">2 rooms", replace
	label values hhrooms lrooms
	*light
	recode hhlight (1 2 3 8 9 10 . =1) (4 6 7 = 2) (5 = 3)
	label define llight 1 "wood/gras" 2 "fuel" 3 "electricity", replace
	label values hhlight llight
	*cook
	recode hhcook (1 2 3 8 9 10 . =1) (4 5 6 7 = 2)
	label define lcook 1 "wood/gras" 2 "fuel/electricity", replace
	label values hhcook lcook
	*electricity
	recode hhelectr (. =2)
	*generator
	recode hhgen(. =2)
	*phone
	recode hhphone (. =2)
	*drinking
	recode hhdrink (1 3 4 9 10 11 12 .=1) (2 5 6 7 8 = 2)
	label define ldrink 1 "safe" 2 "unsafe", replace
	label values hhdrink ldrink
	*toilet
	recode hhtoilet (1 2 5 7 . =1) (3 4 6 8 = 2)
	label define ltoilet 1 "unsafe" 2 "safe", replace
	label values hhtoilet ltoilet
	*waste
	recode hhwaste (7 .=1) (2 3 4 5 = 2) (6 = 3)
	label define lwaste 1 "none" 2 "legal" 3 "illegal", replace
	label values hhwaste lwaste
	*rename and reorder
	ren (ea zone state sector) (cluster strata mcat_state urban)	
	drop head_age
	ren (nchild nadult nsenior pchild psenior) mcon_=
	ren (hhwall hhroof hhfloor hhrooms hhlight hhcook hhelectr hhgen hhphone hhdrink hhtoilet hhwaste head_sex) mcat_=
	gen xdurables = 0
	*add remaining variables
	merge 1:1 hhid using "${gsdTemp}/NGA-HHFoodItems.dta",  keepusing(weight xfood*) keep(match) nogen
	merge 1:1 hhid using "${gsdTemp}/NGA-HHNonFoodItems.dta",  keepusing(xnonfood*) keep(match) nogen
	keep hhid strata urban cluster weight hhsize mcon_* mcat_* xdurables xfood* xnonfood*
	order hhid strata urban cluster weight hhsize mcon_* mcat_* xdurables xfood* xnonfood*, first
	drop if missing(weight)
	*ensure no missing values
	desc, varl
	local lv = r(varlist)
	foreach v of local lv {
		assert !missing(`v')
	}
	compress
	save "`using'", replace
}
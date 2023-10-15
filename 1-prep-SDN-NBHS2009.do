*Prepare Sudan

ma drop all
set more off
set seed 23081980

local using= "${gsdData}/SDN-NBHS2009-HHData.dta"
capture confirm file "`using'"
if _rc != 0 {
	*data directory
	local sData = "${gsdDataBox}/SDN-NBHS2009"

	import excel "`sData'/fitem.xlsx", sheet("Sheet1") firstrow clear
	labmask item, val(itemlabel)
	keep item
	merge 1:m item using "`sData'/temp_food.dta", assert(master match) keep(master match) nogen
	keep identif item hhsize hhweight value
	ren (identif item value hhweight) (hhid foodid xfood weight) 
	fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
	save "${gsdTemp}/SDN-HHFoodItems.dta", replace

	import excel "`sData'/nfitem.xlsx", sheet("Sheet1") firstrow clear
	drop if missing(item)
	labmask item, val(itemlabel)
	keep item recall
	merge 1:m item using "`sData'/temp_nonfood.dta", assert(master match) keep(master match) keepusing(identif hhsize hhweight q3 module)
	gen value = q3 if module == 4
	replace value = q3/12 if module == 5
	drop q3
	save "${gsdTemp}/nonfood.dta", replace
	keep if _merge == 1
	keep item recall
	save "${gsdTemp}/item_unmatched.dta", replace
	merge 1:m item using "`sData'/temp_energy.dta", assert(master match) keep(master match) nogen keepusing(identif hhsize hhweight v05 v07 v09 v11 v13)
	egen ey = rsum(v05 v07 v09 v11 v13)
	ren ey value
	drop v05 v07 v09 v11 v13
	append using "${gsdTemp}/nonfood.dta"
	duplicates drop identif item, force
	keep identif item hhsize hhweight value
	ren (identif item value hhweight) (hhid nonfoodid xnonfood weight) 
	fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
	save "${gsdTemp}/SDN-HHNonFoodItems.dta", replace
	
	*prepare individual data
	use "`sData'/NBHS_IND.dta", clear
	ren b41 age
	gen age_child = age<15 if age<.
	gen age_adult = inrange(age,15,64) if age<.
	gen age_senior = age>64 if age<.
	*get head variables
	gen head_age = age if b2==1
	gen head_sex = b3 if b2==1
	collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior head_age head_sex, by(hhid cluster)
	merge 1:1 hhid cluster using "`sData'/NBHS_HH.dta", nogen assert(match) keep(match) keepusing(h1 h5 h9 h3 h7 h8 h10 i* urban hhweight state) 
	ren (h1 h5 h9 h3 h7 h8 h10 hhweight) (hhhouse hhwater hhtoilet hhsleep hhlight hhcook hhwaste weight)
	ren (head_sex head_age state) (hhsex age strata)
	replace hhsex =1 if hhsex>=.
	*collect durables but we won't use them for the moment
	local li = "21 22 23 24 25 31 32 33 34 35 36 37 38 39"
	gen xdurables = 0
	foreach i of local li {
		replace xdurables = xdurables + i`i'_2 * i`i'_3 if i`i'_1==1 & (i`i'_2 * i`i'_3>0)
	}
	replace xdurables = 0 if missing(xdurables)
	drop i*
	*simplify by setting missing values to conservative answers
	*type
	recode hhhouse (1/2=1) (3/4=2) (5/20=3) (11=1) (-9=1) (.=1)
	label define lhouse 1 "Tent" 2 "Tukul" 3 "House/Apt" 4 "Other", replace
	label values hhhouse lhouse
	*sleep
	recode hhsleep (3/20=3) (-9 .=0)
	label define lsleep 0 "None" 1 "1 Room" 2 "2 Rooms" 3 ">2 Rooms", replace
	label values hhsleep lsleep
	*water
	recode hhwater (1/4=1) (5=2) (6/11=3) (11/13=4) (-9 .=4)
	label define lwater 1 "Borehole" 2 "Hand pump" 3 "Open Water" 4 "Other"
	label values hhwater lwater
	*light
	recode hhlight (1/5=1) (6/10=2) (11=3) (-9 .=3)
	label define llight 1 "Gas / Paraffin" 2 "Other material" 3 "None", replace
	label values hhlight llight
	*cook
	recode hhcook (-9=3) (3/9 .=3)
	label define lcook 1 "Firewood" 2 "Charcoal" 3 "Other", replace
	label values hhcook lcook
	*toilet
	recode hhtoilet (3/5=3) (6=4) (-9 .=4)
	label define ltoilet 1 "Pit" 2 "Shared Pit" 3 "Flush/Bucket" 4 "None", replace
	label values hhtoilet ltoilet
	*waste
	recode hhwaste (-9/2=4) (3=3) (4=2) (5=1) (6 .=4)
	label define lwaste 1 "Burning" 2 "Heap" 3 "Pit" 4 "Other"
	label values hhwaste lwaste	
	*add consumption
	merge 1:1 hhid using "${gsdTemp}/SDN-HHFoodItems.dta",  keepusing(xfood*) keep(match) nogen
	merge 1:1 hhid using "${gsdTemp}/SDN-HHNonFoodItems.dta",  keepusing(xnonfood*) keep(match) nogen
	*sort and order
	gen pchild = nchild / hhsize
	gen psenior = nsenior / hhsize
	order strata cluster hhid, first
	ren (nchild nadult nsenior pchild psenior) mcon_=
	ren (hhhouse hhsleep hhwater hhlight hhcook hhtoilet hhwaste hhsex) mcat_=
	keep hhid strata urban cluster weight hhsize mcon_* mcat_* xdurables xfood* xnonfood*
	order hhid strata urban cluster weight hhsize mcon_* mcat_* xdurables xfood* xnonfood*, first
	*ensure no missing values
	desc, varl
	local lv = r(varlist)
	foreach v of local lv {
		assert !missing(`v')
	}
	compress
	save "`using'", replace
}
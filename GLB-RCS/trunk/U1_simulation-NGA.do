*Author: Syedah Aroob Iqbal
*Reivewer: lparisotto


clear all
ma drop all
set more off
set matsize 10000
set seed 10051990
	
*data directory
local sData = "${gsdDataBox}/NGA-GHS2016"
	
	
run "${gsdDo}/fRCS.do"
*Calculating hhsize:
use "`sData'/sect1_harvestw3.dta", clear
*Drop individuals who are not living in the household.
drop if s1q4a == 2
bysort hhid: gen hhsize = _N
collapse hhsize, by(hhid)
save "${gsdTemp}/hhsize.dta", replace

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
merge m:1 hhid using "${gsdTemp}/hhsize.dta", assert(match) nogen
ren wt_wave3 weight
gen value = s10aq2
replace value = s10bq4 if missing(value)
*Aggregating by item
collapse (sum) value, by(hhid item_cd weight hhsize)
ren (item_cd value) (foodid xfood) 
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "${gsdTemp}/NGA-HHFoodItems.dta", replace

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
merge m:1 hhid using "${gsdTemp}/hhsize.dta", assert(match) nogen
ren wt_wave3 weight
collapse (sum) value, by (hhid item_cd weight hhsize)
ren (item_cd value) (nonfoodid xnonfood) 
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "${gsdTemp}/NGA-HHNonFoodItems.dta", replace
merge 1:1 hhid using "${gsdTemp}/NGA-HHFoodItems.dta",  keepusing(xfood*) keep(match) nogen
save "${gsdData}/NGA-HHData.dta", replace

local using= "${gsdData}/NGA-HHData.dta"
local nmodules = 4
local ncoref = 33
local ncorenf = 25
local ndiff= 3
local nsim = 20
local nmi = 20
local rseed = 23081980
local dirbase = "${gsdOutput}"



RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') `egalshare'

*Prepare Sudan

ma drop all
set more off
set seed 23081980

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
merge 1:1 hhid using "${gsdTemp}/SDN-HHFoodItems.dta",  keepusing(xfood*) keep(match) nogen
save "${gsdData}/SDN-NBHS2009-HHData.dta", replace

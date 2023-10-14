*prepare ETH HCE dataset

ma drop all
set more off
set seed 23081980

*data directory
local sData = "${gsdDataBox}/ETH-HCE2011"

use "`sData'/2011 HCE aggregate .dta", clear 
drop if missing(uhhid)
save "${gsdTemp}/2011_HCE_aggregate_formerge.dta", replace

use "`sData'/hces_2011_expenditure.dta", clear
*Procuring unique hhid's:
merge m:1 cq11 cq12 cq13 cq14 cq15 cq16 cq17 cq18 cq19 using "${gsdTemp}/2011_HCE_aggregate_formerge.dta",  keep(match) keepusing(uhhid fexpend nfexpend texpend apoor) nogen
*32% of the data is duplicates in terms of all variabls expect value, quantity and frequency cq11 cq12 cq13 cq14 cq15 cq16 cq17 cq18 cq19 month top1 item top2 top3 top4 type source newunit ur rep uhhid
order uhhid, before(cq11)
bysort uhhid: egen texpend_check_all = total(value)
ren (wgt uhhid cq19) (weight hhid hhsize) 
save "${gsdTemp}/hces_2011_expenditure_hhid.dta", replace
*As texpend is closer to total expenditure values without dropping duplicates. We do not drop duplicates, instead, we aggregate the values in duplicates to get a total expenditure value per item.
use "${gsdTemp}/hces_2011_expenditure_hhid.dta", clear
*Restricting the data to food categories
keep if inlist(top1,1,2)
*Aggregating by item
collapse (sum) value, by(hhid item weight hhsize)
ren (item value) (foodid xfood) 
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "${gsdTemp}/ETH-HHFoodItems.dta", replace

use "${gsdTemp}/hces_2011_expenditure_hhid.dta", clear
*Restricting the data to non-food expenditure categories
keep if inrange(top1,3,13)
collapse (sum) value, by (hhid item weight hhsize)
ren (item value) (nonfoodid xnonfood) 
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "${gsdTemp}/ETH-HHNonFoodItems.dta", replace
merge 1:1 hhid using "${gsdTemp}/ETH-HHFoodItems.dta",  keepusing(xfood*) keep(match) nogen
compress
save "${gsdData}/ETH-HCE2011-HHData.dta", replace

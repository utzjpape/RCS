*Author: Syedah Aroob Iqbal
*Reivewer: lparisotto


clear all
	ma drop all
	set more off
	set matsize 10000
	set seed 10051990
	
	*parameters
	*number of modules
	local M = 4
	*number of simulations
	local N = 20
	*number of imputations 
	local nI = 20
	*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
	local ndiff = 3
	

run "${gsdDo}/fRCS.do"
use "${gsdDataBox}/2011 HCE aggregate .dta", clear 
drop if missing(uhhid)
save "${gsdData}/2011_HCE_aggregate_formerge.dta", replace

*Incorporating manual aggregation of food items:
import excel using "${gsdDataBox}/FoodConsumption_Manual_Revisionsv1.xlsx", firstrow clear
drop top*
*save "${gsdData}/FoodConsumption_Manual_Revisions.dta", replace
*use "${gsdDataBox}/hces_2011_expenditure.dta" nogen
merge 1:m item using  "${gsdDataBox}/hces_2011_expenditure.dta", nogen
*Procuring unique hhid's:
merge m:1 cq11 cq12 cq13 cq14 cq15 cq16 cq17 cq18 cq19 using "${gsdData}/2011_HCE_aggregate_formerge.dta",  keep(match) keepusing(uhhid fexpend nfexpend texpend) nogen
/*32% of the data is duplicates in terms of all variabls expect value, quantity and frequency cq11 cq12 cq13 cq14 cq15 cq16 cq17 cq18 cq19 month top1 item top2 top3 top4 type source newunit ur rep uhhid
order uhhid, before(cq11)
bysort uhhid: egen texpend_check_all = total(value)
gen imputed = (inlist(item,4201101,4202101,4202102,4401201))
bysort uhhid imputed: egen texpend_check_minus_imp = total(value)
bysort uhhid: egen texpend_check_minus_imp1 = max(texpend_check_minus_imp)
drop texpend_check_minus_imp
ren texpend_check_minus_imp1 texpend_check_minus_imp
gen match = (floor(texpend) == floor(texpend_check_all)) | (ceil(texpend) == ceil(texpend_check_all)) | (floor(texpend) == ceil(texpend_check_all)) | (ceil(texpend) == floor(texpend_check_all))
gen diff = abs(texpend_check_all - texpend) if match == 0
gen diff2 = abs(texpend_check_minus_imp - texpend) if match == 0
gen diff_imp = abs(texpend_check_minus_imp - texpend_check_all)
gen rate_imp = diff_imp*rate
collapse texpend_check_all cq19 match, by(uhhid)
*Checking how differences in total expenditure affect poverty index
*merge 1:1 uhhid using "${gsdData}/2011_HCE_aggregate_formerge.dta",  keep(match) nogen
*merge 1:1 uhhid using 




bysort uhhid: egen fexpend_check = total(value) if top1 == 1
bysort uhhid: egen fexpend_check1 = max(fexpend_check)
replace fexpend_check = fexpend_check1
drop fexpend_check1
bysort uhhid: egen alexpend_check = total(value) if top1 == 2
bysort uhhid: egen alexpend_check1 = max(alexpend_check)
replace alexpend_check = alexpend_check1
drop alexpend_check1
gen fexpend_check2 = fexpend_check + alexpend_check
gen fmatch = (floor(fexpend) == floor(fexpend_check)) | (ceil(fexpend) == ceil(fexpend_check)) | (floor(fexpend) == ceil(fexpend_check)) | (ceil(fexpend) == floor(fexpend_check))
gen fmatch2 = (floor(fexpend) == floor(fexpend_check2)) | (ceil(fexpend) == ceil(fexpend_check2)) | (floor(fexpend) == ceil(fexpend_check2)) | (ceil(fexpend) == floor(fexpend_check2))
gen fdiff = abs(fexpend_check - fexpend) if fmatch == 0

*/

ren (wgt uhhid cq19) (weight hhid hhsize) 
save "${gsdData}/hces_2011_expenditure_hhid.dta", replace
*duplicates tag hhid item type source, gen (tag)
*collapse (sum) value, by(hhid texpend* fexpend* nfexpend* top1 item) 
*As texpend is closer to total expenditure values without dropping duplicates. We do not drop duplicates, instead, we aggregate the values in duplicates to get a total expenditure value per item.
use "${gsdData}/hces_2011_expenditure_hhid.dta", clear
*Restricting the data to food categories
keep if inlist(top1,1,2)
*Aggregating by itemn, As these are our new item codes
collapse (sum) value, by(hhid itemn Combined_itemlabel weight hhsize)
labmask itemn, values( Combined_itemlabel )
ren (itemn value) (foodid xfood) 
drop Combined_itemlabel
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "${gsdTemp}/HH-FoodItems.dta", replace

use "${gsdData}/hces_2011_expenditure_hhid.dta", clear
*Restricting the data to non-food expenditure categories
keep if inrange(top1,3,13)
collapse (sum) value, by (hhid top4 weight hhsize)
ren (top4 value) (nonfoodid xnonfood) 
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "${gsdTemp}/HH-NonFoodItems.dta", replace
merge 1:1 hhid using "${gsdTemp}/HH-FoodItems.dta",  keepusing(xfood*) keep(match) nogen
save "${gsdTemp}/HHData.dta", replace

local using= "${gsdTemp}/HHData.dta"
local nmodules = 4
local ncoref = 33
local ncorenf = 25
local ndiff= 3
local nsim = `N'
local nmi = `nI'
local rseed = 23081980
local dirbase = "${gsdOutput}"



RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') `egalshare'




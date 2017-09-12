*Author: Syedah Aroob Iqbal
*Reivewer: lparisotto


clear all
	ma drop all
	set more off
	set matsize 10000
	set seed 10051990
	
	*parameters
	*number of modules
	local M = 6
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

use "${gsdDataBox}/hces_2011_expenditure.dta", clear
*Procuring unique hhid's:
merge m:1 cq11 cq12 cq13 cq14 cq15 cq16 cq17 cq18 cq19 using "${gsdData}/2011_HCE_aggregate_formerge.dta",  keep(match) keepusing(uhhid fexpend nfexpend texpend apoor) nogen
*32% of the data is duplicates in terms of all variabls expect value, quantity and frequency cq11 cq12 cq13 cq14 cq15 cq16 cq17 cq18 cq19 month top1 item top2 top3 top4 type source newunit ur rep uhhid
order uhhid, before(cq11)
bysort uhhid: egen texpend_check_all = total(value)
ren (wgt uhhid cq19) (weight hhid hhsize) 
save "${gsdData}/hces_2011_expenditure_hhid.dta", replace
*As texpend is closer to total expenditure values without dropping duplicates. We do not drop duplicates, instead, we aggregate the values in duplicates to get a total expenditure value per item.
use "${gsdData}/hces_2011_expenditure_hhid.dta", clear
*Restricting the data to food categories
keep if inlist(top1,1,2)
*Aggregating by item
collapse (sum) value, by(hhid item weight hhsize)
ren (item value) (foodid xfood) 
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "${gsdTemp}/HH-FoodItems.dta", replace

use "${gsdData}/hces_2011_expenditure_hhid.dta", clear
*Restricting the data to non-food expenditure categories
keep if inrange(top1,3,13)
collapse (sum) value, by (hhid item weight hhsize)
ren (item value) (nonfoodid xnonfood) 
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "${gsdTemp}/HH-NonFoodItems.dta", replace
merge 1:1 hhid using "${gsdTemp}/HH-FoodItems.dta",  keepusing(xfood*) keep(match) nogen
save "${gsdTemp}/HHData.dta", replace

local using= "${gsdTemp}/HHData.dta"
local nmodules = 6
local ncoref = 33
local ncorenf = 25
local ndiff= 3
local nsim = 20
local nmi = 20
local rseed = 23081980
local dirbase = "${gsdOutput}"



RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') `egalshare'




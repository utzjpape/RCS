clear all
version 14
ma drop all
set more off
set matsize 10000
set seed 10051990

*data directory
local sData = "${gsdDataBox}/SDN-NBHS2009"

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
include "${gsdDo}/fRCS_estimate_.do"
include "${gsdDo}/fRCS_estimate_mi_.do"

import excel "`sData'/Questionnaire_2017/fitem.xlsx", sheet("Sheet1") firstrow clear
labmask item, val(itemlabel)
keep item
merge 1:m item using "`sData'/original computation/result/temp_food.dta", assert(master match) keep(master match) nogen
keep identif item hhsize hhweight value
ren (identif item value hhweight) (hhid foodid xfood weight) 
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "${gsdTemp}/SDN-HHFoodItems.dta", replace

import excel "`sData'/Questionnaire_2017/nfitem.xlsx", sheet("Sheet1") firstrow clear
drop if missing(item)
labmask item, val(itemlabel)
keep item recall
merge 1:m item using "`sData'/original computation/result/temp_nonfood.dta", assert(master match) keep(master match) keepusing(identif hhsize hhweight q3 module)
gen value = q3 if module == 4
replace value = q3/12 if module == 5
drop q3
save "${gsdTemp}/nonfood.dta", replace
keep if _merge == 1
keep item recall
save "${gsdTemp}/item_unmatched.dta", replace
merge 1:m item using "`sData'/original computation/result/temp_energy.dta", assert(master match) keep(master match) nogen keepusing(identif hhsize hhweight v05 v07 v09 v11 v13)
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
save "${gsdData}/SDN-HHData.dta", replace

local using= "${gsdData}/SDN-HHData.dta"
local nmodules = 4
local ncoref = 33
local ncorenf = 25
local ndiff= 3
local nsim = 20
local nmi = 20
local rseed = 23081980
local dirbase = "${gsdOutput}"



RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') `egalshare'




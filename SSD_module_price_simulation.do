*******************************************************************
** This dofile tests the robustness of the RCS methods depending on
** the size of the core and non-core module. The outcome variables
** can be the price indicators estimated and poverty prediction. 
** But I try the former first.

** Strategy: we try the module combinations for every 10 core items
** (like 11, 21, …61, etc.) instead of increasing one. 
** "Module item combination_SOM.do" calculates the number of items 
** in each modules. 

** 1. Simulate the deflators (and shares) ginve the number of the 
**  core items
**** Note: Since non-food deflators are not calculated in the SOM and SSD. 
****1.1. Produce the partitions given the number of core items
****1.2. Given each of the no of core items, produce a N set of 
****HH samples that randomized module allocation. 
****1.3  Geting the deflators and shares using one of N sample
****1.4. Getting the means and SE of the deflators from the N samples
** 2. Calculate the deflators (and shares) using the full-data. 
** 3. Compare the deflator from the full to the simulated deflator. 
*******************************************************************
capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SSD-module_price_simulation-2017-05-07(5-10per_100time).smcl", replace

*country
local co="SSD"

*parameters
*number of modules
local M = 4
*number of simulations
local N = 100
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*Number of items for food and non-food:
local num_f=149
local num_nf=118

*Num of core items: 11, 21,..., CN
*Num of core items: 11, 21,..., CN (If we go more than CN=71, we do not observe any consumption in any module) since core module is too big and optional module becomes too small. 


**Step0: Prepare basic data to be used

clear all
ma drop all
set more off

*data directory
local sData = "${gsdDataBox}/SSD-NBHS2009"

*deflators
local deflator_u = 1.029 // This deflator is obtained using the data to replicate the official poverty rate since we do not have the deflator in the report.
local deflator_r = .915   // This deflator is obtained using the data to replicate the official poverty rate since we do not have the deflator in the report.

*poverty line ($1.90)
local xpovline = 76.40 // $1.90 line in LCU

*Number of items for food and non-food:
local num_f=149	
local num_nf=118

*Make temp directories
local lc_sdBase = "${gsdOutput}/`co'/modsim/d`ndiff'm`M'"
capture: mkdir "${gsdOutput}/`co'/modsim"
capture: mkdir "${gsdOutput}/`co'/modsim/d`ndiff'm`M'"
local lc_sdTemp = "`lc_sdBase'/Temp"
capture: mkdir "`lc_sdTemp'"
local lc_sdOut = "`lc_sdBase'/Out"
capture: mkdir "`lc_sdOut'"

*prepare items in wide format, adding zeros for missings and conserving labels
capture: program drop fItems2RCS
program define fItems2RCS
	syntax , hhid(varname) itemid(varname) value(varname)
	* save the value labels for variables in local list
	quiet: levelsof `itemid', local(`itemid'_levels)
	foreach val of local `itemid'_levels {
		local `itemid'_`val' : label `: value label `itemid'' `val'
	}
	*create zeros for missing values
	quiet: reshape wide `value', i(`hhid') j(`itemid')
	foreach v of varlist `value'* {
		quiet: replace `v'=0 if `v'>=.
	}
	*reinstantiate labels
	foreach val of local `itemid'_levels {
		label var `value'`val' "``itemid'_`val''"
	}
end

*CREATE MODULES 
*for validation of the method, missing data is assumed to be missing
*as the consumption aggregate implicitly assumes.
*food consumption
use "`sData'/NBHS_FOOD.dta", clear
ren (item value) (foodid xfood)
include "${gsdDo}/SSD-labels.do"
label values foodid lfoodid
replace xfood=xfood*4 // The data is weekly. 
keep hhid foodid xfood
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "`lc_sdTemp'/HH-FoodItems.dta", replace
*non food consumption
use "`sData'/NBHS_NONFOOD.dta", clear
ren (item q3) (nonfoodid xnonfood)
replace nonfoodid = 83003 if nonfoodid==830031
include "${gsdDo}/SSD-labels.do"
label values nonfoodid lnonfoodid
*module=5 implies 12m recall; module=4 is a 30d recall
replace xnonfood = xnonfood / 12 if module==5
*ATTENTION: Previous code removed households with missing non-food consumption
*as it is unlikely for aperiod of 12m.
*drop if nonfoodid>=.
replace nonfoodid = 11101 if nonfoodid>=.
keep hhid nonfoodid xnonfood
collapse (sum) xnonfood, by(hhid nonfoodid)
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "`lc_sdTemp'/HH-NonFoodItems.dta", replace

*get household characteristics
use "`sData'/NBHS_IND.dta", clear
ren b41 age
gen age_child = age<15 if age<.
gen age_adult = inrange(age,15,64) if age<.
gen age_senior = age>64 if age<.
collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior, by(hhid cluster)
merge 1:1 hhid cluster using "`sData'/NBHS_HH.dta", nogen assert(match) keep(match) keepusing(h1 h5 h9 h3 h7 h8 h10 i* head_sex head_age urban hhweight) 
ren (h1 h5 h9 h3 h7 h8 h10 hhweight) (hhhouse hhwater hhtoilet hhsleep hhlight hhcook hhwaste weight)
*ren (head_sex head_age urban) (hhsex age strata) 
ren (head_sex head_age) (hhsex age)
replace hhsex =1 if hhsex>=.
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
*gen pwork = nwork / hhsize
*gen bwork = nwork>0
*collect durables but we won't use them as is for the poverty assessment. 
/*
local li = "21 22 23 24 25 31 32 33 34 35 36 37 38 39"
gen xdurables = 0
foreach i of local li {
	replace xdurables = xdurables + i`i'_2 * i`i'_3 if i`i'_1==1 & (i`i'_2 * i`i'_3>0)
}
*/
gen xdurables_pc = 0
drop i*
*simplify by setting missing values to conservative answers
*type
*recode hhhouse (1/2=1) (3/4=2) (5/20=3) (11=4) (-9=4) (.=4)
*label define lhouse 1 "Tent" 2 "Tukul" 3 "House/Apt" 4 "Other", replace // To avid too few cases in 4
recode hhhouse (1/2=3) (3/4=1) (5/20=2) (11=3) (-9=3) (.=3)
label define lhouse 1 "Tukul" 2 "House/Apt" 3 "Tent/Other", replace
label values hhhouse lhouse
*sleep
recode hhsleep (3/12=3) (-9=0)
label define lsleep 0 "None" 1 "1 Room" 2 "2 Rooms" 3 ">2 Rooms", replace
label values hhsleep lsleep
*water
*recode hhwater (1/4=1) (5=2) (6/11=3) (11/12=4) (-9=4)
recode hhwater (1/4=1) (5=2) (6/11=3) (12/13=4) (-9=4) // ST fixed typo 
label define lwater 1 "Borehole" 2 "Hand pump" 3 "Open Water" 4 "Other"
label values hhwater lwater
*light
recode hhlight (1/5=1) (6/10=2) (11=3) (-9=3)
label define llight 1 "Gas / Paraffin" 2 "Other material" 3 "None", replace
label values hhlight llight
*cook
recode hhcook (-9=3) (3/9=3)
label define lcook 1 "Firewood" 2 "Charcoal" 3 "Other", replace
label values hhcook lcook
*toilet
*recode hhtoilet (3/5=3) (6=4) (-9=4)
*label define ltoilet 1 "Pit" 2 "Shared Pit" 3 "Flush/Bucket" 4 "None", replace
recode hhtoilet (3/5=1) (6=3) (-9=3)
label define ltoilet 1 "Pit/Flush/Bucket" 2 "Shared Pit" 3 "None", replace
label values hhtoilet ltoilet
*waste
recode hhwaste (-9/2=4) (3=3) (4=2) (5=1) (6=4)
label define lwaste 1 "Burning" 2 "Heap" 3 "Pit" 4 "Other"
label values hhwaste lwaste
*add durables and food and non-food
merge 1:1 hhid using "`lc_sdTemp'/HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "`lc_sdTemp'/HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
egen totnfood=rowtotal(xnonfood*)
drop if totnfood==0 //  4 HHs with nonfood consumption=0 will be dropped. 
drop totnfood
gen deflator=`deflator_u' if urban==1
replace deflator=`deflator_r' if urban==2
gen povline=`xpovline'
save "`lc_sdTemp'/HHData.dta", replace

/*
use "`lc_sdTemp'/HHData.dta", clear
keep xfood* 
*149 food items.
use "`lc_sdTemp'/HHData.dta", clear
keep xnonfood* 
*118 non-food items.
*/

*Run ado files to be used:
include "${gsdDo}/RCS_module_simulation.do"

**Step1: Simulate the deflators (and shares) ginve the number of the core items
**Step1.1: Produce the partitions given the number of core items
**Step1.2: Given each of the no of core items, produce a N set of 
****HH samples that randomized module allocation. 
****STEP 1.3  Geting the deflators and shares using one of N sample

/*
*country
local co="SSD"

*parameters
*number of modules
local M = 4
*number of simulations
local N = 20
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*Number of items for food and non-food:
local num_f=105
local num_nf=85

*Num of core items: 11, 21,..., CN
*Num of core items: 11, 21,..., CN (If we go more than CN=71, we do not observe any consumption in any module) since core module is too big and optional module becomes too small. 
/*
local ST=35
local EN=60
*/

*/
local ST=5	
local EN=60

/*
**Step0: Prepare basic data to be used

*data directory
local sData = "${gsdDataBox}/SSD-NBHS2009"

*Make temp directories
local lc_sdBase = "${gsdOutput}/`co'/modsim/d`ndiff'm`M'"
capture: mkdir "${gsdOutput}/`co'/modsim"
capture: mkdir "${gsdOutput}/`co'/modsim/d`ndiff'm`M'"
local lc_sdTemp = "`lc_sdBase'/Temp"
capture: mkdir "`lc_sdTemp'"
local lc_sdOut = "`lc_sdBase'/Out"
capture: mkdir "`lc_sdOut'"
*/


*start RCS code
*run simulation
local using= "`lc_sdTemp'/HHData.dta"
local dirout = "${gsdOutput}/`co'/modsim/"
local nmodules = `M'
*local ncoref = 33
*local ncorenf = 25
local ndiff=`ndiff'
local nsim = `N'
local rseed = 23081980
local dirbase = "`dirout'/d`ndiff'm`nmodules'"

/*
*Prepare for labels for food and non-food item share
local itemlabel= "${gsdDo}/SSD-labels.do"

include "`itemlabel'"
*/

*Take urban to fix missing in non-food file
use "`sData'/NBHS_FOOD.dta", clear
rename item itemid

save "`lc_sdTemp'/NBHS_FOOD2.dta",replace

use "`sData'/NBHS_FOOD.dta", clear
collapse state cluster urban, by(hhid)
save "`lc_sdTemp'/NBHS_FOOD_urban.dta",replace


use "`sData'/NBHS_NONFOOD.dta", clear
merge m:1 hhid using "`lc_sdTemp'/NBHS_FOOD_urban.dta", update // missing in urban is fixed. 
replace item = 83003 if item==830031
include "${gsdDo}/SSD-labels.do"
label values item lnonfoodid
*module=5 implies 12m recall; module=4 is a 30d recall
replace q3 = q3 / 12 if module==5
*ATTENTION: Previous code removed households with missing non-food consumption
*as it is unlikely for aperiod of 12m.
*drop if nonfoodid>=.
replace item = 11101 if item>=.
collapse (sum) q3, by(hhid item urban)
rename item itemid
save "`lc_sdTemp'/NBHS_NONFOOD2.dta",replace


local foodfile "`lc_sdTemp'/NBHS_FOOD2.dta"
local nfoodfile "`lc_sdTemp'/NBHS_NONFOOD2.dta"

local model = "hhsize pchild psenior i.hhsex i.hhwater i.hhcook hhsleep i.hhhouse i.hhtoilet i.hhwaste"


forvalue csim = `ST'(5)`EN'{
display "Simulation=" as text `csim' as result "%" as text
local fcsim= ceil(`num_f'*`csim'*0.01)
local nfcsim= ceil(`num_nf'*`csim'*0.01)
display "N of food items=" as text `fcsim' as result
display "N of non-food items=" as text `nfcsim' as result

*methods
local lmethod = "sim`csim'per"

RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`fcsim') ncorenf(`nfcsim') ndiff(`ndiff')
RCS_assign using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed')

display "before simulate"
dis "`using'"
dis "Next"
dis "`dirbase'"
dis `nmodules'
dis `nsim'
dis "`lmethod'"
dis "`model'"
dis `rseed'

RCS_simulate_msim using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') lmethod("`lmethod'") model("`model'") rseed(`rseed')



*RCS_partition xfood, hhid("hhid") itemid("foodid") fweight("weight") hhsize("hhsize") nmodules(`nmodules') ncore(`ncoref') ndiff(`ndiff') `egalshare'

forvalue isim=1/`N'{
display "`isim'"

*calculate Laspeyres weights
*since we use this to impute item consumption
local lis = "food nonfood"
foreach cat of local lis {
	/*
	local lc_sdTemp "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4\Temp"
	local cat = "food" // Temp ST
	local isim=1 // Temp ST
	*/

	use "`lc_sdTemp'/`cat'-consumption_mi_`isim'.dta",clear
	**Data set is filled with all items in each household. 
	*replace block=55 if block==.
	*egen gr=group(strata ea block hh)
	
	*ensure that food and non-food have the same format
	if ("`cat'"=="food") {
		*ren price_ssp_per_kg_cleaned uprice
		merge 1:1 hhid itemid using "`foodfile'", keepusing(uv urban)
		*assert _m==1 | _merge==3
		keep if _m==3 // Food items for HH that are not used are excluded.
		drop _m
		bys hhid: egen _urban=max(urban)
		replace urban=_urban if urban==.
		drop _urban
		rename uv uprice
	}
	else {
		gen uprice = cons_value // Just a filler(dummy) for non-food price index
		merge 1:1 hhid itemid using "`nfoodfile'", keepusing(urban)
		*assert _m==1 | _merge==3
		keep if _m==3 // Food items for HH that are not used are excluded.
		drop _m
		bys hhid: egen _urban=max(urban)
		replace urban=_urban if urban==.
		drop _urban
		}

	
	keep if cons_value!=.
	drop fcons_value
	
	*gen gr=hhid
	*ensure that food and non-food have the same format
*	local li = "PL SL SC"
*	local lj = "Urban Rural IDP"
*	keep team strata ea block hh weight mod_item mod_hh itemid cons_value uprice astrata type
	*calculate core and optional module consumption per hh
	bys hhid: egen double tcore = total(cons_value) if mod_item==0
	lab var tcore "HH total core cons"
	bys hhid: egen double topt = total(cons_value) if mod_item==mod_hh
	lab var topt "HH total non-core cons"
	*add zero consumption 
*	reshape wide cons_value fcons_value, i(strata ea block hh mod_item) j(itemid)

	reshape wide cons_value uprice, i(hhid urban mod_item) j(itemid)
	foreach v of varlist cons_value* {
		replace `v' = 0 if missing(`v') & inlist(mod_item,0,mod_hh)
									 }
	*reshape long cons_value uprice, i(strata ea block hh mod_item) j(itemid)
	reshape long cons_value uprice, i(hhid mod_item) j(itemid)
	drop if missing(cons_value) 
	assert inlist(mod_item,0,mod_hh)
	*calculate item shares relative to core or optional module
	gen double denom = min(tcore,topt)
	gen double cons_share = cons_value / denom
	save "`lc_sdTemp'/`cat'-share.dta", replace
	

	*calculate weights: aggregate across households (using hh weights)
	use "`lc_sdTemp'/`cat'-share.dta", clear
	collapse (mean) cons_share denom cons_value [pweight=weight], by(itemid mod_item) // calculate means share by item, and mean (total) expenditure by modules. 
	drop if cons_share==0
	*calculate total denominator for core and all optional modules
	reshape wide denom, i(itemid cons_share) j(mod_item)
	foreach v of varlist denom?{
		egen double t`v'= max(`v')
		assert float(`v')==float(t`v') if !missing(`v')
	}
  
	egen double tdenom = rowtotal(tdenom?)
	egen double denom = rowmin(denom?)
	drop denom? tdenom?
	*save module specific shares
	ren cons_share cons_mshare
	gen cons_tshare = cons_mshare * denom / tdenom
	label var cons_mshare "Share of module consumption"
	label var cons_tshare "Share of total consumption"
	*calibrate weights to sum up to 1 
	*check whether calibration worked
	egen x = total(cons_tshare)
	assert round(x,0.001)==1
	drop x
	ren denom mdenom
	label var tdenom "Total denominator"
	label var mdenom "Module-specific denominator"
	su cons_tshare
	return li
	keep itemid cons_tshare cons_mshare
	rename cons_tshare t_rcs
	rename cons_mshare m_rcs
	replace t_rcs=t_rcs*100
	replace m_rcs=m_rcs*100
	save "`lc_sdTemp'/`cat'-weights_rcs.dta", replace
*	export excel using "weight.xlsx", sheetreplace sheet("`cat'") first(varl)


	*prepare median and average prices
	use "`lc_sdTemp'/`cat'-share.dta", clear
	*national for missing
	bysort itemid: egen up_all_med = median(uprice)
	bysort itemid: egen up_all_avg = mean(uprice)
	drop if up_all_med==. // Some items (about 10% of total consumtpion) miss prices. 
	collapse (median) uprice_med = uprice (mean) uprice_avg = uprice [pweight=weight], by(urban itemid up_all_*)
	replace uprice_med = up_all_med if missing(uprice_med)
	replace uprice_avg = up_all_avg if missing(uprice_avg)
	drop up_all_*
	*make one line per item with columns for a-strata
	reshape wide uprice_med uprice_avg, i(itemid) j(urban)
	*label
	capture: label var uprice_med1 "Avg Price of urban"
	capture: label var uprice_med2 "Avg Price of rural"
	capture: label var uprice_avg1 "Med Price of urban"
	capture: label var uprice_avg2 "Med Price of rural"
	
	*add share
	*merge 1:1 itemid using "`cat'-weights_rcs.dta", nogen assert(match) keepusing(t_rcs m_rcs) // This assertion does not hold since some items do not have unit values
	merge 1:1 itemid using "`lc_sdTemp'/`cat'-weights_rcs.dta", keepusing(t_rcs m_rcs)
	keep if _m==3
	drop _merge
	order t_rcs m_rcs, after(itemid)
	gsort -t_rcs
	save "`lc_sdTemp'/shares_rcs-`cat'.dta", replace
	*export excel itemid cons_* uprice_* using "shares.xlsx", sheetreplace sheet("`cat'") first(varl)
	*construct deflator
	keep itemid t_rcs uprice_med*
	reshape long uprice_med, i(itemid) j(urban)
	gen deflator = t_rcs * uprice_med
	collapse (sum) deflator, by(urban)
	*normalize to 1 by using the average
	egen x = mean(deflator)
	replace deflator = deflator / x
	*label var astrata "Analytical Strata"
	label var deflator "`cat' deflator"
	drop x
	save "`lc_sdTemp'/`cat'-deflator_rcs_`isim'.dta", replace
	*export excel using "deflator.xlsx", sheetreplace sheet("`cat'") first(varl)
}


use "`lc_sdTemp'/food-weights_rcs.dta",clear
gen food=1
append using "`lc_sdTemp'/nonfood-weights_rcs.dta"
replace food=0 if food==.

replace t_rcs=0 if t_rcs==.
replace m_rcs=0 if m_rcs==.

save "`lc_sdTemp'/share_rcs_`isim'.dta", replace

}

****STEP 1.4. Getting the means and SE of the deflators from the N samples

use "`lc_sdTemp'/food-deflator_rcs_1.dta",clear
rename deflator def_rcs_1
lab var def_rcs_1 "RCS deflator sim 1"

forvalue i=2/`N'{
merge 1:1 _n using "`lc_sdTemp'/food-deflator_rcs_`i'.dta"
drop _m
rename deflator def_rcs_`i'
lab var def_rcs_`i' "RCS deflator sim `i'"
}
egen def_rcs=rowmean(def_rcs_*)
egen def_rcs_sd=rowsd(def_rcs_*)
gen def_rcs_lb=def_rcs-1.96*def_rcs_sd
gen def_rcs_ub=def_rcs+1.96*def_rcs_sd
gen ncore=`csim'
lab var def_rcs "Deflator est. in full data"
lab var def_rcs_sd "RCS deflator standard error"
lab var def_rcs_lb "RCS deflator 95% CI lower bound"
lab var def_rcs_ub "RCS deflator 95% CI upper bound"
lab var ncore "Number of items in core module"
save "`lc_sdTemp'/rcs_deflator_`csim'.dta", replace
}





/*
local ST=15	
local EN=60
*/

local povline="povline"
local deflator="deflator"
local lmethod =""
/*
local num_f=149
local num_nf=118
*/

forvalue csim = `ST'(5)`EN'{
display "Simulation=" as text `csim' as result "%" as text
local fcsim= ceil(`num_f'*`csim'*0.01)
local nfcsim= ceil(`num_nf'*`csim'*0.01)
display "N of food items=" as text `fcsim' as result
display "N of non-food items=" as text `nfcsim' as result

*methods
local lmethod1 = "sim`csim'per"
local lmethod="`lmethod' `lmethod1'"
}

dis "`lmethod'"

/*
*country
local co="SSD"

*parameters
*number of modules
local M = 4
*number of simulations
local N = 20
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*Number of items for food and non-food:
local num_f=149
local num_nf=118

*Num of core items: 11, 21,..., CN
*Num of core items: 11, 21,..., CN (If we go more than CN=71, we do not observe any consumption in any module) since core module is too big and optional module becomes too small. 

**Step0: Prepare basic data to be used

*data directory
local sData = "${gsdDataBox}/SSD-NBHS2009"

*Make temp directories
local lc_sdBase = "${gsdOutput}/`co'/modsim/d`ndiff'm`M'"
capture: mkdir "${gsdOutput}/`co'/modsim"
capture: mkdir "${gsdOutput}/`co'/modsim/d`ndiff'm`M'"
local lc_sdTemp = "`lc_sdBase'/Temp"
capture: mkdir "`lc_sdTemp'"
local lc_sdOut = "`lc_sdBase'/Out"
capture: mkdir "`lc_sdOut'"

local using= "`lc_sdTemp'/HHData.dta"
local dirout = "${gsdOutput}/`co'/modsim/"
local nmodules = `M'
*local ncoref = 33
*local ncorenf = 25
local ndiff=`ndiff'
local nsim = `N'
local rseed = 23081980
local dirbase = "`dirout'/d`ndiff'm`nmodules'"
dis "`dirbase'"
local povline="povline"
local deflator="deflator"
*/

RCS_collate_msim using "`using'", dirbase("`dirbase'") nsim(`nsim') lmethod("`lmethod'")  povline(`povline') deflator(`deflator')
RCS_analyze_msim using "`using'", dirbase("`dirbase'") lmethod("`lmethod'") povline(povline) deflator(deflator)


*******************************
**Step 2: Full-sample
*******************************
local lis = "food nonfood"
foreach cat of local lis {

	use "`lc_sdTemp'//`cat'-consumption_mi_1.dta",clear  // Any `cat'-consumption_mi_*.dta has the same fulldata consumption. 
	if ("`cat'"=="food") {
		*ren price_ssp_per_kg_cleaned uprice
		merge 1:1 hhid itemid using "`foodfile'", keepusing(uv urban)
		*assert _m==1 | _merge==3
		keep if _m==3 // Keep items for HH used for poverty analysis.
		drop _m
		bys hhid: egen _urban=max(urban)
		replace urban=_urban if urban==.
		drop _urban
		rename uv uprice
	}
	else {
		gen uprice = cons_value // Just a filler(dummy) for non-food price index
		merge 1:1 hhid itemid using "`nfoodfile'", keepusing(urban)
		*assert _m==1 | _merge==3
		keep if _m==3 // Keep items for HH used for poverty analysis.
		drop _m
		bys hhid: egen _urban=max(urban)
		replace urban=_urban if urban==.
		drop _urban
		}	
	
	drop cons_value
	rename fcons_value cons_value
	
	bys hhid: egen double tcore = total(cons_value) if mod_item==0
	lab var tcore "HH total core cons"
	bys hhid mod_item: egen double topt = total(cons_value) if mod_item!=0
	lab var topt "HH total non-core cons"
	*add zero consumption 
	reshape wide cons_value uprice, i(hhid mod_item urban) j(itemid)
	foreach v of varlist cons_value* {
		replace `v' = 0 if missing(`v') 
									 }
	reshape long cons_value uprice, i(hhid mod_item urban) j(itemid)
	drop if missing(cons_value)  // None dropped
	gen double denom = min(tcore,topt)
	gen double cons_share = cons_value / denom
	replace cons_share=0 if cons_share==.
	drop if denom==0
	save "`lc_sdTemp'//`cat'-share.dta", replace

	*calculate weights: aggregate across households (using hh weights)
	use "`lc_sdTemp'//`cat'-share.dta", clear
	collapse (mean) cons_share denom cons_value [pweight=weight], by(itemid mod_item) // calculate means share by item, and mean (total) expenditure by modules. 
	reshape wide denom, i(itemid cons_share) j(mod_item)
	keep if cons_share!=0
	foreach v of varlist denom?{
		egen double t`v'= max(`v')
		assert float(`v')==float(t`v') if !missing(`v')
	}
	
	gen mod_item=0 if denom0!=.
	forvalue i=1/4 {
	replace mod_item=`i' if denom`i'!=.
					}
	egen double tdenom = rowtotal(tdenom?)
	egen double denom = rowmin(denom?)
	drop denom? tdenom?
	*save module specific shares
	ren cons_share cons_mshare
	gen aa=denom / tdenom
	ta aa
	gen cons_tshare = cons_mshare * denom / tdenom
	label var cons_mshare "Share of module consumption"
	label var cons_tshare "Share of total consumption"
	*calibrate weights to sum up to 1 
	*check whether calibration worked
	egen x = total(cons_tshare)
	assert round(x,0.001)==1
	drop x
	ren denom mdenom
	label var tdenom "Total denominator"
	label var mdenom "Module-specific denominator"
	su cons_tshare
	return li
	su cons_mshare
	return li
	keep cons_tshare cons_mshare itemid mod_item
	rename cons_tshare t_full
	rename cons_mshare m_full
	lab var t_full "Total share in full data"
	lab var m_full "Module share in full data"
	replace t_full=t_full*100
	replace m_full=m_full*100
	if ("`cat'"=="food"){
	gen food=1
		}
	if ("`cat'"=="nonfood"){
	gen food=0
		}
	save "`lc_sdTemp'//`cat'-weights_full.dta", replace
	*export excel using "weight.xlsx", sheetreplace sheet("`cat'") first(varl)

	*prepare median and average prices
	use "`lc_sdTemp'//`cat'-share.dta", clear
	*national for missing
	bysort itemid: egen up_all_med = median(uprice)
	bysort itemid: egen up_all_avg = mean(uprice)
	drop if up_all_med==. // Some items (about 10% of total consumtpion) miss prices. 
	collapse (median) uprice_med = uprice (mean) uprice_avg = uprice [pweight=weight], by(urban itemid up_all_*)
	replace uprice_med = up_all_med if missing(uprice_med)
	replace uprice_avg = up_all_avg if missing(uprice_avg)
	drop up_all_*
	*make one line per item with columns for a-strata
	reshape wide uprice_med uprice_avg, i(itemid) j(urban)
	*label
	capture: label var uprice_med1 "Avg Price of urban"
	capture: label var uprice_med2 "Avg Price of rural"
	capture: label var uprice_avg1 "Med Price of urban"
	capture: label var uprice_avg2 "Med Price of rural"
	
	*add share
	merge 1:1 itemid using "`lc_sdTemp'//`cat'-weights_full.dta", keepusing(t_full m_full)
	keep if _m==3
	drop _merge
	order t_full m_full, after(itemid)
	gsort -t_full
	save "`lc_sdTemp'//shares_full-`cat'.dta", replace
	*construct deflator
	keep itemid t_full uprice_med*
	reshape long uprice_med, i(itemid) j(urban)
	gen deflator = t_full * uprice_med
	collapse (sum) deflator, by(urban)
	*normalize to 1 by using the average
	egen x = mean(deflator)
	replace deflator = deflator / x
	*label var astrata "Analytical Strata"
	label var deflator "`cat' deflator"
	drop x
	save "`lc_sdTemp'//`cat'-deflator_full.dta", replace
}	
	

	
***********************************
** Output for Laspeyres calcualtion
***********************************	

use "`lc_sdTemp'/rcs_deflator_`ST'.dta"
*gen id=1000+foodid if foodid!=.
*replace id=2000+nonfoodid if nonfoodid!=.
local ST2=`ST'+5
forvalue csim = `ST2'(5)`EN'{
append using "`lc_sdTemp'/rcs_deflator_`csim'.dta"
}

merge m:1 urban using "`lc_sdTemp'/food-deflator_full.dta"
drop _m

order ncore urban
sort ncore urban
rename deflator def_full

gen def_full_in=0
replace def_full_in=1 if (def_rcs_lb<=def_full & def_full<=def_rcs_ub)
lab var def_full_in "Full-estimate in RCS deflator 95% CI(1=Yes)"
ta def_full_in

local lis="def_"
foreach tm of local lis {
forvalue i=1/`N'{
	gen `tm'bias_`i'=`tm'rcs_`i'-`tm'full
	gen `tm'mse_`i'=(`tm'rcs_`i'-`tm'full)^2
	}
	egen `tm'bias=rowmean(`tm'bias_*)
	egen `tm'mse=rowmean(`tm'mse_*)
	drop `tm'bias_* `tm'mse_*
	}	
				
lab var def_bias "Mean bias of RCS deflator(%)"
lab var def_mse "Mean squared error of RCS deflator(%)"

order ncore urban def_full def_rcs def_bias def_mse def_rcs_sd def_rcs_lb def_rcs_ub def_full_in 

save "`lc_sdTemp'/sim_deflator.dta", replace
export excel using "`lc_sdOut'/Sim_deflator_`co'.xls", replace first(var) 

capture: log close
	






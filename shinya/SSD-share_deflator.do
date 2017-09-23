********************************************************************
** Simulation to compare RCS food and non-food item shares to those
** in full data
** In the end, mean bias, MSE and CI of RCS shares are calculated.
********************************************************************

***********************************************************************
** In advance, run RCS_prepare and RCS_assign functions in U1_simulation-SOM.do 
** with N=100 (#simulation), and get data used for this analysis.
***********************************************************************

set more off 
set seed 123456 

*parameters
*Country
local co ="SSD"
*number of modules
local M = 4
*number of simulations
local N = 100
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*data directory
local sData = "${gsdDataBox}/SSD-NBHS2009"

*local lc_sdTemp "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\`co'\d3m4\Temp"
local lc_sdBase = "${gsdOutput}/`co'/d`ndiff'm`M'"
local lc_sdTemp = "`lc_sdBase'/Temp"
local lc_sdOut = "`lc_sdBase'/Out"

cd "`lc_sdTemp'"

*Prepare for labels for food and non-food item share
local itemlabel= "${gsdDo}/SSD-labels.do"

include "`itemlabel'"

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

***Calculate mean consumption by urban/rural for result summary
use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4\Temp\simd_MImvn.dta",clear
keep if simulation==1
merge 1:1 hhid using "`lc_sdTemp'/NBHS_FOOD_urban.dta"
drop _m
table urban [aw=weight], c(m ref) 

*****Prepare item labels to be used in the final presentation for item shares
/* Run only once to get full-label 
use "`lc_sdTemp'/food-weights_full.dta", clear
append using "`lc_sdTemp'/nonfood-weights_full.dta"

label save lfood using foodid_lab, replace
label save lnonfood using nonfoodid_lab, replace

include foodid_lab
include nonfoodid_lab


*/



/*
*For SOM:
forvalue i=1/`N'{
use "`lc_sdTemp'/`cat'-consumption_mi_`isim'.dta",clear
gen id=1000+foodid if foodid!=.
replace id=2000+nonfoodid if nonfoodid!=.
save "`lc_sdTemp'/`cat'-consumption_mi_`isim'_2.dta"
}
*/

*include "${gsdDo}/foodnfoodid_lab-`co'.do"

include "${gsdDo}/RCS_share.do"



***********************************
** Output for item weight analysis
***********************************
use "`lc_sdTemp'/food-weights_full.dta", clear
append using "`lc_sdTemp'/nonfood-weights_full.dta"

*gen id=1000+foodid if foodid!=.
*replace id=2000+nonfoodid if nonfoodid!=.
forvalue i=1/`N'{
merge 1:1 itemid using "`lc_sdTemp'/share_rcs_`i'.dta"
drop _m
rename t_rcs t_rcs_`i'
rename m_rcs m_rcs_`i'
lab var t_rcs_`i' "RCS total share estimate in simulation `i'(%)"
lab var m_rcs_`i' "RCS module share estimate in simulation `i'(%)"
}

*Prepare for labels for food and non-food item share
local itemlabel= "${gsdDo}/SSD-labels.do"

*add labels for item
include "`itemlabel'"
label value itemid litemid

order itemid food

egen t_rcs=rowmean(t_rcs_*)
egen t_rcs_sd=rowsd(t_rcs_*)
gen t_rcs_lb=t_rcs-1.96*t_rcs_sd
gen t_rcs_ub=t_rcs+1.96*t_rcs_sd

lab var t_rcs "Total share estimate in full data(%)"
lab var t_rcs_sd "RCS total share estimate standard error"
lab var t_rcs_lb "RCS total share 95% CI lower bound(%)"
lab var t_rcs_ub "RCS total share 95% CI upper bound(%)"

egen m_rcs=rowmean(m_rcs_*)
egen m_rcs_sd=rowsd(m_rcs_*)
gen m_rcs_lb=m_rcs-1.96*m_rcs_sd
gen m_rcs_ub=m_rcs+1.96*m_rcs_sd

lab var m_rcs "Share module estimate in full data(%)"
lab var m_rcs_sd "RCS module share estimate standard error"
lab var m_rcs_lb "RCS module share 95% CI lower bound(%)"
lab var m_rcs_ub "RCS module share 95% CI upper bound(%)"


gen t_full_in=0
replace t_full_in=1 if (t_rcs_lb<=t_full & t_full<=t_rcs_ub)
lab var t_full_in "Full-estimate in RCS total share 95% CI(1=Yes)"
ta t_full_in

gen m_full_in=0
replace m_full_in=1 if (m_rcs_lb<=m_full & m_full<=m_rcs_ub)
lab var m_full_in "Full-estimate in RCS module share 95% CI(1=Yes)"
ta m_full_in

gsort -food -t_full // Order items from a bigger share, starting from food and non-food

local lis="t_ m_"
foreach tm of local lis {
forvalue i=1/`N'{
	gen `tm'bias_`i'=`tm'rcs_`i'-`tm'full
	gen `tm'mse_`i'=(`tm'rcs_`i'-`tm'full)^2
	}
	egen `tm'bias=rowmean(`tm'bias_*)
	egen `tm'mse=rowmean(`tm'mse_*)
	drop `tm'bias_* `tm'mse_*
	}	
				
lab var t_bias "Mean bias of RCS tot share estimate(%)"
lab var t_mse "Mean squared error of tot RCS share estimate(%)"
lab var m_bias "Mean bias of RCS mod share estimate(%)"
lab var m_mse "Mean squared error of RCS mod share estimate(%)"

lab var mod_item "Mode of item"
lab var food "Food (=1) or non-food(=0)"
lab var itemid "Item ID"
order itemid food mod_item t_full t_rcs t_bias t_mse t_rcs_sd t_rcs_lb t_rcs_ub t_full_in m_full m_rcs m_bias m_mse m_rcs_sd m_rcs_lb m_rcs_ub m_full_in

/*
br id full bias mse rcs_sd rcs_lb rcs_ub full_in if mod_item>0
br id full bias mse rcs_sd rcs_lb rcs_ub full_in if mod_item==0
*/

cd "`lc_sdTemp'"
cd ..
cd Out
save "Share_`co'.dta", replace

/*
foreach var of var t_* m_* {
replace `var'=round(`var', 0.1)
							}
*/
export excel using "Share_`co'.xls", replace first(var) sheet("`co'_All")

keep if mod_item>0

export excel using "Share_`co'.xls", sheetreplace first(var) sheet("`co'_Noncore items")




********************************************************************
** Simulation to compare RCS food and non-food item shares, 
** estimated food index, to those in full data.
** In the end, 1) mean bias, MSE and CI of RCS shares 
** 2) Laspeyres price index, are calculated.
********************************************************************

********************************************************************
** Somalia (SOM) 
** Full-data only convers Hergasia, so we need to take prices from
** the RCS sample (except for Hergasia). 
********************************************************************

***********************************************************************
** In advance, run RCS_prepare and RCS_assign functions in U1_simulation-SOM.do 
** with N=100 (#simulation), and get data used for this analysis.
***********************************************************************

set more off 
set seed 123456 

*parameters
*Country
local co ="SOM"
*number of modules
local M = 4
*number of simulations
local N = 100
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*local lc_sdTemp "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\`co'\d3m4\Temp"
local lc_sdBase = "${gsdOutput}/`co'/d`ndiff'm`M'"
local lc_sdTemp = "`lc_sdBase'/Temp"
cd "`lc_sdTemp'"

*data directory
local sData = "${gsdDataBox}/SOM-SLHS13"

*Prep for urban 
use "`sData'/data_consumptionagg.dta", clear
collapse urban strata, by(hhid)
label value urban urban
save "`lc_sdTemp'/SOM_urban.dta",replace

use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4\Temp\simd_MImvn.dta",clear
keep if simulation==1
merge 1:1 hhid using "`lc_sdTemp'/SOM_urban.dta"
drop _m
table urban [aw=weight], c(m ref) 



	use "`sData'/food_consumption_clean.dta", clear
	rename foodid itemid
	drop uprice
	replace uv=. if uv==0 // Cleaning unit value price.
	rename uv uprice
	bysort itemid: egen up_all_med = median(uprice)
	bysort itemid: egen up_all_avg = mean(uprice)
	drop if up_all_med==. // None miss prices. 
	keep itemid up_all_med up_all_avg
	collapse uprice_med=up_all_med uprice_avg=up_all_avg, by(itemid)
	*make one line per item with columns for a-strata
	gen urban=2
	reshape wide uprice_med uprice_avg, i(itemid) j(urban)
	*label
	capture: label var uprice_med1 "Avg Price of urban"
	capture: label var uprice_med2 "Avg Price of rural"
	capture: label var uprice_avg1 "Med Price of urban"
	capture: label var uprice_avg2 "Med Price of rural"
	save "`lc_sdTemp'/food-nat-price.dta",replace
	
	
forvalue isim=1/`N'{
display "`isim'"

quiet {

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
	rename itemid foodid
	merge 1:1 hhid foodid using "`sData'/food_consumption_clean.dta", keepusing(uv)
	drop if _m==2 // Drop for non-Heresia 
	drop _m
	*ren unit_price uprice
	*ren cons_usd cons_value // done in fRCS.do
	rename foodid itemid
	replace uv=. if uv==0 // Cleaning unit value price.
	rename uv uprice
	}
	else {
	*ren pr_usd cons_value // done in fRCS.do
		*dummy for unit price
		gen uprice = cons_value
	}
	
	keep if cons_value!=.
	drop fcons_value
	
	gen urban=1 // Urban =1 if in Hargeiza
	
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
	reshape wide cons_value uprice, i(hhid mod_item) j(itemid)
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
		if ("`cat'"=="food"){
	gen food=1
	clonevar foodid=itemid
	gen nonfoodid=.
		}
	if ("`cat'"=="nonfood"){
	gen food=0
	clonevar nonfoodid=itemid
	gen foodid=.
		}
	save "`lc_sdTemp'/`cat'-weights_rcs.dta", replace
*	export excel using "weight.xlsx", sheetreplace sheet("`cat'") first(varl)

	if ("`cat'"=="food"){
	*prepare median and average prices
	use "`cat'-share.dta", clear
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
	
	merge 1:1 itemid using "`lc_sdTemp'/`cat'-nat-price.dta"
	drop if _m==2 // some items have prices at the national level. 
	drop _m
	*add share
	*merge 1:1 itemid using "`cat'-weights_rcs.dta", nogen assert(match) keepusing(t_rcs m_rcs) // This assertion does not hold since some items do not have unit values
	merge 1:1 itemid using "`cat'-weights_rcs.dta", keepusing(t_rcs m_rcs)
	keep if _m==3
	drop _merge
	order t_rcs m_rcs, after(itemid)
	gsort -t_rcs
	save "shares_rcs-`cat'.dta", replace
	*export excel itemid cons_* uprice_* using "shares.xlsx", sheetreplace sheet("`cat'") first(varl)
	*construct deflator
	keep itemid t_rcs uprice_med*
	reshape long uprice_med, i(itemid) j(urban)
	gen deflator = t_rcs * uprice_med
	collapse (sum) deflator, by(urban)
	gen sum_rP_rQ=deflator
	*normalize to 1 by using the average
	egen x = mean(deflator)
	replace deflator = deflator / x
	*label var astrata "Analytical Strata"
	label var deflator "`cat' deflator"
	drop x
	save "`cat'-deflator_rcs_`isim'.dta", replace
	*export excel using "deflator.xlsx", sheetreplace sheet("`cat'") first(varl)

	}
	
/*

	*prepare median and average prices
	use "`cat'-share.dta", clear
	
	use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4\Temp\food-share.dta", clear
	*national for missing
	bysort itemid: egen up_all_med = median(uprice)
	bysort itemid: egen up_all_avg = mean(uprice)
	collapse (median) uprice_med = uprice (mean) uprice_avg = uprice [pweight=weight], by(astrata itemid up_all_*)
	replace uprice_med = up_all_med if missing(uprice_med)
	replace uprice_avg = up_all_avg if missing(uprice_avg)
	drop up_all_*
	*make one line per item with columns for a-strata
	reshape wide uprice_med uprice_avg, i(itemid) j(astrata)
	*label
	forvalues i = 1/3 {
		forvalues j = 1/3 {
			capture: label var uprice_med`i'`j' "Avg Price `: word `i' of `li'' `: word `j' of `lj''"
			capture: label var uprice_avg`i'`j' "Med Price `: word `i' of `li'' `: word `j' of `lj''"
		}
	}
	*add share
	merge 1:1 itemid using "`cat'-weights.dta", nogen assert(match) keepusing(cons_?share)
	order cons_tshare cons_mshare, after(itemid)
	gsort -cons_tshare
	save "shares-`cat'.dta", replace
	export excel itemid cons_* uprice_* using "shares.xlsx", sheetreplace sheet("`cat'") first(varl)
	
	*construct deflator
	keep itemid cons_tshare uprice_med*
	reshape long uprice_med, i(itemid) j(astrata)
	gen deflator = cons_tshare * uprice_med
	collapse (sum) deflator, by(astrata)
	*normalize to 1 by using the average
	egen x = mean(deflator)
	replace deflator = deflator / x
	label var astrata "Analytical Strata"
	label var deflator "`cat' deflator"
	drop x
	save "`cat'-deflator.dta", replace
	export excel using "deflator.xlsx", sheetreplace sheet("`cat'") first(varl)

*/

}

use "`lc_sdTemp'/food-weights_rcs.dta",clear
append using "`lc_sdTemp'/nonfood-weights_rcs.dta"

replace t_rcs=0 if t_rcs==.
replace m_rcs=0 if m_rcs==.

gen id=1000+foodid if foodid!=.
replace id=2000+nonfoodid if nonfoodid!=.

save "`lc_sdTemp'/share_rcs_`isim'.dta", replace

}
}

*******************************
** Full-sample
*******************************
/* - Not used
local lis = "food nonfood"
foreach cat of local lis {
	*local cat = "food" // Temp ST
	*local isim=1 // Temp ST
	*use "`cat'.dta", clear

	use "`lc_sdTemp'/`cat'-consumption_mi_1.dta",clear
	**Data set is filled with all items in each household. 
	*replace block=55 if block==.
	*egen gr=group(strata ea block hh)
	
	*keep if cons_value!=.
	drop cons_value
	rename fcons_value cons_value
	bysort hhid: egen double totcons=total(cons_value)
	gen double share=cons_value/totcons
	
	collapse (mean) share [pweight=weight], by(itemid mod_item)
	
	keep share itemid mod_item
	rename share full
	replace full=full*100
	
		if ("`cat'"=="food"){
	gen food=1
	rename itemid foodid
	gen nonfoodid=.
		}
	if ("`cat'"=="nonfood"){
	gen food=0
	rename itemid nonfoodid
	gen foodid=.
		}
	save "`lc_sdTemp'/`cat'-weights_full.dta", replace
}
*/



*******************************************
* Applied the (RCS) same code that calculates both
* total and module shares for full-data. 
*******************************************



local lis = "food nonfood"
foreach cat of local lis {

/*	
*local cat = "food" // Temp ST
*parameters
*Country
local co ="SOM"
*number of modules
local M = 4
*number of simulations
local N = 100
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*local lc_sdTemp "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\`co'\d3m4\Temp"

local lc_sdBase = "${gsdOutput}/`co'/d`ndiff'm`M'"
local lc_sdTemp = "`lc_sdBase'/Temp"
cd "`lc_sdTemp'"
*/

	*use "`cat'.dta", clear
	use "`lc_sdTemp'/`cat'-consumption_mi_1.dta",clear
	**Data set is filled with all items in each household. 
	*replace block=55 if block==.
	*egen gr=group(strata ea block hh)
	
	if ("`cat'"=="food") {
	rename itemid foodid
	merge 1:1 hhid foodid using "`sData'/food_consumption_clean.dta", keepusing(uv)
	drop if _m==2 // Drop for non-Heresia 
	drop _m
	*ren unit_price uprice
	*ren cons_usd cons_value // done in fRCS.do
	rename foodid itemid
	replace uv=. if uv==0 // Cleaning unit value price.
	rename uv uprice
	}
	else {
	*ren pr_usd cons_value // done in fRCS.do
		*dummy for unit price
		gen uprice = cons_value
	}
	
	*keep if cons_value!=.
	drop cons_value
	rename fcons_value cons_value
	
	*gen gr=hhid
	*ensure that food and non-food have the same format
*	local li = "PL SL SC"
*	local lj = "Urban Rural IDP"
*	keep team strata ea block hh weight mod_item mod_hh itemid cons_value uprice astrata type
	*calculate core and optional module consumption per hh
	bys hhid: egen double tcore = total(cons_value) if mod_item==0
	lab var tcore "HH total core cons"
	bys hhid mod_item: egen double topt = total(cons_value) if mod_item!=0
	lab var topt "HH total non-core cons"
	*add zero consumption 
*	reshape wide cons_value fcons_value, i(strata ea block hh mod_item) j(itemid)
	reshape wide cons_value uprice, i(hhid mod_item) j(itemid)
	foreach v of varlist cons_value* {
		replace `v' = 0 if missing(`v') 
									 }
	*reshape long cons_value uprice, i(strata ea block hh mod_item) j(itemid)
	reshape long cons_value uprice, i(hhid mod_item) j(itemid)
	drop if missing(cons_value)  // None dropped
		*assert inlist(mod_item,0,mod_hh) // This assert does not apply. 
	*calculate item shares relative to core or optional module
	gen double denom = min(tcore,topt)
	gen double cons_share = cons_value / denom
	replace cons_share=0 if cons_share==.
	drop if denom==0
	save "`cat'-share.dta", replace

	*calculate weights: aggregate across households (using hh weights)
	*local cat = "food" // Temp ST
	use "`cat'-share.dta", clear
	collapse (mean) cons_share denom cons_value [pweight=weight], by(itemid mod_item) // calculate means share by item, and mean (total) expenditure by modules. 
	*calculate total denominator for core and all optional modules
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
	*egen double tdenom = rowtotal(tdenom?)
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
	save "`cat'-weights_full.dta", replace
	*export excel using "weight.xlsx", sheetreplace sheet("`cat'") first(varl)
	
	if ("`cat'"=="food"){
	*prepare median and average prices
	use "`cat'-share.dta", clear
	
	gen urban=1 // Urban =1 if in Hargeiza
	
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

	*Add national price
	merge 1:1 itemid using "`lc_sdTemp'/`cat'-nat-price.dta"
	drop if _m==2 // some items have prices at the national level. 
	drop _m
	
	*add share
	*merge 1:1 itemid using "`cat'-weights_rcs.dta", nogen assert(match) keepusing(t_rcs m_rcs) // This assertion does not hold since some items do not have unit values
	merge 1:1 itemid using "`cat'-weights_full.dta", keepusing(t_full m_full)
	keep if _m==3
	drop _merge
	order t_full m_full, after(itemid)
	gsort -t_full
	save "shares_full-`cat'.dta", replace
	*export excel itemid cons_* uprice_* using "shares.xlsx", sheetreplace sheet("`cat'") first(varl)
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
	save "`cat'-deflator_full.dta", replace
	*export excel using "deflator.xlsx", sheetreplace sheet("`cat'") first(varl)
	}
	
	
	}


use "`lc_sdTemp'/food-deflator_full.dta", clear
*append using "`lc_sdTemp'/nonfood-deflator_full.dta"
rename deflator def_full
*gen id=1000+foodid if foodid!=.
*replace id=2000+nonfoodid if nonfoodid!=.
forvalue i=1/`N'{
merge 1:1 _n using "`lc_sdTemp'/food-deflator_rcs_`i'.dta"
drop _m
rename deflator def_rcs_`i'
lab var def_rcs_`i' "RCS deflator sim `i'"
}

egen def_rcs=rowmean(def_rcs_*)
egen def_rcs_sd=rowsd(def_rcs_*)
gen def_rcs_lb=def_rcs-1.96*def_rcs_sd
gen def_rcs_ub=def_rcs+1.96*def_rcs_sd

lab var def_rcs "Deflator est. in full data"
lab var def_rcs_sd "RCS deflator standard error"
lab var def_rcs_lb "RCS deflator 95% CI lower bound"
lab var def_rcs_ub "RCS deflator 95% CI upper bound"

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

order urban def_full def_rcs def_bias def_mse def_rcs_sd def_rcs_lb def_rcs_ub def_full_in 

save deflator.dta, replace
export excel using "Deflator_`co'.xls", replace first(var) 
	

	
	
	
	
*parameters
*Country
local co ="SOM"
*number of modules
local M = 4
*number of simulations
local N = 100
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*local lc_sdTemp "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\`co'\d3m4\Temp"
local lc_sdBase = "${gsdOutput}/`co'/d`ndiff'm`M'"
local lc_sdTemp = "`lc_sdBase'/Temp"
cd "`lc_sdTemp'"

use "`lc_sdTemp'/food-weights_full.dta", clear
append using "`lc_sdTemp'/nonfood-weights_full.dta"

gen id=1000+itemid if food==1
replace id=2000+itemid if food==0
forvalue i=1/`N'{
merge 1:1 id using "`lc_sdTemp'/share_rcs_`i'.dta"
drop _m
rename t_rcs t_rcs_`i'
rename m_rcs m_rcs_`i'
lab var t_rcs_`i' "RCS total share estimate in simulation `i'(%)"
lab var m_rcs_`i' "RCS module share estimate in simulation `i'(%)"
}

/* Run only once to get full-label
use "nonfood-weights_full.dta",clear
label save lnonfood using nonfoodid_lab, replace

use "food-weights_full.dta",clear
label save lfood using foodid_lab, replace
*/

include "${gsdDo}/foodnfoodid_lab-`co'.do"
label value id lfoodnfood
drop foodid nonfoodid
order id food

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
lab var id "Item ID"
order id food mod_item t_full t_rcs t_bias t_mse t_rcs_sd t_rcs_lb t_rcs_ub t_full_in m_full m_rcs m_bias m_mse m_rcs_sd m_rcs_lb m_rcs_ub m_full_in

/*
br id full bias mse rcs_sd rcs_lb rcs_ub full_in if mod_item>0
br id full bias mse rcs_sd rcs_lb rcs_ub full_in if mod_item==0
*/

drop itemid

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





/* Bootstrap (to be done?)
use "`lc_sdTemp'/`cat'-consumption_mi_1.dta",clear

 program define lnsim, rclass
        version 14.2
        syntax [, obs(integer 1) mu(real 0) sigma(real 1) ]
        drop _all
        set obs `obs'
        tempvar z
        gen `z' = exp(rnormal(`mu',`sigma'))
        summarize `z'
        return scalar mean = r(mean)
        return scalar Var  = r(Var)
    end


local lis = "food nonfood"
foreach cat of local lis {
	*local cat = "food" // Temp ST
	*local isim=1 // Temp ST
	*use "`cat'.dta", clear
	**Data set is filled with all items in each household. 
	*replace block=55 if block==.
	*egen gr=group(strata ea block hh)
	
	*keep if cons_value!=.
	drop cons_value
	rename fcons_value cons_value
	bysort hhid: egen double totcons=total(cons_value)
	gen double share=cons_value/totcons
	
	collapse (mean) share [pweight=weight], by(itemid)
	
	keep share itemid
	rename share full
	replace full=full*100
	
		if ("`cat'"=="food"){
	gen food=1
	rename itemid foodid
	gen nonfoodid=.
		}
	if ("`cat'"=="nonfood"){
	gen food=0
	rename itemid nonfoodid
	gen foodid=.
		}
	save "`lc_sdTemp'/`cat'-weights_full.dta", replace
}

*/



/* Applied the (RCS) same code for full-data. - Not used


local lis = "food nonfood"
foreach cat of local lis {
	*local cat = "food" // Temp ST
	local isim=1 // Temp ST
	*use "`cat'.dta", clear
	local lc_sdTemp "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4\Temp"
	use "`lc_sdTemp'/`cat'-consumption_mi_`isim'.dta",clear
	**Data set is filled with all items in each household. 
	*replace block=55 if block==.
	*egen gr=group(strata ea block hh)
	
	*keep if cons_value!=.
	drop cons_value
	rename fcons_value cons_value
	
	*gen gr=hhid
	*ensure that food and non-food have the same format
*	local li = "PL SL SC"
*	local lj = "Urban Rural IDP"
*	keep team strata ea block hh weight mod_item mod_hh itemid cons_value uprice astrata type
	*calculate core and optional module consumption per hh
	bys hhid: egen double tcore = total(cons_value) if mod_item==0
	lab var tcore "HH total core cons"
	bys hhid: egen double topt = total(cons_value) if mod_item!=0
	lab var topt "HH total non-core cons"
	*add zero consumption 
*	reshape wide cons_value fcons_value, i(strata ea block hh mod_item) j(itemid)
	reshape wide cons_value, i(hhid mod_item) j(itemid)
	foreach v of varlist cons_value* {
		replace `v' = 0 if missing(`v') 
									 }
	*reshape long cons_value uprice, i(strata ea block hh mod_item) j(itemid)
	reshape long cons_value, i(hhid mod_item) j(itemid)
	drop if missing(cons_value) 
	*assert inlist(mod_item,0,mod_hh) // This assert does not apply. 
	*calculate item shares relative to core or optional module
	gen double denom = min(tcore,topt)
	gen double cons_share = cons_value / denom
	save "`cat'-share.dta", replace

	*calculate weights: aggregate across households (using hh weights)
	local cat = "food" // Temp ST
	use "`cat'-share.dta", clear
	by hhid: egen n=total(cons_share)
	codebook hhid if n==1 // 53 HH has only core items. 
	collapse (mean) cons_share denom cons_value [pweight=weight], by(itemid mod_item) // calculate means share by item, and mean (total) expenditure by modules. 
	drop if cons_share==0
	*calculate total denominator for core and all optional modules
	reshape wide denom, i(itemid cons_share) j(mod_item)
	foreach v of varlist denom?{
		egen double t`v'= max(`v')
		assert float(`v')==float(t`v') if !missing(`v')
	}

	*egen double tdenom = rowtotal(tdenom?)
	egen double tdenom = rowtotal(tdenom0 tdenom1)
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
	keep share itemid
	rename share full
	replace full=full*100
	clonevar id=itemid
	if ("`cat'"=="food"){
	gen food=1
		}
	if ("`cat'"=="nonfood"){
	gen food=0
		}
	save "`cat'-weights_full.dta", replace
	*export excel using "weight.xlsx", sheetreplace sheet("`cat'") first(varl)
	}


*/



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
		*ren price_ssp_per_kg_cleaned uprice
		merge 1:1 hhid itemid using "`foodfile'", keepusing(uv urban)
		*assert _m==1 | _merge==3 // _m==2 exists since we have households to drop to be consistent with poverty analysis
		drop if _m==2
		drop _m
		bys hhid: egen _urban=max(urban)
		replace urban=_urban if urban==.
		drop _urban
		rename uv uprice
	}
	else {
		gen uprice = cons_value // Just a filler(dummy) for non-food price index
		merge 1:1 hhid itemid using "`nfoodfile'", keepusing(urban)
		*assert _m==1 | _merge==3 // _m==2 exists since we have households to drop to be consistent with poverty analysis
		drop if _m==2
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
	*normalize to 1 by using the average
	egen x = mean(deflator)
	replace deflator = deflator / x
	*label var astrata "Analytical Strata"
	label var deflator "`cat' deflator"
	drop x
	save "`cat'-deflator_rcs_`isim'.dta", replace
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

}


*******************************
** Full-sample
*******************************
/*
*parameters
*Country
local co ="SSD"
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
	use "`lc_sdTemp'/`cat'-consumption_mi_1.dta",clear  // Any have the same fulldata consumption. 
	**Data set is filled with all items in each household. 
	*replace block=55 if block==.
	*egen gr=group(strata ea block hh)

	*ensure that food and non-food have the same format
	if ("`cat'"=="food") {
		*ren price_ssp_per_kg_cleaned uprice
		merge 1:1 hhid itemid using "`foodfile'", keepusing(uv urban)
		*assert _m==1 | _merge==3 // HH not used in poverty analysis need to be dropped.
		drop if _m==2
		drop _m
		bys hhid: egen _urban=max(urban)
		replace urban=_urban if urban==.
		drop _urban
		rename uv uprice
	}
	else {
		gen uprice = cons_value // Just a filler(dummy) for non-food price index
		merge 1:1 hhid itemid using "`nfoodfile'", keepusing(urban)
		*assert _m==1 | _merge==3 // HH not used in poverty analysis need to be dropped.
		drop if _m==2
		drop _m
		bys hhid: egen _urban=max(urban)
		replace urban=_urban if urban==.
		drop _urban
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
	reshape wide cons_value uprice, i(hhid mod_item urban) j(itemid)
	foreach v of varlist cons_value* {
		replace `v' = 0 if missing(`v') 
									 }
	*reshape long cons_value uprice, i(strata ea block hh mod_item) j(itemid)
	reshape long cons_value uprice, i(hhid mod_item urban) j(itemid)
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
	

***********************************
** Output for Laspeyres calcualtion
***********************************	
	
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
export excel using "`lc_sdOut'/Deflator_`co'.xls", replace first(var) 


	


*estimate optional consumption using a reduced dataset from Kenya KIHBS 2005/6
clear all
ma drop all
set more off

*number of imputations (should 100 for final results)
local nmi = 20

*********************************************************************************
*load dataset and prepare per-capita variables, quartiles and transform to logs *
*********************************************************************************
*load data file
local sf = "KEN-Example.dta"
if ("${gsdOutput}"!="") local sf = "${gsdOutput}/`sf'"
use "`sf'", clear
*create quartiles for consumption
foreach v of var xfcons0 xnfcons0 {
	xtile p`v' = `v' [pweight=weight] , n(4)
	label var p`v' "Quartiles for `: var label `v''"
}

************************************************************
*find best model in log space of all collected consumption *
************************************************************
*calculate all collected consumption in log space
egen tcons = rowtotal(xfcons* xnfcons*)
gen ltcons = log(tcons)
replace ltcons = log(.01) if missing(ltcons)
*prepare variable lists
unab mcon : mcon_*
fvunab mcat : i.mcat_*
*estimate and select best model
xi: vselect ltcons hhsize urban `mcon' `mcat' [pweight=weight], forward aicc fix(i.hhmod)
local model = "`r(predlist)'"
*output regression
reg ltcons `model' i.hhmod [pweight=weight]
*add quartiles from core consumption to model
local model = "`model' i.pxfcons0 i.pxnfcons0"
drop tcons ltcons
tempfile fh
save "`fh'", replace

***************************************************************
* prepare dataset for estimation with two-step log estimation *
***************************************************************
use "`fh'", clear
ren (xfcons0 xnfcons0) (fcore nfcore)
ren (xfcons? xnfcons?) (y1? y0?)
qui reshape long y0 y1, i(hhid) j(imod)
qui reshape long y, i(hhid imod) j(food)
*remember 0 consumption
gen y_0 = y==0 if !missing(y)
*log and regularize for zero consumption
replace y = .01 if y<=0
replace y = log(y)
*conditional step in estimation skipped if almost all hh have module consumption >0
bysort food imod: egen ny_0 = mean(y_0)
replace y_0 = 0 if ny_0 < 0.01
drop ny_0

*************************************************************************
* run estimation with two-step log estimation with multiple imputations *
*************************************************************************
mi set wide
mi register imputed y y_0
mi register regular imod food
mi register regular hh* cluster strata mcon* _I* pxfcons0 pxnfcons0
mi impute monotone (logit, augment) y_0 (reg, cond(if y_0==0)) y = `model', add(`nmi') by(imod food)
*transform into household-level dataset and out of log-space
keep hhid y y_0 _* imod food fcore nfcore
mi xeq: replace y = exp(y)
*reshape back to the hh-level
mi xeq: replace y = 0 if y_0==1
drop y_0
mi reshape wide y, i(hhid imod) j(food)
mi rename y0 xnfcons
mi rename y1 xfcons
mi reshape wide xfcons xnfcons, i(hhid) j(imod)
mi ren fcore xfcons0
mi ren nfcore xnfcons0
gen xcons = .
mi register passive xcons
quiet: mi passive: replace xcons = 0
foreach v of varlist xfcons? xnfcons? {
	quiet: mi passive: replace xcons = xcons + `v'
}
*cleaning
keep hhid xcons _*xcons _mi*
mi register imputed xcons
mi update
tempfile fh_est
save "`fh_est'", replace

*************************************************
* test results by comparing to full consumption *
*************************************************
use "`fh_est'", clear
merge 1:1 hhid using "`fh'", assert(match) nogen
* calculate FGT for all possible poverty lines
_pctile ccons [pweight=weight*hhsize], nq(100)
quiet forvalues i = 1/100 {
	local pline`i' = r(r`i')
}
gen t_fgt0 = .
gen t_fgt1 = .
mi register passive t_fgt0 t_fgt1
quiet forvalues i = 1/100 {
	*for reference
	gen r_fgt0_i`i' = ccons < `pline`i''
	gen r_fgt1_i`i' = max(`pline`i'' - ccons,0) / `pline`i''
	*for estimates
	mi passive: replace t_fgt0 = xcons < `pline`i''
	mi passive: replace t_fgt1 = max(`pline`i'' - xcons,0) / `pline`i''
	*shortcut to avoid mi collapse
	egen x_fgt0_i`i' = rowmean(_*_t_fgt0)
	egen x_fgt1_i`i' = rowmean(_*_t_fgt1)
}
mi unset
keep r_fgt* x_fgt* weight hhsize
gen id = 1
collapse (mean) r_fgt* x_fgt* [pweight=weight*hhsize], by(id)
reshape long r_fgt0_i x_fgt0_i r_fgt1_i x_fgt1_i, i(id) j(p)
label var p "Percentile Poverty Line"
ren *_i *
drop id
order p r_fgt0 x_fgt0 r_fgt1 x_fgt1
*calculate absolute differences
forvalues i = 0/1 {
	label var r_fgt`i' "FGT`i' Reference"
	label var x_fgt`i' "FGT`i' RCS"
	gen dfgt`i' = abs(r_fgt`i'-x_fgt`i')
	label var dfgt`i' "Absolute difference for FGT`i'"
}
mean dfgt*

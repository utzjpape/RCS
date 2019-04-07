*estimate optional consumption using a reduced dataset from Kenya KIHBS 2005/6

clear all
ma drop all
set more off

*PARAMETERS:
*number of imputations (should 100 for final results)
local nmi = 20
local sfdata = "${gsdOutput}/KEN-Example.dta"

*mata helper functions
cap mata mata drop vselect_best()
mata :
void vselect_best(string scalar m,string scalar ret) 
{
    X = st_matrix(m)
	k = .
	x = .
    for(i=1; i<=rows(X); i++){
        x = min((x,X[i,2]))
		if (x==X[i,2]) {
			k = X[i,1]
		}
    }
	st_local(ret,strofreal(k))
}
end

*********************************************************************************
*load dataset and prepare per-capita variables, quartiles and transform to logs *
*********************************************************************************
capture: use "`sfdata'", clear
if _rc == 601 {
	quiet: do "${gsdDo}/Create-ExKenya.do"
}
*create per capita variables
foreach v of var ccons xfcons* xnfcons* xdurables {
	quiet: gen `v'_pc = `v' / hhsize
	label var `v'_pc "`: var label `v'' per capita"
}
*create quartiles for consumption
foreach v of var xfcons0_pc xnfcons0_pc xdurables_pc {
	xtile p`v' = `v' [pweight=weight] , n(4)
	label var p`v' "Quartiles for `: var label `v''"
}

************************************************************
*find best model in log space of all collected consumption *
************************************************************
*calculate all collected consumption in log space
egen tcons = rowtotal(xfcons*_pc xnfcons*_pc)
gen ltcons = log(tcons)
replace ltcons = log(.01) if missing(ltcons)
*prepare variable lists
unab mcon : mcon_*
fvunab mcat : i.mcat_*
*estimate and select best model
xi: vselect ltcons i.strata hhsize urban i.pxdurables_pc `mcon' `mcat' [pweight=weight], best fix(i.hhmod)
matrix A = r(info)
matrix B = A[1...,colnumb("A","k")],A[1...,colnumb("A","AICC")]
mata vselect_best("B","k")
local model = "`r(best`k')'"
*output regression
reg ltcons `model' i.hhmod [pweight=weight]
*add quartiles from core consumption to model
local model = "`model' i.pxfcons0_pc i.pxnfcons0_pc"
drop tcons ltcons
tempfile fh
save "`fh'", replace

***************************************************************
* prepare dataset for estimation with two-step log estimation *
***************************************************************
use "`fh'", clear
ren (xfcons0_pc xnfcons0_pc) (fcore nfcore)
ren (xfcons?_pc xnfcons?_pc) (y1? y0?)
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
mi register regular hh* cluster strata mcon* _I* pxfcons0_pc pxnfcons0_pc pxdurables_pc
mi impute chained (logit, augment) y_0 (reg, cond(if y_0==0)) y = `model', add(`nmi') by(imod food)
*transform into household-level dataset and out of log-space
keep hhid xdurables_pc y y_0 _* imod food fcore nfcore
mi xeq: replace y = exp(y)
*reshape back to the hh-level
mi xeq: replace y = 0 if y_0==1
drop y_0
mi reshape wide y, i(hhid imod) j(food)
mi rename y0 xnfcons
mi rename y1 xfcons
mi reshape wide xfcons xnfcons, i(hhid) j(imod)
foreach v of var xfcons* xnfcons* {
	mi ren `v' `v'_pc
}
mi ren fcore xfcons0_pc
mi ren nfcore xnfcons0_pc
gen xcons_pc = .
mi register passive xcons_pc
quiet: mi passive: replace xcons_pc = xdurables_pc
foreach v of varlist xfcons?_pc xnfcons?_pc {
	quiet: mi passive: replace xcons_pc = xcons_pc + `v'
}
*cleaning
keep hhid xcons_pc _*xcons_pc _mi*
mi register imputed xcons_pc
mi update
tempfile fh_est
save "`fh_est'", replace

*************************************************
* test results by comparing to full consumption *
*************************************************
use "`fh_est'", clear
merge 1:1 hhid using "`fh'", assert(match) nogen
* calculate FGT for all possible poverty lines
_pctile ccons_pc [pweight=weight*hhsize], nq(100)
quiet forvalues i = 1/100 {
	local pline`i' = r(r`i')
}
gen t_fgt0 = .
gen t_fgt1 = .
mi register passive t_fgt0 t_fgt1
quiet forvalues i = 1/100 {
	*for reference
	gen r_fgt0_i`i' = ccons_pc < `pline`i''
	gen r_fgt1_i`i' = max(`pline`i'' - ccons_pc,0) / `pline`i''
	*for estimates
	mi passive: replace t_fgt0 = xcons_pc < `pline`i''
	mi passive: replace t_fgt1 = max(`pline`i'' - xcons_pc,0) / `pline`i''
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

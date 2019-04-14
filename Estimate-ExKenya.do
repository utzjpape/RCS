*estimate optional consumption using a reduced dataset from Kenya KIHBS 2005/6

clear all
ma drop all
set more off

*PARAMETERS:
*number of imputations (should 100 for final results)
local nmi = 10
local sfdata = "${gsdOutput}/KIHBS2015P-Example_c0-m2.dta"

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
	quiet: do "${gsdDo}/Create-ExKenya.do" 0 2
}
*create per capita variables
foreach v of var ccons xfcons* xnfcons* xdurables {
	quiet: replace `v' = `v' / hhsize
	label var `v' "`: var label `v'' per capita"
}
tempfile fh
save "`fh'", replace

************************************************************
*find best model in log space of all collected consumption *
************************************************************
use "`fh'", clear
*create class for model selection and estimation
capture classutil drop .re
.re = .RCS_estimator.new
*prepare variable lists
unab mcon : mcon_*
fvunab mcat : i.mcat_*
.re.prepare hhsize urban `mcon' `mcat', hhid("hhid") weight("weight") hhmod("hhmod") cluster("cluster") xfcons("xfcons") xnfcons("xnfcons") fix("i.strata") nmi(`nmi')
tempfile fest
esttab using "`fest'.csv", r2 ar2 aic replace
eststo clear

************************************************************
*run estimation *
************************************************************
.re.estimate ,method("mi_2cel")
gen xcons = .
mi register passive xcons
quiet: mi passive: replace xcons = xdurables
foreach v of varlist xfcons? xnfcons? {
	quiet: mi passive: replace xcons = xcons + `v'
}
*cleaning
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

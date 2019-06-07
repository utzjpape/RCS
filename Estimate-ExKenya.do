*estimate optional consumption using a reduced dataset from Kenya KIHBS 2005/6

ma drop all
set more off
set maxiter 100

*PARAMETERS:
*number of imputations (should 100 for final results)
local core = 10
local nmodules = 5
local random = 0
local nmi = 10
local sfdata = "${gsdOutput}/KIHBS2015P-Example_c`core'-m`nmodules'-r`random'.dta"

*********************************************************************************
*load dataset and prepare quartiles and transform to logs *
*********************************************************************************
capture: use "`sfdata'", clear
if _rc == 601 {
	quiet: do "${gsdDo}/Create-ExKenya.do" `core' `nmodules' `random'
}

************************************************************
*find best model in log space of all collected consumption *
************************************************************
use "`sfdata'", clear
*prepare variable lists
unab mcon : mcon_*
fvunab mcat : i.mcat_*
*read saved model (as finding the model can take a while)
capture: confirm file "`sfdata'-model.txt"
if _rc==0 {
		capture file close fhm
		file open fhm using "`sfdata'-model.txt", read
		file read fhm model 
		file read fhm logmodel
		file close fhm
		quiet: xi `mcat'
}
else {
	local model = ""
	local logmodel = ""
}
*create class for model selection and estimation
capture classutil drop .re
.re = .RCS_estimator.new
.re.prepare , hhid("hhid") weight("weight") hhmod("hhmod") cluster("cluster") xfcons("xfcons") xnfcons("xnfcons") nmi(`nmi')
.re.select_model hhsize urban `mcon' `mcat', model("`model'") logmodel("`logmodel'") method("forward aicc")
*save model for next use
capture file close fhm
file open fhm using "`sfdata'-model.txt", replace write
file write fhm "`.re.model'" _n "`.re.logmodel'"
file close fhm

************************************************************
*run estimation *
************************************************************
.re.est_mi_2cel
gen xcons = .
mi register passive xcons
quiet: mi passive: replace xcons = 0
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
merge 1:1 hhid using "`sfdata'", assert(match) nogen
keep _*_xcons ccons _mi_miss weight
ren _*_xcons xcons*
mi unset
egen id = seq()
reshape long xcons, i(id) j(sim)
*draw consumption distribution
twoway (kdensity ccons [aweight=weight]) (kdensity xcons [aweight=weight])

use "`fh_est'", clear
merge 1:1 hhid using "`sfdata'", assert(match) nogen
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
	gen zfgt`i' = r_fgt`i'-x_fgt`i'
	gen dfgt`i' = abs(zfgt`i')
	label var dfgt`i' "Absolute difference for FGT`i'"
}
mean dfgt*
graph twoway (line zfgt0 p) (line zfgt1 p) 


if (1==2) {
	use "`fh_est'", clear
	merge 1:1 hhid using "`sfdata'", assert(match) nogen
	levelsof hhmod, local(lmod)
	local lf = "f nf"
	quiet: mi passive: gen zcons = xfcons0 + xnfcons0
	foreach imod of local lmod {
		foreach f of local lf {
			mean x`f'cons`imod' [pweight=weight]
			matrix X = e(b)
			local x = X[1,1]
			mi estimate: mean x`f'cons`imod' if hhmod != `imod' [pweight=weight]
			matrix Z = e(b_mi)
			local z = Z[1,1]
			di "Factor for imod=`imod' and `f': `=`x'/`z''"
			quiet: mi passive: replace zcons = zcons + x`f'cons`imod' if hhmod==`imod'
			quiet: mi passive: replace zcons = zcons + (x`f'cons`imod' / `z' * `x') if hhmod!=`imod'
		}
	}
}

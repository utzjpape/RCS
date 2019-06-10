cap program drop mkcorr
program define mkcorr
	 version 4.0 

	 local k = rowsof(P)
	 matrix A = cholesky(P)

	 local i 1 
	 quietly {
		 while `i'<=`k' {
			 gen c`i' = invnorm(uniform())
			 local i=`i'+1
		 }
		 local i 1
		 while `i'<=`k' {
			 matrix row = A[`i',.]
			 matrix score y`i' = row
			 local i=`i'+1
		 }
		 local i 1 
		 while `i' <= `k' {
			 drop c`i'
			 local i=`i'+1
		 }
	 }
end

clear
*parameters
local nm = 3
local nmi = 10
local p = .25
*correlation matrix
set obs 6000
local nd = 2*`nm'
matrix P = 2 * matuniform(`nd',`nd')
matrix J = J(`nd',`nd',0)
matrix P = (P-J)' * (P-J)
matrix D = diag(vecdiag(P))
*matrix P = P-D + I(`nd')
*generate data
mkcorr
egen hhmod = seq(), from(1) to(`nm')
forvalues i = 1/`nd' {
	gen mcon_`i' = runiform()
	if `i'<=`nm' {
		gen cfcons`i' = exp(y`i') + mcon_`i'
		gen xfcons`i' = cfcons`i' if hhmod==mod(`i'-1,3)+1
	}
	else {
		gen cnfcons`=`i'-`nm'' = exp(y`i') + mcon_`i' 
		gen xnfcons`=`i'-`nm'' = cnfcons`=`i'-`nm'' if hhmod==mod(`i'-1,3)+1
	}
}
egen ccons = rowtotal(cfcons* cnfcons*)
gen xfcons0 = exp(runiform()) + `p' * ccons / 2
gen xnfcons0 = exp(runiform()) + `p' * ccons / 2
replace ccons = ccons + xfcons0 + xnfcons0
gen cfcons0 = xfcons0
gen cnfcons0 = xnfcons0
drop y*
gen weight = 1
gen hhsize = 3
gen strata = 1
gen cluster = 1
egen hhid = seq()
order *, alpha
order hhid strata cluster weight hhmod hhsize ccons cfcons* cnfcons* mcon_* xfcons? xnfcons?
tempfile fh
save "`fh'", replace

*prepare variable lists
unab mcon : mcon_*
*create class for model selection and estimation
capture classutil drop .re
.re = .RCS_estimator.new
.re.prepare , hhid("hhid") weight("weight") hhmod("hhmod") cluster("cluster") xfcons("xfcons") xnfcons("xnfcons") nmi(`nmi')
.re.select_model `mcon', method("forward aicc")

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
merge 1:1 hhid using "`fh'", assert(match) nogen
keep _*_xcons ccons _mi_miss weight
ren _*_xcons xcons*
mi unset
egen id = seq()
reshape long xcons, i(id) j(sim)
*draw consumption distribution
twoway (kdensity ccons [aweight=weight]) (kdensity xcons [aweight=weight])

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
	gen zfgt`i' = r_fgt`i'-x_fgt`i'
	gen dfgt`i' = abs(zfgt`i')
	label var dfgt`i' "Absolute difference for FGT`i'"
}
mean dfgt*
graph twoway (line zfgt0 p) (line zfgt1 p) 

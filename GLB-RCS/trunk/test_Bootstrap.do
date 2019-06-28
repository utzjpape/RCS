*simulate mi for simple model
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
capture classutil drop .re
.re = .RCS_estimator.new
.re.prepare , hhid("hhid") weight("weight") hhmod("hhmod") cluster("cluster") xfcons("xfcons") xnfcons("xnfcons") nmi(`nmi')
.re.select_model hhsize urban `mcon' `mcat', model("`model'") logmodel("`logmodel'") method("forward aicc")
*save model for next use
capture file close fhm
file open fhm using "`sfdata'-model.txt", replace write
file write fhm "`.re.model'" _n "`.re.logmodel'"
file close fhm
drop xfcons* xnfcons*

gen y = log(ccons)
reg y `.re.logmodel'
predict yhat, xb
gen e = y-yhat
summ e

expand 2, gen(new)
replace y=. if new
mi set wide
mi register imputed y
mi register regular mcon* _Imcat*
mi impute reg y `.re.logmodel', add(50)
ren _*_y x*
mi unset
keep hhid y x*
reshape long x, i(hhid y) j(mi)
bysort hhid: egen ym = mean(x)
gen e = y-ym
summ e

drop y yhat e

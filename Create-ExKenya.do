*prepares example dataset
local using= "${gsdData}/KEN-HHDatared.dta"
capture confirm file "`using'"
if _rc != 0 {
	quiet: do "${gsdDo}/prep-KEN.do"
}

*create input dataset for simulation
local core = `1'
local nm = `2'
local sbase = "${gsdOutput}/KEN-Example_c`core'-m`nm'"
capture classutil drop .r
.r = .RCS.new
.r.prepare using "`using'", dirbase("`sbase'") nmodules(`nm') ncoref(`core') ncorenf(`core') ndiff(3)
.r.mask , nsim(1)

*remove variables not needed and label
use "`sbase'/Temp/mi_1.dta", clear
gen ccons = cfcons + cnfcons + xdurables
drop id_hh rfcons rnfcons cfcons cnfcons xfitem* xnfitem* bfitem* bnfitem* *_pc
order hhid strata urban cluster weight hhmod hhsize ccons xfcons* xnfcons* xdurables mcat* mcon*
label var hhid "Unique household ID"
label var hhmod "Assigned optional module"
label var ccons "Full consumption (only used for checking results)"
forvalues i = 0/`nm' {
	label var xfcons`i' "Food consumption in module `i'"
	label var xnfcons`i' "Non-food consumption in module `i'"
}
save "${gsdOutput}/KEN-Example_c`core'-m`nm'.dta", replace

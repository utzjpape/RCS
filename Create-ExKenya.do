*prepares example dataset
*which survey? choose 2005P 2015P 2015C
local s= "2015P"
local red=""
local using= "${gsdData}/KEN-KIHBS`s'-HHData`red'.dta"
capture confirm file "`using'"
if _rc != 0 {
	quiet: do "${gsdDo}/prep-KEN-KIHBS.do"
}

*create input dataset for simulation
if "`1'"=="" local core = 0
else local core = `1'
if "`2'"=="" local nm = 2
else local nm = `2'
local sbase = "${gsdOutput}/KEN-Example_c`core'-m`nm'"
capture classutil drop .r
.r = .RCS.new
.r.prepare using "`using'", dirbase("`sbase'") nmodules(`nm') ncoref(`core') ncorenf(`core') ndiff(3)
.r.mask , nsim(1)

*remove variables not needed and label
use "`sbase'/Temp/mi_1.dta", clear
drop rfcons rnfcons cfcons cnfcons xfitem* xnfitem* bfitem* bnfitem*
order hhid strata urban cluster weight hhmod hhsize ccons xfcons* xnfcons* xdurables mcat* mcon*
label var hhid "Unique household ID"
label var hhmod "Assigned optional module"
label var ccons "Full consumption per capita (only used for checking results)"
forvalues i = 0/`nm' {
	label var xfcons`i' "Food consumption in module `i' per capita"
	label var xnfcons`i' "Non-food consumption in module `i' per capite"
}
drop px*
save "${gsdOutput}/KIHBS`s'-Example_c`core'-m`nm'.dta", replace

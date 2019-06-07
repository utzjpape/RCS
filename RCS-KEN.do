*SIMULATE RCS FOR KENYA

ma drop all
set more off
set maxiter 100

*parameters
*define as 'red' if using reduced dataset
local red = ""
*which survey? choose 2005P 2015P 2015C
local s= "2005P"

local using= "${gsdData}/KEN-KIHBS`s'-HHData`red'.dta"
capture confirm file "`using'"
if _rc != 0 {
	quiet: do "${gsdDo}/prep-KEN-KIHBS.do"
}

*start RCS code
*number of simulations (should be 20)
local nsim = 1
*number of imputations (should be 50)
local nmi = 50
*methods
local lmethod = "med avg reg tobit mi_reg mi_2cel"
local lk = "0 1 2 3 4 5 10 20 50"
local lk = "0 1 2 3 4 5"

*run for best method over different number of modules and core
foreach kc of local lk {
	forvalues km = 2/9 {
		local dirbase = "${gsdOutput}/KEN-KIHBS`s'`red'-c`kc'-m`km'"
		*create instance to run RCS simulations
		capture classutil drop .r
		.r = .RCS.new
		*.r.test
		.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`km') ncoref(`kc') ncorenf(`kc') nsim(`nsim')
		.r.mask
		if (`kc'==0) | (`km'==2) .r.estimate , lmethod("`lmethod'") nmi(`nmi')
		else .r.estimate , lmethod("mi_2cel") nmi(`nmi')
		.r.collate
		.r.analyze
		gen kc = `kc'
		label var kc "Parameter: number of core items"
		gen km = `km'
		label var km "Parameter: number of modules"
		tempfile fh`kc'_`km'
		save "`fh`kc'_`km''", replace
	}
}
clear
foreach kc of local lk {
	forvalues km = 2/9 {
		append using "`fh`kc'_`km''"
	}
}
table method km metric if indicator=="fgt0" & kc==0, c(mean p) format(%9.2f)
table method kc metric if indicator=="fgt0" & km==2, c(mean p) format(%9.2f)
save "${gsdOutput}/KEN-KIHBS`s'.dta", replace

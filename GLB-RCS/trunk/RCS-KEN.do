*SIMULATE RCS FOR KENYA

ma drop all
set more off
set maxiter 100

*parameters
*Poverty line from povcalnet
local xpovline = 1.90 * 35.4296
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
*number of modules
local nmodules = 2
*number of simulations (should be 20)
local nsim = 5
*number of imputations (should be 50)
local nmi = 25
*methods
local lmethod = "med avg reg tobit mi_ce mi_reg mi_regl mi_2ce mi_2cel"

local lk = "1 5 10 20 50"
local lk = "0 50"

*run over different number of core
foreach k of local lk {
	local ncoref = `k'
	local ncorenf = `k'
	local dirbase = "${gsdOutput}/KEN-KIHBS`s'`red'-c`k'-m`nmodules'"
	
	*create instance to run RCS simulations
	capture classutil drop .r
	.r = .RCS.new
	*.r.test
	.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(3)
	.r.mask , nsim(`nsim')
	.r.estimate , lmethod("`lmethod'") nmi(`nmi')
	.r.collate
	.r.analyze
}

*run over different number of modules
forvalues k = 2/9 {
	local ncoref = 0
	local ncorenf = 0
	local dirbase = "${gsdOutput}/KEN-KIHBS`s'`red'-c0-m`k'"
	
	*create instance to run RCS simulations
	capture classutil drop .r
	.r = .RCS.new
	*.r.test
	.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`k') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(3)
	.r.mask , nsim(`nsim')
	.r.estimate , lmethod("`lmethod'") nmi(`nmi')
	.r.collate
	.r.analyze
	gen k = `k'
	tempfile fh`k'
	save "`fh`k''", replace
}
clear
forvalues k = 2/9 {
	append using "`fh`k''"
}
table method k metric if indicator=="fgt0", c(mean p) format(%9.2f)



error 1

	local dirbase = "${gsdOutput}/KEN-KIHBS`s'`red'-c0-m2"
	capture classutil drop .r
	.r = .RCS.new
	*.r.test
	.r.prepare using "`using'", dirbase("`dirbase'") nmodules(2) ncoref(0) ncorenf(0) ndiff(3)
	.r.mask , nsim(2)
	.r.estimate , lmethod("avg tobit mi_reg") nmi(5)
	.r.collate
	.r.analyze

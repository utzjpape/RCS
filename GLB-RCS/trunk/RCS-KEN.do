*SIMULATE RCS FOR KENYA

clear all
ma drop all
set more off
set maxiter 100

*parameters
*Poverty line from povcalnet
local xpovline = 1.90 * 35.4296
*define as 'red' if using reduced dataset
local red = "red"

local using= "${gsdData}/KEN-HHData`red'.dta"
capture confirm file "`using'"
if _rc != 0 {
	quiet: do "${gsdDo}/prep-KEN.do"
}

*start RCS code
*number of modules
local nmodules = 2
*number of simulations (should be 20)
local nsim = 10
*number of imputations (should be 50)
local nmi = 25
*methods
local lmethod = "med avg reg tobit mi_ce mi_reg mi_regl mi_2ce mi_2cel"

local lk = "1 5 10 20 50"
local lk = "0 50"

*run over different p
foreach k of local lk {
	local ncoref = `k'
	local ncorenf = `k'
	local dirbase = "${gsdOutput}/KEN`red'-c`k'-m`nmodules'"
	
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

*SIMULATE RCS FOR KENYA

clear all
ma drop all
set more off

*parameters
*Poverty line from povcalnet
local xpovline = 1.90 * 35.4296

quiet: do "${gsdDo}/prep-KEN.do"

*start RCS code
local model = "hhsize pchild psenior i.hhsex i.hhtoilet i.hhtenure i.hhhouse i.hhcook hhrooms i.strata"
*number of modules
local nmodules = 3
*number of simulations (should be 20)
local nsim = 10
*number of imputations (should be 50)
local nmi = 50
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3
*methods
local lmethod = "med avg reg tobit mi_ce mi_treg mi_reg mi_regl mi_2ce mi_2cel"

*other parameters
local using= "${gsdData}/KEN-HHData.dta"
local shares = "demo"
local povline = `xpovline'
local rseed = 23081980

*read library
quiet: include "${gsdDo}/fRCS.do"
quiet: include "${gsdDo}/fRCS_estimate_.do"
quiet: include "${gsdDo}/fRCS_estimate_mi_.do"

local lk = "1 5 10 20 50"
local lk = "1 50"

*run over different p
foreach k of local lk {
	local ncoref = `k'
	local ncorenf = `k'
	local dirbase = "${gsdOutput}/KEN-c`k'-m`nmodules'"
	RCS_run using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`k') ndiff(`k') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") povline(`povline') model("`model'") shares(`shares') rseed(`rseed')
}

*how to continue:
*  just use a core module with all items, and then do out-of-sample prediction and check accuracy depending on sample size
*  also try for very small cores (to see whether problem with the 0s)

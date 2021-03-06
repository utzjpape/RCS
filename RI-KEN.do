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
local model = "hhsize pchild psenior i.hhsex i.hhtoilet i.hhtenure i.hhhouse i.hhcook hhrooms i.strata i.item i.item#i.hhsex i.item#i.hhtoilet i.item#i.hhtenure i.item#i.hhhouse i.item#i.hhcook i.item#hhrooms i.item#i.strata"
*number of modules
local nmodules = 1
*number of simulations (should be 20)
local nsim = 5
*number of imputations (should be 50)
local nmi = 50
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3
*methods
local lmethod = "ri_mi_par"
local lmethod = "ri_mi_par"
*other parameters
local using= "${gsdData}/KEN-HHData.dta"
local ncoref = 5
local ncorenf = 5
local ndiff=`ndiff'
local povline = `xpovline'
local lmethod = "`lmethod'"
local rseed = 23081980

*read library
quiet: include "${gsdDo}/fRCS.do"
quiet: include "${gsdDo}/fRCS_estimate_.do"
quiet: include "${gsdDo}/fRCS_estimate_ri_.do"
quiet: include "${gsdDo}/fRCS_estimate_mi_.do"

local lprob = "4 5 6"

*run over different p
foreach prob of local lprob {
	local p = `prob'/10
	local dirbase = "${gsdOutput}/KEN-`lmethod'-p`prob'"
	RCS_run using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') p(`p') lmethod("`lmethod'") povline(`povline') model("`model'") egalshare rseed(`rseed')
	*RCS_estimate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'") rseed(`rseed')
	gen long prob = `prob'
	save "${gsdTemp}/KEN-simdiff-`lmethod'-p`prob'.dta", replace
}

*collect
clear
gen long prob=0
save "${gsdTemp}/simdiff.dta", replace
foreach prob of local lprob {
	use "${gsdTemp}/KEN-simdiff-`lmethod'-p`prob'.dta", clear
	drop prob
	gen long prob = `prob'
	append using "${gsdTemp}/simdiff.dta"
	save "${gsdTemp}/simdiff.dta", replace
}
drop if method=="red"
reshape wide p,i(x prob) j(method) string

*analyze
gen d = abs(pref - p`lmethod')
mean d, over(prob)
twoway (function y=x) (line pref p`lmethod', sort), by(prob)
drop d
reshape wide p`lmethod', i(x pref) j(prob)
twoway (function y=x, lpattern(dash) lcolor(gs5)) (line p`lmethod'? pref, sort)

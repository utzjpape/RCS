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
local lmethod = "ritem_par"
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
quiet: include "${gsdDo}/fRCS_estimate_ritem_.do"
quiet: include "${gsdDo}/fRCS_estimate_mi_.do"

*run over different p
forvalues prob = 9(-1)1 {
	local p = `prob'/10
	local dirbase = "${gsdOutput}/KEN-d`ndiff'm`nmodules'p`prob'"
	RCS_run using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') p(`p') lmethod("`lmethod'") povline(`povline') model("`model'") egalshare rseed(`rseed')
	gen long prob = `prob'
	save "${gsdTemp}/simdiffp`prob'.dta", replace
}

*collect
clear
gen long prob=0
save "${gsdTemp}/simdiff.dta", replace
forvalues prob = 9(-1)1 {
	use "${gsdTemp}/simdiffp`prob'.dta", clear
	drop prob
	gen long prob = `prob'
	append using "${gsdTemp}/simdiff.dta"
	save "${gsdTemp}/simdiff.dta", replace
}
drop if method=="red"
reshape wide p,i(x prob) j(method) string

*analyze
gen d = abs(pref - pritem_ujp)
mean d, over(prob)
twoway (function y=x) (line pref pritem_ujp, sort), by(prob)
drop d
reshape wide pritem_ujp, i(x pref) j(prob)
twoway (function y=x, lpattern(dash) lcolor(gs5)) (line pritem_ujp? pref, sort)

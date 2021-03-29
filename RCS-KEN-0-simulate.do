*SIMULATE RCS FOR KENYA

ma drop all
set more off
set maxiter 100

local using0= "${gsdData}/KEN-KIHBS2015P-HHData.dta"
local using1 = "${gsdData}/KEN-KIHBS-HHData.dta"
capture confirm file "`using0'"
if _rc != 0 {
	quiet: do "${gsdDo}/prep-KEN-KIHBS.do"
}
capture confirm file "`using1'"
if _rc != 0 {
	use "`using0'", clear
	append using "${gsdData}/KEN-KIHBS2005P-HHData.dta", gen(train)
	save "`using1'", replace
}

*test run
*local train = 1
*capture classutil drop .r
*.r = .RCS.new
*.r.prepare using "`using`train''", dirbase("${gsdOutput}/KEN-KIHBS-test") nmodules(20) ncoref(0) ncorenf(0) nsim(2) train(`train')
*.r.mask
*.r.estimate , lmethod("avg") nmi(5)
*.r.collate
*.r.analyze

*start RCS code
cap: prog drop callRCS
program define callRCS
	syntax using/, t(integer) kc(integer) km(integer)
	*number of simulations (should be 20)
	local nsim = 20
	*number of imputations (should be 50)
	local nmi = 50
	*methods
	local lmethod = "med avg reg tobit mi_reg mi_2cel swift2"
	local dirbase = "${gsdOutput}/KEN-KIHBS-c`kc'-m`km'-t`t'"
	*create instance to run RCS simulations
	capture classutil drop .r
	.r = .RCS.new
	if (((`kc'==0) | (`km'==2)) & (`t'==0) & (`km'<=10)) {
		.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`km') ncoref(`kc') ncorenf(`kc') nsim(`nsim') train(`t')
		.r.mask
		.r.estimate , lmethod("`lmethod'") nmi(`nmi')
	}
	else if (`km'>10) {
		.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`km') ncoref(`kc') ncorenf(`kc') nsim(1) train(`t')
		.r.mask
		.r.estimate , lmethod("avg") nmi(1)
	}
	else {
		.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`km') ncoref(`kc') ncorenf(`kc') nsim(`nsim') train(`t')
		.r.mask
		.r.estimate , lmethod("mi_2cel swift2") nmi(`nmi')
	}
	.r.collate
	.r.analyze , force
	gen kc = `kc'
	label var kc "Parameter: number of core items"
	gen km = `km'
	label var km "Parameter: number of modules"
	gen t = `t'
	label var t "Used training set"
	save "`dirbase'.dta", replace
end

*iterations
local lc = "0 1 3 5 10 20"
local lm = "2 4 6 8 10 12 15 20 25 30 40 50"
*run for best method over different number of modules and core
*forvalues t = 0/1 {
local t = 1
	*determine whether we use a 2005 for training and run imputations on 2015
*	foreach kc of local lc {
local kc = 0
		foreach km of local lm {
			callRCS using "`using`t''",t(`t') kc(`kc') km(`km')
		}
*	}
*}

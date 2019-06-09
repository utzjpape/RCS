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
local train = 1
capture classutil drop .r
.r = .RCS.new
.r.prepare using "`using`train''", dirbase("${gsdOutput}/KEN-KIHBS-test") nmodules(2) ncoref(10) ncorenf(10) nsim(2) train(`train') erase
.r.mask
.r.estimate , lmethod("avg mi_2cel") nmi(5)
.r.collate
.r.analyze

*start RCS code
*number of simulations (should be 20)
local nsim = 5
*number of imputations (should be 50)
local nmi = 50
*methods
local lmethod = "med avg reg tobit mi_reg mi_2cel"
local lc = "0 1 3 5 10 20 50"
local lm = "2 4 6 9"

*run for best method over different number of modules and core
forvalues t = 0/1 {
	*determine whether we use a 2005 for training and run imputations on 2015
	foreach kc of local lc {
		foreach km of local lm {
			local dirbase = "${gsdOutput}/KEN-KIHBS-c`kc'-m`km'-t`t'"
			*create instance to run RCS simulations
			capture classutil drop .r
			.r = .RCS.new
			.r.prepare using "`using`t''", dirbase("`dirbase'") nmodules(`km') ncoref(`kc') ncorenf(`kc') nsim(`nsim') train(`t')
			.r.mask
			if (((`kc'==0) | (`km'==2)) & `t'==0) .r.estimate , lmethod("`lmethod'") nmi(`nmi')
			else .r.estimate , lmethod("mi_2cel") nmi(`nmi')
			.r.collate
			.r.analyze
			gen kc = `kc'
			label var kc "Parameter: number of core items"
			gen km = `km'
			label var km "Parameter: number of modules"
			gen t = `t'
			label var t "Used training set"
			tempfile fh`kc'_`km'_`t'
			save "`fh`kc'_`km'_`t''", replace
		}
	}
	clear
	foreach kc of local lc {
		forvalues km = 2/9 {
			append using "`fh`kc'_`km'_`t''"
		}
	}
	save "${gsdOutput}/KEN-KIHBS-t`t'.dta", replace
}

*analysis without training set
use "${gsdOutput}/KEN-KIHBS-t0.dta", clear
replace p = abs(p) if metric == "bias"
*estimation technique comparison
table method metric kc if !inlist(method,"llo","red") & indicator=="fgt0" & inlist(metric,"bias","cv"), by(km) c(mean p) format(%9.2f)
table method metric kc if !inlist(method,"llo","red") & indicator=="fgt1" & inlist(metric,"bias","cv"), by(km) c(mean p) format(%9.2f)
table method metric kc if !inlist(method,"llo","red") & indicator=="fgt2" & inlist(metric,"bias","cv"), by(km) c(mean p) format(%9.2f)
*module - core tradeoff
table km metric kc if method=="mi_2cel" & inlist(indicator,"fgt0","fgt1","fgt2") & inlist(metric,"bias"), by(indicator) c(mean p) format(%9.3f)
table km metric kc if method=="mi_2cel" & inlist(indicator,"fgt0","fgt1","fgt2") & inlist(metric,"cv"), by(indicator) c(mean p) format(%9.3f)

collapse (mean) p rpq_red rpq_rcs, by(method indicator metric kc km)
local sg = ""
local lm = "bias cv"
forvalues i=0/2 {
	foreach m of local lm {
		local g = "g`i'_`m'"
		cap graph drop `g'
		twoway (scatter p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel") (qfit p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel") (scatter p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel") (qfit p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel"), title("FGT`i'", size(normal)) ytitle("`m'") xtitle("Proportion of Asked Questions") ylabel(,angle(0)) legend(order(1 "RCS" 2 "RCS (fitted)" 3 "Reduced" 4 "Reduced (fitted)") size(vsmall)) graphregion(fcolor(white)) bgcolor(white) name(`g')
		local sg = "`sg' `g'"
	}
}
grc1leg `sg', graphregion(fcolor(white)) col(2)
graph drop `sg'
graph export "${gsdOutput}/RCS-Red.png", replace

*analysis with training set
*TODO

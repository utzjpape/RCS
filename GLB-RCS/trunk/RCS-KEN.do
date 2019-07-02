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
	local lmethod = "med avg reg tobit mi_reg mi_2cel"
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
		.r.estimate , lmethod("mi_2cel") nmi(`nmi')
	}
	.r.collate
	.r.analyze
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
forvalues t = 0/1 {
	*determine whether we use a 2005 for training and run imputations on 2015
	foreach kc of local lc {
		foreach km of local lm {
			callRCS using "`using`t''",t(`t') kc(`kc') km(`km')
		}
	}
}


*collect results
*forvalues t = 0/1 {
local t = 1
	clear
	foreach kc of local lc {
		foreach km of local lm {
			cap: append using "${gsdOutput}/KEN-KIHBS-c`kc'-m`km'-t`t'.dta"
			if _rc==601 di "File ${gsdOutput}/KEN-KIHBS-c`kc'-m`km'-t`t'.dta does not exist."
		}
	}
	save "${gsdOutput}/KEN-KIHBS-t`t'.dta", replace
*}


************************************************
* COMPARISON OF METHODS
************************************************
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

************************************************
* COMPARISON WITH REDUCED
************************************************
collapse (mean) p (max) max_p=p, by(method indicator metric rpq_red rpq_rcs kc km)
local sg = ""
local lm = "bias cv"
forvalues i=0/2 {
	*plot for bias
	local m = "bias"
	local g = "g`i'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		(scatter max_p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="red",  msize(vsmall) color(eltgreen)) ///
		(qfit max_p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="red", color(eltgreen)) ///
		(scatter p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="red",  msize(vsmall) color(emerald)) ///
		(qfit p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="red", color(emerald)) ///
		, title("FGT`i'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(1 "Rapid (avg)" 2 "Rapid (avg; fitted)" 3 "Rapid (max)" 4 "Rapid (max; fitted)" 5 "Reduced (avg)" 6 "Reduced (avg; fitted)" 7 "Reduced (max)" 8 "Reduced (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
	*plot for cv
	local m = "cv"
	local g = "g`i'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		, title("FGT`i'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(1 "Rapid (avg)" 2 "Rapid (avg; fitted)" 3 "Rapid (max)" 4 "Rapid (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
}
grc1leg `sg', imargin(b=0 t=0) graphregion(fcolor(white)) col(2) name(gfgt`i', replace) 
graph export "${gsdOutput}/RCS-red_fgt`i'.png", replace
graph drop `sg'
local sg = ""
*some stats for the text
summ p max_p if method=="mi_2cel" & indicator=="fgt0" & metric=="bias", d
list if method=="mi_2cel" & indicator=="fgt0" & metric=="bias" & rpq_rcs==.5
list if method=="red" & indicator=="fgt0" & metric=="bias" & inrange(rpq_red,.49,.51)
list if method=="mi_2cel" & indicator=="fgt0" & metric=="bias" & inrange(rpq_rcs,.25,.25)


************************************************
* COMPARISON WITH LLO
************************************************
*analysis with LLO
use "${gsdOutput}/KEN-KIHBS-t1.dta", clear
replace p = abs(p) if inlist(metric,"bias")
collapse (mean) p (max) max_p=p, by(method indicator metric kc km rpq_red rpq_rcs)
forvalues i=0/2 {
	*plot for bias
	local m = "bias"
	local g = "g`i'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		(scatter max_p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="llo", msize(vsmall) color(eltgreen)) ///
		(qfit max_p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="llo", color(eltgreen)) ///
		(scatter p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="llo", msize(vsmall) color(emerald)) ///
		(qfit p rpq_red if indicator=="fgt`i'" & metric=="`m'" & method=="llo", color(emerald)) ///
		, title("FGT`i'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(1 "Rapid (avg)" 2 "Rapid (avg; fitted)" 3 "Rapid (max)" 4 "Rapid (max; fitted)" 5 "Adj. reduced (avg)" 6 "Adj. reduced (avg; fitted)" 7 "Adj. reduced (max)" 8 "Adj. reduced (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
	*plot for cv
	local m = "cv"
	local g = "g`i'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="fgt`i'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		, title("FGT`i'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(1 "Rapid (avg)" 2 "Rapid (avg; fitted)" 3 "Rapid (max)" 4 "Rapid (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
}
grc1leg `sg', imargin(b=0 t=0) graphregion(fcolor(white)) col(2) name(gfgt`i', replace)
graph export "${gsdOutput}/RCS-LLO_fgt`i'.png", replace
graph drop `sg'
local sg = ""
*some stats for the text
list if method=="mi_2cel" & indicator=="fgt0" & metric=="bias" & p<.009

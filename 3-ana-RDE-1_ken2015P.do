*runs the analysis on the results files from the simulation

*check whether files have been creates
local res0= "${gsdOutput}/KEN-KIHBS-t0.dta"
local res1= "${gsdOutput}/KEN-KIHBS-t1.dta"
capture confirm file "`res0'"
if _rc != 0 {
	di as error "Please run RCS-KEN-0-simulate.do first to create the result files."
	error 1
}
capture confirm file "`res1'"
if _rc != 0 {
	di as error "Please run RCS-KEN-0-simulate.do first to create the result files."
	error 1
}

************************************************
* COMPARISON OF METHODS
************************************************
*analysis without training set
use "${gsdOutput}/KEN-KIHBS-t0.dta", clear
replace p = abs(p) if metric == "bias"
*estimation technique comparison
table method metric kc if !inlist(method,"llo","red") & inlist(metric,"bias","cv") & (km==2), by(indicator) c(mean p) format(%9.2f)
table method metric km if !inlist(method,"llo","red") & inlist(metric,"bias","cv") & (kc==0), by(indicator) c(mean p) format(%9.2f)
*module - core tradeoff
table km metric kc if method=="mi_2cel" & inlist(indicator,"fgt0","fgt1","fgt2","gini") & inlist(metric,"bias"), by(indicator) c(mean p) format(%9.3f)
table km metric kc if method=="mi_2cel" & inlist(indicator,"fgt0","fgt1","fgt2","gini") & inlist(metric,"cv"), by(indicator) c(mean p) format(%9.3f)

************************************************
* COMPARISON WITH REDUCED
************************************************
collapse (mean) p (max) max_p=p, by(method indicator metric rpq_red rpq_rcs kc km)
local sg = ""
local lm = "bias cv"
local lind = "fgt0 fgt1 fgt2 gini"
foreach sind of local lind {
	*plot for bias
	local m = "bias"
	local g = "g`sind'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		(scatter max_p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="red",  msize(vsmall) color(eltgreen)) ///
		(qfit max_p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="red", color(eltgreen)) ///
		(scatter p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="red",  msize(vsmall) color(emerald)) ///
		(qfit p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="red", color(emerald)) ///
		, title("`sind'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(3 "Rapid (avg)" 4 "Rapid (avg; fitted)" 1 "Rapid (max)" 2 "Rapid (max; fitted)" 7 "Reduced (avg)" 8 "Reduced (avg; fitted)" 5 "Reduced (max)" 6 "Reduced (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
	*plot for cv
	local m = "cv"
	local g = "g`sind'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		, title("`sind'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(3 "Rapid (avg)" 4 "Rapid (avg; fitted)" 1 "Rapid (max)" 2 "Rapid (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
}
grc1leg `sg', imargin(b=0 t=0) graphregion(fcolor(white)) col(2) name(gred, replace) 
graph export "${gsdOutput}/RCS-red.png", replace
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
local lind = "fgt0 fgt1 fgt2 gini"
foreach sind of local lind {
	*plot for bias
	local m = "bias"
	local g = "g`sind'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		(scatter max_p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="llo", msize(vsmall) color(eltgreen)) ///
		(qfit max_p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="llo", color(eltgreen)) ///
		(scatter p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="llo", msize(vsmall) color(emerald)) ///
		(qfit p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="llo", color(emerald)) ///
		, title("`sind'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(3 "Rapid (avg)" 4 "Rapid (avg; fitted)" 1 "Rapid (max)" 2 "Rapid (max; fitted)" 7 "Adj. reduced (avg)" 8 "Adj. reduced (avg; fitted)" 5 "Adj. reduced (max)" 6 "Adj. reduced (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
	*plot for cv
	local m = "cv"
	local g = "g`sind'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel",  msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		, title("`sind'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(3 "Rapid (avg)" 4 "Rapid (avg; fitted)" 1 "Rapid (max)" 2 "Rapid (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
}
grc1leg `sg', imargin(b=0 t=0) graphregion(fcolor(white)) col(2) name(gllo, replace)
graph export "${gsdOutput}/RCS-LLO.png", replace
graph drop `sg'
local sg = ""
*some stats for the text
list if method=="mi_2cel" & indicator=="fgt0" & metric=="bias" & p<.009

************************************************
* COMPARISON WITH SWIFT2
************************************************
*analysis with SWIFT2
use "${gsdOutput}/KEN-KIHBS-t1.dta", clear
replace p = abs(p) if inlist(metric,"bias")
collapse (mean) p (max) max_p=p, by(method indicator metric kc km rpq_red rpq_rcs)
local lind = "fgt0 fgt1 fgt2 gini"
foreach sind of local lind {
	*plot for bias
	local m = "bias"
	local g = "g`sind'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="swift2", msize(vsmall) color(eltgreen)) ///
		(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="swift2", color(eltgreen)) ///
		(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="swift2", msize(vsmall) color(emerald)) ///
		(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="swift2", color(emerald)) ///
		, title("`sind'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(3 "Rapid (avg)" 4 "Rapid (avg; fitted)" 1 "Rapid (max)" 2 "Rapid (max; fitted)" 7 "Swift 2.0 (avg)" 8 "Swift 2.0 (avg; fitted)" 5 "Swift 2.0 (max)" 6 "Swift 2.0 (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
	*plot for cv
	local m = "cv"
	local g = "g`sind'_`m'"
	twoway ///
		(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", msize(vsmall) color(erose)) ///
		(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(erose)) ///
		(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", msize(vsmall) color(maroon)) ///
		(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel", color(maroon)) ///
		(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="swift2", msize(vsmall) color(eltgreen)) ///
		(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="swift2", color(eltgreen)) ///
		(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="swift2", msize(vsmall) color(emerald)) ///
		(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="swift2", color(emerald)) ///
		, title("`sind'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(3 "Rapid (avg)" 4 "Rapid (avg; fitted)" 1 "Rapid (max)" 2 "Rapid (max; fitted)" 7 "Swift 2.0 (avg)" 8 "Swift 2.0 (avg; fitted)" 5 "Swift 2.0 (max)" 6 "Swift 2.0 (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
	local sg = "`sg' `g'"
}
grc1leg `sg', imargin(b=0 t=0) graphregion(fcolor(white)) col(2) name(gllo, replace)
graph export "${gsdOutput}/RCS-SW2.png", replace
graph drop `sg'
local sg = ""

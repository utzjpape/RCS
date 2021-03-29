*SIMULATE RCS FOR NIGER (USAID)

ma drop all
set more off
set maxiter 100

local using= "${gsdData}/NIG-USAID2017-HHData.dta"
capture confirm file "`using'"
if _rc != 0 {
	local sData = "${gsdDataBox}/NIG-USAID2017"
	*prepare food and non-food
	use "`sData'/fcons.dta", clear
	ren (fcons item) (xfood itemid)
	fItems2RCS, hhid(hhid) itemid(itemid) value(xfood) red(0)
	save "${gsdTemp}/NIG-HH-FoodItems.dta", replace
	
	use "`sData'/nfcons.dta", clear
	ren (nfcons item) (xnonfood itemid)
	fItems2RCS, hhid(hhid) itemid(itemid) value(xnonfood) red(0)
	save "${gsdTemp}/NIG-HH-NonFoodItems.dta", replace

	*get household member information: education and labor
	use "`sData'/hh.dta", clear
	*add food and non-food
	merge 1:1 hhid using "${gsdTemp}/NIG-HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
	merge 1:1 hhid using "${gsdTemp}/NIG-HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
	*delete missing consumption
	egen ctf = rowtotal(xfood*)
	egen ctnf = rowtotal(xnonfood*)
	drop if missing(ctf) | missing(ctnf) | (ctf==0)
	drop ctf ctnf ccons
	ren (head_sex edu depend_ratio child_all adult_all num_depend) (mcat_hhhsex mcat_edu mcon_dr mcon_nchildren mcon_nadults mcon_ndepend)
	order hhid strata cluster weight hhsize xdurables mcat* mcon*
	save "`using'", replace
}

*start RCS code
cap: prog drop callRCS
program define callRCS
	syntax using/, t(integer) kc(integer) km(integer)
	*number of simulations (should be 20)
	local nsim = 5
	*number of imputations (should be 50)
	local nmi = 50
	*methods
	local lmethod = "mi_2cel"
	local dirbase = "${gsdOutput}/NIG-USAID2017-c`kc'-m`km'-t`t'"
	*create instance to run RCS simulations
	capture classutil drop .r
	.r = .RCS.new
	.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`km') ncoref(`kc') ncorenf(`kc') nsim(`nsim') train(`t')
	.r.mask
	.r.estimate , lmethod("`lmethod'") nmi(`nmi')
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

local lc = "0 2 4 6 8 10"
local lm = "2 4 6 8 10"
foreach kc of local lc {
	foreach km of local lm {
		callRCS using "`using'",t(0) kc(`kc') km(`km')
	}
}

*collect results
clear
foreach kc of local lc {
	foreach km of local lm {
		cap: append using "${gsdOutput}/PAK-HIES-c`kc'-m`km'-t0.dta"
		if _rc==601 di "File ${gsdOutput}/PAK-HIES-c`kc'-m`km'-t0.dta does not exist."
	}
}
save "${gsdOutput}/PAK-HIES-t0.dta", replace


************************************************
* COMPARISON WITH REDUCED
************************************************
use "${gsdOutput}/PAK-HIES-t0.dta", clear
replace p = abs(p)
collapse (mean) p (max) max_p=p, by(method indicator metric rpq_rcs kc km)
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
		, title("FGT`i'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(1 "Rapid (avg)" 2 "Rapid (avg; fitted)" 3 "Rapid (max)" 4 "Rapid (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
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
graph export "${gsdOutput}/RCS-PAK_fgt`i'.png", replace
graph drop `sg'
local sg = ""

if "${gsdDo}"=="" {
	di as error "0-0-run init.do first
	error 1
}

version 14.2

*check whether files have been creates
local res= "${gsdOutput}/RDE-results_all.dta"
capture confirm file "`res'"
if _rc != 0 {
	di as error "Please run 2-sim-RDE.do first to create the result files."
	error 1
}

************************************************
* COMPARISON WITH REDUCED
************************************************
use "`res'", clear
*mask RCS for more than 10 modules, as this would become unfeasible, but we ran the simulations
*for getting low number of effective questions asked for reduced consumption
replace p = . if method == "mi_2cel" & km>10
replace p = abs(p) if metric == "bias"
collapse (mean) p (max) max_p=p, by(method indicator metric rpq_red rpq_rcs kc km sd)
local sd = "KEN-KIHBS NGA-GHS SDN-NBHS2009 SLD-SLHS2013 SSD-NBHS2009"
foreach d of local sd {
	if inlist("`d'","KEN-KIHBS","NGA-GHS") {
		local red = "llo"
		local mark = "Adj. "
	}
	else {
		local red = "red"
		local mark = ""
	}
	local sg = ""
	local lm = "bias cv"
	local lind = "fgt0 fgt1 fgt2 gini"
	foreach sind of local lind {
		*plot for bias
		local m = "bias"
		local g = "g`sind'_`m'"
		twoway ///
			(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel" & sd=="`d'",  msize(vsmall) color(erose)) ///
			(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel" & sd=="`d'", color(erose)) ///
			(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel" & sd=="`d'",  msize(vsmall) color(maroon)) ///
			(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel" & sd=="`d'", color(maroon)) ///
			(scatter max_p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="`red'" & sd=="`d'",  msize(vsmall) color(eltgreen)) ///
			(qfit max_p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="`red'" & sd=="`d'", color(eltgreen)) ///
			(scatter p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="`red'" & sd=="`d'",  msize(vsmall) color(emerald)) ///
			(qfit p rpq_red if indicator=="`sind'" & metric=="`m'" & method=="`red'" & sd=="`d'", color(emerald)) ///
			, title("`sind'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(3 "Rapid (avg)" 4 "Rapid (avg; fitted)" 1 "Rapid (max)" 2 "Rapid (max; fitted)" 7 "`mark'Reduced (avg)" 8 "`mark'Reduced (avg; fitted)" 5 "`mark'Reduced (max)" 6 "`mark'Reduced (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
		local sg = "`sg' `g'"
		*plot for cv
		local m = "cv"
		local g = "g`sind'_`m'"
		twoway ///
			(scatter max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel" & sd=="`d'",  msize(vsmall) color(erose)) ///
			(qfit max_p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel" & sd=="`d'", color(erose)) ///
			(scatter p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel" & sd=="`d'",  msize(vsmall) color(maroon)) ///
			(qfit p rpq_rcs if indicator=="`sind'" & metric=="`m'" & method=="mi_2cel" & sd=="`d'", color(maroon)) ///
			, title("`sind'", size(small)) ytitle("`m'", size(small)) xtitle("Proportion of effective questions", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(order(3 "Rapid (avg)" 4 "Rapid (avg; fitted)" 1 "Rapid (max)" 2 "Rapid (max; fitted)") size(vsmall) cols(4)) graphregion(fcolor(white)) bgcolor(white) name(`g', replace)
		local sg = "`sg' `g'"
	}
	grc1leg `sg', imargin(b=0 t=0) graphregion(fcolor(white)) col(2) name(gred, replace) 
	graph export "${gsdOutput}/RCS-red_`d'.png", replace
	graph drop `sg'
	local sg = ""
	*some stats for the text
	summ p max_p if method=="mi_2cel" & indicator=="fgt0" & metric=="bias", d
	list if method=="mi_2cel" & indicator=="fgt0" & metric=="bias" & rpq_rcs==.5
	list if method=="red" & indicator=="fgt0" & metric=="bias" & inrange(rpq_red,.49,.51)
	list if method=="mi_2cel" & indicator=="fgt0" & metric=="bias" & inrange(rpq_rcs,.25,.25)
}


ma drop all
set more off
set maxiter 100

capture confirm file "${gsdData}/KEN-KIHBS2015C-HHData.dta"
if _rc != 0 {
	quiet: do "${gsdDo}/1-prep-KEN-KIHBS.do"
}

local mi = 50

************************************************
* CONSUMPTION SHARES
************************************************
local lfood = "food nonfood"
local ly = "2005 2015"
foreach sf of local lfood {
	foreach y of local ly {
		use "${gsdData}/KEN-KIHBS-HHData.dta", clear
		keep if train == (`y'==2005)
		local hhid = "hhid strata urban cluster"
		keep `hhid' weight x`sf'*
		reshape long x`sf', i(`hhid' weight) j(itemid)
		ren x`sf' x
		replace x = 0 if mi(x)
		bysort `hhid': egen xt = total(x)
		gen px = x/xt
		replace px = 0 if mi(px)
		collapse (mean) px [pweight=weight], by(itemid)
		gsort -px
		egen r = seq()
		ren px px`y'
		cap label drop l`sf'
		run "${gsdDataBox}/KEN-KIHBS/KEN-KIHBS_`sf'-label.do"
		label val itemid l`sf'
		save "${gsdTemp}/`sf'-shares_`y'.dta", replace
	}
	use "${gsdTemp}/`sf'-shares_2005.dta", clear
	ren itemid itemid2005
	merge 1:1 r using "${gsdTemp}/`sf'-shares_2015.dta", nogen assert(match)
	order r, first
	export excel using "${gsdOutput}/KIHBS-shares.xls", sheetreplace sheet("`sf'") firstrow(var)
	export excel using "${gsdOutput}/KIHBS-shares.xls", sheetreplace sheet("`sf'_id") nolabel firstrow(var)
}

******************************************
* BALANCE TESTS FOR 2015/16 PAPI AND CAPI
******************************************
*balance tests for household characteristics
use "${gsdData}/KEN-KIHBS-HHData.dta", clear
version 16
keep if train==0
append using "${gsdData}/KEN-KIHBS2015C-HHData.dta", gen(survey)
svyset cluster [pweight = weight], strata(strata) singleunit(centered)
unab xcon : mcon*
fvunab xcat : i.mcat*
xi `xcat'
local vars = "`xcon' `_dta[__xi__Vars__To__Drop__]'"
capture: file close fh
file open fh using "${gsdOutput}/KIHBS-baltests.csv", write replace
file write fh "variable" _tab "KIHBS" _tab "Pilot" _tab "Difference" _n
foreach v of local vars {
	svy: mean `v', over(survey)
	matrix M = e(b)
	matrix SE = e(V)
	scalar me0 = M[1,1]
	scalar me1 = M[1,2]
	scalar diff = me1 - me0
	scalar se0 = SE[1,1]^(1/2)
	scalar se1 = SE[2,2]^(1/2)
	*next line doesn't seem to work anymore in Stata 18
*	test [`v']0 = [`v']1
	local star =""
	if r(p) <= .1 local star = "`star'*"
	if r(p) <= .05 local star = "`star'*"
	if r(p) <= .01 local star = "`star'*"
	if r(p) < 0.001 local sp = "<0.001"
	else local sp : di %5.3f (r(p))
	file write fh "`v'" _tab %5.3f (me0) _tab %5.3f (me1) _tab %5.3f (diff) "`star'" _n _tab "(" %5.3f (se0) ")" _tab "(" %5.3f (se1) ")" _tab "(`sp')" _n
}
file close fh

xi: balancetable survey mcon* `xcat' using "${gsdOutput}/KIHBS-balance.xlsx" [aweight=weight], pval vce(cluster cluster) varla replace
*balance of modules for CAPI
use "${gsdData}/KEN-KIHBS2015C-HHData.dta", clear
tab hhmod

************************************************************************
* CALCULATE INDICATORS FOR KIHBS 2015/16, CIHBS 2015/16 (RCS) AND SWIFT
************************************************************************
*CIHBS pilot
eststo clear
capture confirm file "${gsdOutput}/KEN-KIHBS_cmp.dta"
if _rc != 0 {
	use "${gsdData}/KEN-KIHBS-HHData.dta", replace
	keep if train==0
	save "${gsdData}/KEN-KIHBS2015P-HHData.dta", replace
	
	local hhid = "hhid strata urban cluster"
	*get poverty lines from 2015 PAPI dataset
	use "${gsdData}/KEN-KIHBS2015P-HHData.dta", clear
	egen xcons = rowtotal(xfood* xnonfood*)
	_pctile xcons [pweight=weight*hhsize], nq(100)
	quiet forvalues i = 1/100 {
		local pline`i' = r(r`i')
		*for reference
		gen ref_fgt0_i`i' = xcons < `pline`i''
		gen ref_fgt1_i`i' = max(`pline`i'' - xcons,0) / `pline`i''
		gen ref_fgt2_i`i' = ref_fgt1_i`i'^2
	}
	quiet: fastgini xcons [pweight=weight*hhsize]
	gen ref_gini = r(gini)
	gen xid = 1
	collapse (mean) ref_* [pweight=weight*hhsize], by(xid)
	reshape long ref_fgt0_i ref_fgt1_i ref_fgt2_i, i(xid) j(p)
	ren *_i *
	drop xid
	order p ref_fgt0 ref_fgt1 ref_fgt2 ref_gini
	tempfile fref
	save "`fref'", replace

	use "${gsdData}/KEN-KIHBS2015C-HHData.dta", clear
	drop xfood* xnonfood* xfcons xnfcons
	unab mcon : mcon_*
	fvunab mcat : i.mcat_*
	*read saved model (as finding the model can take a while)
	local sfmodel = "${gsdData}/KEN-KIHBS2015C-model.txt"
	capture: confirm file "`sfmodel'"
	if _rc==0 {
			capture file close fhm
			file open fhm using "`sfmodel'", read
			file read fhm model 
			file read fhm logmodel
			file close fhm
			quiet: xi `mcat'
	}
	else {
		local model = ""
		local logmodel = ""
	}
	*create class for model selection and estimation
	capture classutil drop .re
	.re = .RCS_estimator.new
	.re.prepare , hhid("`hhid'") weight("weight") hhmod("hhmod") cluster("cluster") xfcons("xfcons") xnfcons("xnfcons") nmi(50)
	.re.select_model hhsize urban `mcon' `mcat', model("`model'") logmodel("`logmodel'") method("forward aicc")
	*save model for next use
	capture file close fhm
	file open fhm using "`sfmodel'", replace write
	file write fhm "`.re.model'" _n "`.re.logmodel'"
	file close fhm
	*run estimation
	.re.est_mi_2cel
	gen xcons = .
	mi register passive xcons
	quiet: mi passive: replace xcons = 0
	foreach v of varlist xfcons? xnfcons? {
		quiet: mi passive: replace xcons = xcons + `v'
	}
	*cleaning
	mi register imputed xcons
	mi update
	merge 1:1 `hhid' using "${gsdData}/KEN-KIHBS2015C-HHData.dta", assert(match) nogen keepusing(strata urban cluster weight hhsize hhmod)
	ren _*_xcons xcons*
	mi unset
	keep `hhid' strata urban cluster weight hhsize xcons*
	drop xcons_* xcons
	reshape long xcons, i(`hhid' strata urban cluster weight hhsize) j(mi)
	quiet forvalues i = 1/100 {
		gen rcs_fgt0_i`i' = xcons < `pline`i''
		gen rcs_fgt1_i`i' = max(`pline`i'' - xcons,0) / `pline`i''
		gen rcs_fgt2_i`i' = rcs_fgt1_i`i'^2
	}
	quiet: fastgini xcons [pweight=weight*hhsize]
	gen rcs_gini = r(gini)
	gen xid = 1
	collapse (mean) rcs_* [pweight=weight*hhsize], by(xid)
	reshape long rcs_fgt0_i rcs_fgt1_i rcs_fgt2_i, i(xid) j(p)
	ren *_i *
	drop xid
	order p rcs_fgt0 rcs_fgt1 rcs_fgt2 rcs_gini
	tempfile frcs
	save "`frcs'", replace
	
	*get simplified aggregate without imputations
	use "${gsdData}/KEN-KIHBS2015C-HHData.dta", clear
	egen xcons = rowtotal(xfcons? xnfcons?)
	keep `hhid' xcons weight hhsize
	quiet forvalues i = 1/100 {
		*for reference
		gen red_fgt0_i`i' = xcons < `pline`i''
		gen red_fgt1_i`i' = max(`pline`i'' - xcons,0) / `pline`i''
		gen red_fgt2_i`i' = red_fgt1_i`i'^2
	}
	quiet: fastgini xcons [pweight=weight*hhsize]
	gen red_gini = r(gini)
	gen xid = 1
	collapse (mean) red_* [pweight=weight*hhsize], by(xid)
	reshape long red_fgt0_i red_fgt1_i red_fgt2_i, i(xid) j(p)
	ren *_i *
	drop xid
	order p red_fgt0 red_fgt1 red_fgt2 red_gini
	tempfile fred
	save "`fred'", replace
	
	*add swift consumption distribution with and without MI
	use "${gsdData}/KEN-KIHBS-HHData.dta", clear
	keep if train==1
	drop train
	egen xcons = rowtotal(xfood* xnonfood*)
	append using "${gsdData}/KEN-KIHBS2015P-HHData.dta", gen(test)
	drop xfood* xnonfood*
	unab mcon : mcon_*
	fvunab mcat : i.mcat_*
	gen logmodel = ln(xcons)
	replace logmodel = ln(.01) if xcons==0
	local sfmodel = "${gsdData}/KEN-KIHBS2005P-swiftmodel.txt"
	capture: confirm file "`sfmodel'"
	if _rc==0 {
			capture file close fhm
			file open fhm using "`sfmodel'", read
			file read fhm logmodel 
			file close fhm
			quiet: xi `mcat'
	}
	else {
		quiet: xi: vselect logmodel hhsize urban `mcon' `mcat' if !test [pweight=weight], forward aicc
		local logmodel = "`r(predlist)'"
		capture file close fhm
		file open fhm using "`sfmodel'", replace write
		file write fhm "`logmodel'"
		file close fhm
	}
	*output regression
	eststo: quietly reg logmodel `logmodel' if !test [pweight=weight]
	esttab , r2 ar2 aic
	esttab using "${gsdOutput}/KEN-KIHBS_cmp-models.csv", r2 ar2 aic replace
	eststo clear
	*predict icons if test, xb
	*apply to 2015P
	mi set wide
	mi register imputed logmodel
	mi register regular `logmodel'
	*run ols or truncated regression
	mi impute regress logmodel = `logmodel', add(50)
	ren _*_logmodel icons*
	mi unset
	keep if test
	keep `hhid' strata urban cluster weight hhsize icons*
	reshape long icons, i(`hhid' strata urban cluster weight hhsize) j(mi)
	replace icons = exp(icons)
	quiet forvalues i = 1/100 {
		gen swi_fgt0_i`i' = icons < `pline`i''
		gen swi_fgt1_i`i' = max(`pline`i'' - icons,0) / `pline`i''
		gen swi_fgt2_i`i' = swi_fgt1_i`i'^2
	}
	quiet: fastgini icons [pweight=weight*hhsize]
	gen swi_gini = r(gini)
	gen xid = 1
	collapse (mean) swi_* [pweight=weight*hhsize], by(xid)
	reshape long swi_fgt0_i swi_fgt1_i swi_fgt2_i, i(xid) j(p)
	ren *_i *
	drop xid
	order p swi_fgt0 swi_fgt1 swi_fgt2 swi_gini
	tempfile fswi
	save "`fswi'", replace

	*ANALYSIS
	use "`fref'", clear
	merge 1:1 p using "`frcs'", nogen
	merge 1:1 p using "`fred'", nogen
	merge 1:1 p using "`fswi'", nogen
	save "${gsdOutput}/KEN-KIHBS_cmp.dta", replace
} 
else use "${gsdOutput}/KEN-KIHBS_cmp.dta", clear

*calculate absolute differences
local lind = "fgt0 fgt1 fgt2 gini"
foreach sind of local lind {
	label var ref_`sind' "`sind' Reference"
	label var red_`sind' "`sind' Observed"
	label var rcs_`sind' "`sind' Rapid"
	label var swi_`sind' "`sind' X-Survey"
	gen brcs`sind' = ref_`sind'-rcs_`sind'
	gen drcs`sind' = abs(brcs`sind')
	gen bswi`sind' = ref_`sind'-swi_`sind'
	gen dswi`sind' = abs(bswi`sind')
	gen bred`sind' = ref_`sind'-red_`sind'
	gen dred`sind' = abs(bred`sind')
	label var brcs`sind' "Rapid"
	label var drcs`sind' "Rapid"
	label var bswi`sind' "X-Survey"
	label var dswi`sind' "X-Survey"
	label var bred`sind' "Observed"
	label var dred`sind' "Observed"
}
mean d*
twoway (line brcsfgt0 p, color(maroon)) (line bswifgt0 p, color(brown)) , title("fgt0", size(small)) ytitle("bias", size(small)) xtitle("Poverty Percentile", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(size(vsmall) cols(2)) graphregion(fcolor(white)) bgcolor(white) name(gfgt0, replace)
twoway (line brcsfgt1 p, color(maroon)) (line bswifgt1 p, color(brown)) , title("fgt1", size(small)) ytitle("bias", size(small)) xtitle("Poverty Percentile", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(size(vsmall) cols(2)) graphregion(fcolor(white)) bgcolor(white) name(gfgt1, replace)
twoway (line brcsfgt2 p, color(maroon)) (line bswifgt2 p, color(brown)) , title("fgt2", size(small)) ytitle("bias", size(small)) xtitle("Poverty Percentile", size(small)) ylabel(,angle(0) labsize(small)) xlabel(,labsize(small)) legend(size(vsmall) cols(2)) graphregion(fcolor(white)) bgcolor(white) name(gfgt2, replace)
grc1leg gfgt0 gfgt1 gfgt2, imargin(b=0 t=0) graphregion(fcolor(white)) col(1) name(gind, replace)
graph export "${gsdOutput}/RCS-XS.png", replace
graph drop gfgt0 gfgt1 gfgt2

*SIMULATE RCS FOR KENYA

ma drop all
set more off
set maxiter 100

local using= "${gsdData}/PAK-HIES2015-HHData.dta"
capture confirm file "`using'"
if _rc != 0 {
	local sData = "${gsdDataBox}/PAK-HIES2015"
	*prepare food and non-food
	use "`sData'/fcons.dta", clear
	ren fcons xfood
	fItems2RCS, hhid(hhid) itemid(itemid) value(xfood) red(0)
	save "${gsdTemp}/PAK-HH-FoodItems.dta", replace
	use "`sData'/nfcons.dta", clear
	ren nfcons xnonfood
	fItems2RCS, hhid(hhid) itemid(itemid) value(xnonfood) red(0)
	save "${gsdTemp}/PAK-HH-NonFoodItems.dta", replace
	*get household member information: education and labor
	use "`sData'/hh.dta", clear
	*add food and non-food
	merge 1:1 hhid using "${gsdTemp}/PAK-HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
	merge 1:1 hhid using "${gsdTemp}/PAK-HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
	*delete missing consumption
	egen ctf = rowtotal(xfood*)
	egen ctnf = rowtotal(xnonfood*)
	drop if missing(ctf) | missing(ctnf) | (ctf==0)
	drop ctf ctnf ccons
	compress
	gen xdurables = 0
	order hhid strata urban cluster weight hhsize xdurables mcat* mcon*
	destring strata, replace
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
	local dirbase = "${gsdOutput}/PAK-HIES-c`kc'-m`km'-t`t'"
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

local lc = "0 5 10"
local lm = "2 4 6 8 10"
foreach kc of local lc {
	foreach km of local lm {
		callRCS using "`using'",t(0) kc(`kc') km(`km')
	}
}

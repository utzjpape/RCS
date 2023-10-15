*SIMULATE RCS FOR PAKISTAN

ma drop all
set more off
set seed 23081980

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
	drop strata
	ren mcat_province strata
	order hhid strata urban cluster weight hhsize xdurables mcat* mcon*
	destring strata, replace
	destring hhid, replace
	keep hhid strata urban cluster weight hhsize mcon_* mcat_* xdurables xfood* xnonfood*
	order hhid strata urban cluster weight hhsize mcon_* mcat_* xdurables xfood* xnonfood*, first
	*ensure no missing values
	desc, varl
	local lv = r(varlist)
	foreach v of local lv {
		assert !missing(`v')
	}
	compress
	save "`using'", replace
}


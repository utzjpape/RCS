*preparing GHS from Nigeria 2016

ma drop all
set more off
set seed 23081980

local using= "${gsdData}/NGA-GHS-HHData.dta"
capture confirm file "`using'"
if _rc != 0 {
	run "${gsdDo}/1-prep-NGA-GHS2016.do"
	run "${gsdDo}/1-prep-NGA-GHS2019.do"
	
	*combine both datasets
	use "${gsdTemp}/NGA-GHS2019-HHData.dta", clear
	append using "${gsdTemp}/NGA-GHS2016-HHData.dta", gen(train)
	save "`using'", replace

	egen x = rowtotal(xfood* xnonfood* xdurables)
	keep hhid strata urban cluster weight hhsize x train
	summ x if train, d
	summ x if !train, d	
}

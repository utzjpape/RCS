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
	*drop items that are not in both aggregates
	drop xfood148 xnonfood105 xnonfood326 xnonfood513 xfood21 xfood54 xfood55 xfood65 xfood85 xfood95 xfood131 xfood140 xnonfood518 xnonfood519
	save "`using'", replace

	egen x = rowtotal(xfood* xnonfood* xdurables)
	keep hhid strata urban cluster weight hhsize x train
	summ x if train, d
	summ x if !train, d	
}

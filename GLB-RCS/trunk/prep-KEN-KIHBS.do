*Prepare Kenya unified KIHBS data

ma drop all
set more off

*parameters
*data directory
local sData = "${gsdDataBox}/KEN-KIHBS"

*arrange dataset
forvalues i = 1/3 {
	if (`i'==1) local s = "2005P"
	else if (`i'==2) local s = "2015P"
	else if (`i'==3) local s = "2015C"
	*Food
	use "`sData'/fcons-unified.dta", clear
	keep if survey==`i'
	drop survey
	ren (itemid fcons) (foodid xfood)
	collapse (sum) xfood, by(clid hhid foodid)
	fItems2RCS, hhid(clid hhid) itemid(foodid) value(xfood) red(0)
	tempfile ffood
	save "`ffood'", replace
	*Non-Food
	use "`sData'/nfcons-unified.dta", clear
	keep if survey==`i'
	drop survey
	ren (itemid nfcons) (nonfoodid xnonfood)
	collapse (sum) xnonfood, by(clid hhid nonfoodid)
	fItems2RCS, hhid(clid hhid) itemid(nonfoodid) value(xnonfood) red(0)
	tempfile fnonfood
	save "`fnonfood'", replace
	*Household Dataset
	use "`sData'/hh-unified.dta", clear
	keep if survey==`i'
	drop survey
	merge 1:1 clid hhid using "`ffood'", nogen keep(match) keepusing(xfood*)
	merge 1:1 clid hhid using "`fnonfood'", nogen keep(match) keepusing(xnonfood*)
	keep clid urban uid county weight hhsize rooms ownhouse wall roof floor impwater impsan elec_acc depen_cat nchild pchild nadult padult nsenior psenior literacy malehead ageheadg hhedu hhh_empstat asset_index xfood* xnonfood*
	ren (uid clid county asset_index) (hhid cluster strata assets)
	ren (rooms ownhouse impwater impsan elec_acc nchild pchild nadult padult nsenior psenior literacy assets) mcon_=
	ren (wall roof floor depen_cat malehead ageheadg hhedu hhh_empstat) mcat_=
	xtile mcat_rooms = mcon_rooms [pweight=weight], n(4)
	xtile mcat_passets = mcon_assets [pweight=weight], n(4)
	gen xdurables = 0
	order hhid strata urban cluster weight hhsize mcon_* mcat_* xdurables xfood* xnonfood*, first
	compress
	save "${gsdData}/KEN-KIHBS`s'-HHData.dta", replace

	*produce reduced dataset
	gen r = runiform()
	bysort cluster: egen xr = mean(r)
	drop if xr < .5
	drop r xr
	compress
	save "${gsdData}/KEN-KIHBS`s'-HHDatared.dta", replace
}



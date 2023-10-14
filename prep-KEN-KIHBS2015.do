*Prepare Kenya unified KIHBS data

ma drop all
set more off
set seed 23081980

*parameters
*data directory
local sData = "${gsdDataBox}/KEN-KIHBS2015"

*adjust capi pilot for smaller core
use "`sData'/fcons-unified.dta", clear
label copy litemid lfood
label save lfood using "${gsdData}/KEN-KIHBS_food-label.do" , replace
merge m:1 survey clid hhid using "`sData'/hh-unified.dta", nogen assert(match using) keep(match) keepusing(hhmod)
*reduce number of core items to become more realistic
sort itemid
replace mod_item = -1 if mod_item==0 & !inlist(itemid,801,1101,103,105,301) & (survey==3)
egen xx = group(itemid) if mod_item==-1 & (survey==3)
replace mod_item = mod(xx-1,3)+1 if mod_item==-1 & (survey==3)
replace fcons = . if (mod_item != hhmod) & (mod_item!=0) & (survey==3)
drop xx hhmod
save "`sData'/fcons-unified_adj.dta", replace
keep if survey ==3
collapse (sum) fcons, by(clid hhid mod_item)
reshape wide fcons, i(clid hhid) j(mod_item)
tempfile ff
save "`ff'", replace
*non-food
use "`sData'/nfcons-unified.dta", clear
label copy litemid lnonfood
label save lnonfood using "${gsdData}/KEN-KIHBS_nonfood-label.do" , replace
merge m:1 survey clid hhid using "`sData'/hh-unified.dta", nogen assert(match using) keep(match) keepusing(hhmod)
sort itemid
replace mod_item = -1 if mod_item==0 & !inlist(itemid,3206,2001,5507,3509,3026) & (survey==3)
egen xx = group(itemid) if mod_item==-1 & (survey==3)
replace mod_item = mod(xx-1,3)+1 if mod_item==-1 & (survey==3)
replace nfcons = . if (mod_item != hhmod) & (mod_item!=0) & (survey==3)
drop xx hhmod
save "`sData'/nfcons-unified_adj.dta", replace
keep if survey ==3
collapse (sum) nfcons, by(clid hhid mod_item)
reshape wide nfcons, i(clid hhid) j(mod_item)
tempfile fn
save "`fn'", replace

*arrange dataset
forvalues i = 1/3 {
	if (`i'==1) local s = "2005P"
	else if (`i'==2) local s = "2015P"
	else if (`i'==3) local s = "2015C"
	*Food
	use "`sData'/fcons-unified_adj.dta", clear
	keep if survey==`i'
	drop survey
	ren (itemid fcons) (foodid xfood)
	collapse (sum) xfood, by(clid hhid foodid)
	fItems2RCS, hhid(clid hhid) itemid(foodid) value(xfood) red(0)
	tempfile ffood
	save "`ffood'", replace
	*Non-Food
	use "`sData'/nfcons-unified_adj.dta", clear
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
	if `i'==3 {
		merge 1:1 clid hhid using "`ff'", nogen keep(match) keepusing(fcons*)
		merge 1:1 clid hhid using "`fn'", nogen keep(match) keepusing(nfcons*)
		levelsof hhmod, local(lnm)
		foreach k of local lnm {
			replace fcons`k'=0 if mi(fcons`k') & hhmod==`k'
			replace nfcons`k'=0 if mi(nfcons`k') & hhmod==`k'
			replace fcons`k'=. if hhmod!=`k'
			replace nfcons`k'=. if hhmod!=`k'
		}
		ren fcons* xfcons*
		ren nfcons* xnfcons*
		replace xfcons0 = 0 if mi(xfcons0)
		replace xnfcons0 = 0 if mi(xnfcons0)
		egen xxx = rowtotal(xfood* xnonfood*)
		replace totcons = xxx
		drop xxx
	}
	egen x = rowtotal(xfood* xnonfood*)
	assert round(totcons-x,10^-4)==0
	drop x
	*remove outliers
	summ totcons, d
	drop if totcons > 5*r(sd)
	if `i'==3 local hhmod = "hhmod xfcons* xnfcons*"
	else local hhmod = ""
	keep clid urban uid county weight hh* rooms ownhouse wall roof floor impwater impsan elec_acc depen_cat nchild pchild nadult padult nsenior psenior literacy malehead ageheadg hhedu hhh_empstat asset_index xfood* xnonfood* `hhmod'
	ren (uid clid county asset_index) (hhid cluster strata assets)
	ren (rooms ownhouse impwater impsan elec_acc nchild pchild nadult padult nsenior psenior literacy assets) mcon_=
	ren (wall roof floor depen_cat malehead ageheadg hhedu hhh_empstat) mcat_=
	xtile mcat_rooms = mcon_rooms [pweight=weight], n(4)
	xtile mcat_passets = mcon_assets [pweight=weight], n(4)
	gen xdurables = 0
	order hhid strata urban cluster weight hhsize mcon_* mcat_* `hhmod' xdurables xfood* xnonfood*, first
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

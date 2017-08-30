*functions for RCS
* RCS_run: runs the whole suite of programs
*
* RCS_partition: sub-program to partition items into modules
*
* RCS_prepare: prepares the dataset; runs the partition
* RCS_assign: assigns households to their modules for each simulation
* RCS_simulate: simulates RCS
* RCS_collate: collates results from the simulation
* RCS_analyze: analyzes the results




	*SIMULATE PARTIAL SURVEYS FOR HERGAISA

	clear all
	ma drop all
	set more off
	set matsize 10000

	*parameters
	*number of modules
	local M = 4
	*number of simulations
	local N = 20
	*number of imputations 
	local nI = 20
	*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
	local ndiff = 3

	*methods
	*local lmethod = "med avg reg tobit MICE MImvn"
	*local lmethod = "reg tobit"
	*local lmethod = "med avg reg reg2 reg3 tobit tobit2 tobit3 MICE MImvn"
	*local lmethod = "twopart twopart2 twopart3"
	local lmethod = "med avg reg reg2 reg3 tobit tobit2 tobit3 twopart twopart2 twopart3 MICE MImvn"
	*local lmethod = "reg"
	
	*xfood=xfood*4 is implimented below! decide if we keep this or not. 


*data directory
local sData = "${gsdDataBox}/SOM-SLHS13"

*NOT used: deflator to divide nominal expenditures by and poverty line for urban Hargeiza
*NOT used: local xdeflator = 1.094426
local xdeflator = 1 // Does nothing

local xpovline = 184.1037
*local xfline = 112.686

*Make temp directories
local lc_sdBase = "${gsdOutput}/SOM/modct/"
capture: mkdir "${gsdOutput}/SOM/modct"
local lc_sdTemp = "`lc_sdBase'/Temp"
capture: mkdir "`lc_sdTemp'"
local lc_sdOut = "`lc_sdBase'/Out"
capture: mkdir "`lc_sdOut'"


*prepare items in wide format, adding zeros for missings and conserving labels
capture: program drop fItems2RCS
program define fItems2RCS
	syntax , hhid(varname) itemid(varname) value(varname)
	* save the value labels for variables in local list
	quiet: levelsof `itemid', local(`itemid'_levels)
	foreach val of local `itemid'_levels {
		local `itemid'_`val' : label `: value label `itemid'' `val'
	}
	*create zeros for missing values
	quiet: reshape wide `value', i(`hhid') j(`itemid')
	foreach v of varlist `value'* {
		quiet: replace `v'=0 if `v'>=.
	}
	*reinstantiate labels
	foreach val of local `itemid'_levels {
		label var `value'`val' "``itemid'_`val''"
	}
end



*CREATE MODULES 
*for validation of the method, missing data is assumed to be missing
*as the consumption aggregate implicitly assumes.
*food consumption
use "`sData'/food_consumption_clean.dta", clear
keep hhid foodid xfood
*replace xfood = xfood / `xdeflator' if xfood<.
replace xfood = xfood / `xdeflator' if xfood<.
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "`lc_sdTemp'/HH-FoodItems.dta", replace
*non food consumption
use "`sData'/non_food_clean.dta", clear
keep hhid nonfoodid xnonfood
*replace xnonfood = xnonfood / `xdeflator' if xnonfood<.
replace xnonfood = xnonfood / `xdeflator' if xnonfood<.
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "`lc_sdTemp'/HH-NonFoodItems.dta", replace

/*
*get confidence interval for poverty
use "`sData'/wfilez.dta", clear
drop if strata ~= 1
svyset cluster [pweight=weight]
gen poor = rpce < zupper
mean poor
*graph food share
gen x = rfood_pc + rnonfood_pc
gen ratio = rfood_pc / x
egen r = rank(x)
sort x
twoway (scatter ratio r) (qfit ratio r), title("Hergeiza")
graph export "`lc_sdOut'\Hergeiza_fshare.png", as(png) replace
*/

*get household characteristics
use "`sData'/data_i_proc_public.dta", clear
gen hhsex = S3_A04 if S3_A05==1
ren S3_A06_1 age
gen work = S3_AE07==1
keep hhid hhsex age work strata cluster weight
gen age_child = age<15 if age<.
gen age_adult = inrange(age,15,64) if age<.
gen age_senior = age>64 if age<.
collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior nwork=work (firstnm) hhsex, by(hhid strata cluster weight)
merge 1:1 hhid using "`sData'/data_h_proc_public.dta", nogen assert(match) keep(match) keepusing(S13_G01 S13_G03A S13_G04_* S13_G05 S13_G07 S13_G10 S13_G15 S13_G24 S13_G26)
ren (S13_G01 S13_G03A S13_G05 S13_G07 S13_G10 S13_G15 S13_G24 S13_G26) (hhhouse hhwater hhtoilet hhmaterial hhmode hhplot hhfood hhsleep)
ren S13_G04_* hhcook_*
drop hhcook_7 hhcook_99 hhcook_4
*simplify by setting missing values to conservative answers
recode hhhouse (99=7) (.=7)
recode hhwater (99=8) (.=8) (5=8)
recode hhtoilet (99=2) (.=2) (4=3)
recode hhsleep (99=2) (.=2)
recode hhmaterial (99=5) (.=5) (4=5)
recode hhmode (5=4) (99=4) (.=4) (3=4)
recode hhfood (99=2) (.=2)
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
gen pwork = nwork / hhsize
gen bwork = nwork>0
*add durables and food and non-food
merge 1:1 hhid using "`sData'/wfilez.dta", nogen keep(match) keepusing(xdurables_pc)
merge 1:1 hhid using "`lc_sdTemp'/HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "`lc_sdTemp'/HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
save "`lc_sdTemp'/HHData.dta", replace



*RCS_partition xvalue, hhid("hhid") itemid("foodid") fweight("weight") hhsize("hhsize") nmodules(4) ncore(33) ndiff(3)
*use "`lc_sdTemp'/HH-Food.dta", clear
*local xvalue = "xfood"
*local hhid = "hhid"
*local itemid = "foodid"
*local fweight= "weight"
*local hhsize = "hhsize"
*local nmodules = 4
*local ncore = 33
*local ndiff=3
capture: program drop RCS_partition
program define RCS_partition
	syntax varname, hhid(varname) itemid(varname) fweight(varname) hhsize(varname) nmodules(integer) ncore(integer) ndiff(integer) [EGALshare]
	*prepare dataset
	local xvalue = "`varlist'"
	local M = `nmodules'
	local ncore = `ncore'
	local pc = "_pc"
	*preserve dataset
	preserve
	*obtain share of household consumption
	gen wx = `xvalue' * `fweight'
	if ("`egalshare'"!="") {
		bysort `hhid': egen shhx = total(`xvalue')
		replace shhx = `xvalue' / shhx * `fweight'
		bysort `itemid': egen sx = total(shhx)
		bysort `itemid': egen cx = total(`fweight')
		replace sx = sx / cx
		drop cx shhx
	}
	else {
		bysort `itemid': egen sx = total(wx)
		bysort `itemid': egen sweight = total(`fweight')
		replace sx = sx / sweight
		drop sweight
	}
	label var sx "Weighted average household consumption share of item"
	drop wx
	*rank share
	gsort -sx `itemid' `hhid'
	egen r = group(sx `itemid')
	quiet: summ r
	local rmax = r(max)
	local rcore = r(max) - `ncore'
	*get reduced items (core + one optional)
	local rreduced = floor((r(max) - `ncore') * (1-1/`nmodules'))
	*select core items
	gen c_x = `xvalue' if r>`rcore'
	replace `xvalue' = . if r>`rcore'
	*get next highest rank
	quiet: summ r if r<=`rcore'
	local r = r(max)
	*get food item ids in local macros
	levelsof `itemid', local(nlevels)
	levelsof `itemid' if r<=`rcore', local(nc_nlevels)
	levelsof `itemid' if r>`rcore', local(c_nlevels)
	levelsof `itemid' if r>=`rreduced', local(r_nlevels)
	*make wide dataset
	ren `xvalue' nc_x
	reshape wide nc_x c_x sx r, i(`hhid' `hhsize' `fweight') j(`itemid')
	*split core (c_x) and non-core (nc_x) items
	foreach v of varlist c_x* nc_x* {
		quiet: summ `v'
		if r(N)==0 drop `v'
		else {
			quiet: replace `v' = 0 if `v'>=.
			*per capita consumption
			gen `v'_pc = `v'/`hhsize'
		}
	}
	*assign correct ranks even if missing
	foreach id of local nlevels {
		egen rr`id' = max(r`id')
		drop r`id'
	}
	ren rr* r*
	*calculate total consumption
	egen tc = rowtotal(c_x*)
	egen tnc = rowtotal(nc_x*)
	gen t = tc + tnc
	egen tc_pc = rowtotal(c_x*_pc)
	egen tnc_pc = rowtotal(nc_x*_pc)
	gen t_pc = tc_pc + tnc_pc
	*get maximum number of variables per module
	local nmax = ceil(`: word count `nc_nlevels'' / `M')
	*prepare optional modules
	forvalues m = 1/`M' {
		*IDs in module
		local m`m'_id = ""
		*vars in module
		local m`m'_v = ""
		*number of variables in module
		local m`m'_n = 0
		*last R2 for module
		local m`m'_r2 = 0
	}
	*iteratively assign items to modules
	while `r'>0 {
		*get next item to assign to a module
		foreach id of local nc_nlevels {
			if r`id'[1]==`r' {
				local vnext = "nc_x`id'`pc'"
				local idnext = "`id'"
			}
		}
		*make sure that items are more or less equally distributed: don't allow diff in n >ndiff
		local n_largest = 0
		local n_lowest=`nmax'
		forvalues m = 1 / `M' {
			if `m`m'_n'>`n_largest' local n_largest = `m`m'_n'
			if `m`m'_n'<`n_lowest' local n_lowest = `m`m'_n'
		}
		if (`n_lowest' < `n_largest' - `ndiff') {
			*find lowest
			forvalues m = 1 / `M' {
				if `m`m'_n'==`n_lowest' local mmax=`m'
			}
			*update R2
			reg t`pc' `hhsize' `m`mmax'_v' `vnext'
			local diff_m`mmax'_r2 =  e(r2) - `m`mmax'_r2'
		}
		else {
			*find best module for item by highest R2 increase
			local mmax = 1
			forvalues m = 1 / `M' {
				*only assign to modules which are not yet full
				if (`m`m'_n'<`nmax') {
					reg t`pc' `hhsize' `m`m'_v' `vnext'
					local diff_m`m'_r2 =  e(r2) - `m`m'_r2'
					*get largest
					if (`diff_m`m'_r2' > `diff_m`mmax'_r2') local mmax = `m'
				}
				else local diff_m`m'_r2 = -1
			}
		}
		*add variable to model with largest R2 difference
		local m`mmax'_id = "`m`mmax'_id' `idnext' "
		local m`mmax'_v = "`m`mmax'_v' `vnext' "
		local m`mmax'_n = `m`mmax'_n' + 1
		local m`mmax'_r2 = `m`mmax'_r2' + `diff_m`mmax'_r2'
		*next variables
		local r = `r' - 1
	}

	*create assignment
	restore
	*aggregate
	bysort `hhid': egen hhtot = sum(`xvalue')
	gen hhshare = `xvalue' / hhtot
	collapse (mean) hhshare (sum) `xvalue' [pweight=`fweight'], by(`itemid')
	egen xtot = sum(`xvalue')
	gen totshare = `xvalue'/xtot
	drop xtot
	label var hhshare "Average Household Consumption Share"
	label var totshare "Consumption Share of Total Consumption"
	if ("`egalshare'"!="") {
		gsort -hhshare
	}
	else {
		gsort -totshare
	}
	*make assignment
	gen itemmod = .
	*core
	foreach id of local c_nlevels {
		quiet: replace itemmod = 0 if `itemid' == `id'
	}
	*non-core
	forvalues m = 1/`M' {
		foreach id of local m`m'_id {
			quiet: replace itemmod = `m' if `itemid'==`id'
		}
	}
	*reduced
	gen itemred = 0
	foreach id of local r_nlevels {
		quiet: replace itemred = 1 if `itemid' == `id'
	}
end

capture: program drop RCS_prepare_modct
program define RCS_prepare_modct
	syntax using/, dirbase(string) nmodules(integer) ndiff(integer) lc_sdTemp(string) fN(integer) nN(integer) [EGALshare]
/*
	*prepare output directories
	capture: mkdir "`dirbase'"
	local lc_sdTemp = "`dirbase'/modct/Temp"
	capture: mkdir "`dirbase'"
	local lc_sdOut = "`dirbase'/Out"
	capture: mkdir "`dirbase'"
*/	
	*create food and non-food files
	use "`using'", clear
	keep hhid hhsize weight xfood*
	*get labels
	capture: label drop lfood
	foreach v of varlist xfood* {
		local xxx = "`v'"
		label define lfood `: subinstr local xxx "xfood" ""' "`: var label `v''", add
	}
	*reshape
	quiet: reshape long xfood, i(hhid hhsize weight) j(foodid)
	*add labels and save
	label values foodid lfood
	quiet: save "`lc_sdTemp'/HH-Food.dta", replace
	use "`using'", clear
	keep hhid hhsize weight xnonfood*
	*get labels
	capture: label drop lnonfood
	foreach v of varlist xnonfood* {
		local xxx = "`v'"
		label define lnonfood `: subinstr local xxx "xnonfood" ""' "`: var label `v''", add
	}
	*reshape
	quiet: reshape long xnonfood, i(hhid hhsize weight) j(nonfoodid)
	*add labels and save
	label values nonfoodid lnonfood
	quiet: save "`lc_sdTemp'/HH-NonFood.dta", replace
	
	forvalue fi=1/`fN'{
	display "food core=" as text "`fi'" as result
	quiet{
	*CREATE PARTITIONS
	*food partition
	use "`lc_sdTemp'/HH-Food.dta", clear
	quiet: RCS_partition xfood, hhid("hhid") itemid("foodid") fweight("weight") hhsize("hhsize") nmodules(`nmodules') ncore(`fi') ndiff(`ndiff') `egalshare'
	gen itemcode = foodid
	order itemcode, before(foodid)
	*save "`lc_sdTemp'/fsim_fpartition.dta", replace
	gen count=1 
	collapse (sum) count, by(itemmod)
	save "`lc_sdTemp'/fsim_fpartition2_`fi'.dta", replace
	*export excel using "`lc_sdOut'/FoodConsumption.xls", replace first(var) sheet("Items")
	*collapse (sum) hhshare totshare, by(itemmod)
	*export excel using "`lc_sdOut'/FoodConsumption.xls", sheetreplace first(var) sheet("Module Share")
	*non-food partition
						}
		}				

	forvalue ni=1/`nN'{	
	display "Non-food core=" as text "`ni'" as result
	quiet{	
	use "`lc_sdTemp'/HH-NonFood.dta", clear
	quiet: RCS_partition xnonfood, hhid("hhid") itemid("nonfoodid") fweight("weight") hhsize("hhsize") nmodules(`nmodules') ncore(`ni') ndiff(`ndiff') `egalshare'
	gen itemcode = nonfoodid
	order itemcode, before(nonfoodid)
	*save assignment
	*save "`lc_sdTemp'/fsim_nfpartition.dta", replace
	gen count=1 
	collapse (sum) count, by(itemmod)
	save "`lc_sdTemp'/fsim_nfpartition2_`ni'.dta", replace
	*export excel using "`lc_sdOut'/NonFoodConsumption.xls", replace first(var) sheet("Items")
	*collapse (sum) hhshare totshare, by(itemmod)
	*export excel using "`lc_sdOut'/NonFoodConsumption.xls", sheetreplace first(var) sheet("Module Share")
						}
		}
end



	*parameters
	*number of modules
	local M = 4
	*number of simulations
	local N = 20
	*number of imputations 
	local nI = 20
	*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
	local ndiff = 3

	*Make temp directories
local lc_sdBase = "${gsdOutput}/SOM/modct/"
capture: mkdir "${gsdOutput}/SOM/modct"
local lc_sdTemp = "`lc_sdBase'/Temp"
capture: mkdir "`lc_sdTemp'"
local lc_sdOut = "`lc_sdBase'/Out"
capture: mkdir "`lc_sdOut'"


*start RCS code
*run simulation
local using= "`lc_sdTemp'/HHData.dta"
local dirout = "${gsdOutput}/SOM/modct/"
local nmodules = `M'
*local ncoref = 33
*local ncorenf = 25
local ndiff=`ndiff'
local nsim = `N'
local nmi = `nI'
local rseed = 23081980
local dirbase = "`dirout'/modct/"

local fN = 101
local nN = 81
RCS_prepare_modct using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff')  lc_sdTemp(`lc_sdTemp') fN(`fN') nN(`nN') 


use "`lc_sdTemp'/fsim_fpartition2_1.dta",clear
rename count m1
forvalue i=2/101{
merge 1:1 itemmod using "`lc_sdTemp'/fsim_fpartition2_`i'.dta"
rename count m`i'
drop _m
}
gen food=1
tempfile food
save `food'

use "`lc_sdTemp'/fsim_nfpartition2_1.dta",clear
rename count m1
forvalue i=2/81{
merge 1:1 itemmod using "`lc_sdTemp'/fsim_nfpartition2_`i'.dta"
rename count m`i'
drop _m
}
gen food=2
tempfile nfood
save `nfood'

use `food',clear
append using `nfood'
order food
label define food 1 "Food" 2 "Non-food"
label value food food

save "`lc_sdOut'/SOM_partition.dta", replace



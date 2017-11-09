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
set trace on



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
	local sData = "${gsdDataBox}"

	*local xdeflator = 1.094426
	local xpovline = 76.40


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
*for validation of the method, missing data is assumed to be zero
*as the consumption aggregate implicitly assumes.
*food consumption
use "`sData'/NBHS_FOOD.dta", clear
ren (item value) (foodid xfood)
include "${gsdDo}/SSD-labels.do"
label values foodid lfoodid
replace xfood=xfood*4 // This tests my hopo
keep hhid foodid xfood
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "${gsdTemp}/HH-FoodItems.dta", replace
*non food consumption
use "`sData'/NBHS_NONFOOD.dta", clear
ren (item q3) (nonfoodid xnonfood)
replace nonfoodid = 83003 if nonfoodid==830031
include "${gsdDo}/SSD-labels.do"
label values nonfoodid lnonfoodid
*module=5 implies 12m recall; module=4 is a 30d recall
replace xnonfood = xnonfood / 12 if module==5
*ATTENTION: Previous code removed households with missing non-food consumption
*as it is unlikely for aperiod of 12m.
*drop if nonfoodid>=.
replace nonfoodid = 11101 if nonfoodid>=.
keep hhid nonfoodid xnonfood
collapse (sum) xnonfood, by(hhid nonfoodid)
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "${gsdTemp}/HH-NonFoodItems.dta", replace

*get household characteristics
use "${gsdDataBox}/NBHS_IND.dta", clear
ren b41 age
gen age_child = age<15 if age<.
gen age_adult = inrange(age,15,64) if age<.
gen age_senior = age>64 if age<.
collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior, by(hhid cluster)
merge 1:1 hhid cluster using "${gsdDataBox}/NBHS_HH.dta", nogen assert(match) keep(match) keepusing(h1 h5 h9 h3 h7 h8 h10 i* head_sex head_age urban hhweight) 
ren (h1 h5 h9 h3 h7 h8 h10 hhweight) (hhhouse hhwater hhtoilet hhsleep hhlight hhcook hhwaste weight)
ren (head_sex head_age urban) (hhsex age strata)
replace hhsex =1 if hhsex>=.
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
*gen pwork = nwork / hhsize
*gen bwork = nwork>0
*collect durables but we won't use them for the moment
local li = "21 22 23 24 25 31 32 33 34 35 36 37 38 39"
gen xdurables = 0
foreach i of local li {
	replace xdurables = xdurables + i`i'_2 * i`i'_3 if i`i'_1==1 & (i`i'_2 * i`i'_3>0)
}
gen xdurables_pc = 0
replace xdurables_pc=xdurables/hhsize // ST added comment: why not used?
drop i*
*simplify by setting missing values to conservative answers
*type
recode hhhouse (1/2=1) (3/4=2) (5/20=3) (11=4) (-9=4) (.=4)
label define lhouse 1 "Tent" 2 "Tukul" 3 "House/Apt" 4 "Other", replace
label values hhhouse lhouse
*sleep
recode hhsleep (3/12=3) (-9=0)
label define lsleep 0 "None" 1 "1 Room" 2 "2 Rooms" 3 ">2 Rooms", replace
label values hhsleep lsleep
*water
recode hhwater (1/4=1) (5=2) (6/11=3) (11/12=4) (-9=4)
label define lwater 1 "Borehole" 2 "Hand pump" 3 "Open Water" 4 "Other"
label values hhwater lwater
*light
recode hhlight (1/5=1) (6/10=2) (11=3) (-9=3)
label define llight 1 "Gas / Paraffin" 2 "Other material" 3 "None", replace
label values hhlight llight
*cook
recode hhcook (-9=3) (3/9=3)
label define lcook 1 "Firewood" 2 "Charcoal" 3 "Other", replace
label values hhcook lcook
*toilet
recode hhtoilet (3/5=3) (6=4) (-9=4)
label define ltoilet 1 "Pit" 2 "Shared Pit" 3 "Flush/Bucket" 4 "None", replace
label values hhtoilet ltoilet
*waste
recode hhwaste (-9/2=4) (3=3) (4=2) (5=1) (6=4)
label define lwaste 1 "Burning" 2 "Heap" 3 "Pit" 4 "Other"
label values hhwaste lwaste
*add durables and food and non-food
merge 1:1 hhid using "${gsdTemp}/HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "${gsdTemp}/HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
save "${gsdTemp}/HHData.dta", replace

use "${gsdTemp}/HH-Food.dta", clear
RCS_partition xfood, hhid("hhid") itemid("foodid") fweight("weight") hhsize("hhsize") nmodules(4) ncore(33) ndiff(3)
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
	export excel using "`lc_sdOut'/FoodConsumption.xls", replace first(var) sheet("Items")
	collapse (sum) hhshare totshare, by(itemmod)
	export excel using "`lc_sdOut'/FoodConsumption.xls", sheetreplace first(var) sheet("Module Share")
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
local lc_sdBase = "${gsdOutput}/SSD/modct/"
capture: mkdir "${gsdOutput}/SSD/modct"
local lc_sdTemp = "${gsdTemp}"
capture: mkdir "`lc_sdTemp'"
local lc_sdOut = "`lc_sdBase'/Out"
capture: mkdir "`lc_sdOut'"


*start RCS code
*run simulation
local using= "${gsdTemp}/HHData.dta"
local dirout = "${gsdOutput}/SSD/modct/"
local nmodules = 4
*local ncoref = 33
*local ncorenf = 25
local ndiff= 3
local nsim = `N'
local nmi = `nI'
local rseed = 23081980
local dirbase = "${gsdOutput}"


local fN = 145
local nN = 114
RCS_prepare_modct using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff')  lc_sdTemp(`lc_sdTemp') fN(`fN') nN(`nN') 



use "`lc_sdTemp'/fsim_fpartition2_1.dta",clear
rename count m1
forvalue i=2/145{
merge 1:1 itemmod using "`lc_sdTemp'/fsim_fpartition2_`i'.dta"
rename count m`i'
drop _m
}
gen food=1
tempfile food
save `food'

use "`lc_sdTemp'/fsim_nfpartition2_1.dta",clear
rename count m1
forvalue i=2/114{
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

save "`lc_sdOut'/SSD_partition.dta", replace


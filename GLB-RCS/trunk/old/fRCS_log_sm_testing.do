*functions for RCS in which the imputed values are transformed with log. 
* RCS_run: runs the whole suite of programs
*
* RCS_partition: sub-program to partition items into modules
*
* RCS_prepare: prepares the dataset; runs the partition
* RCS_assign: assigns households to their modules for each simulation
* RCS_simulate: simulates RCS
* RCS_collate: collates results from the simulation
* RCS_analyze: analyzes the results


*calculate standard error
capture: program drop fse
program define fse, rclass
	args vref vest brelative
	tempvar d
	gen `d' = (`vest' - `vref')
	if (`brelative'==1) replace `d' = `d' / `vref'
	quiet: summ `d'
	local xb = r(mean)
	tempvar sd
	gen `sd' = (`d') ^2
	quiet: summ `sd'
	local xsd = sqrt(r(mean))
	return scalar bias = `xb'
	return scalar se = `xsd'
end

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

capture: program drop RCS_prepare
program define RCS_prepare
	syntax using/, dirbase(string) nmodules(integer) ncoref(integer) ncorenf(integer) ndiff(integer) [EGALshare]
	*prepare output directories
	capture: mkdir "`dirbase'"
	local lc_sdTemp = "`dirbase'/Temp"
	capture: mkdir "`dirbase'"
	local lc_sdOut = "`dirbase'/Out"
	capture: mkdir "`dirbase'"
	
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
	save "`lc_sdTemp'/HH-Food.dta", replace
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
	save "`lc_sdTemp'/HH-NonFood.dta", replace
	
	*CREATE PARTITIONS
	*food partition
	use "`lc_sdTemp'/HH-Food.dta", clear
	quiet: RCS_partition xfood, hhid("hhid") itemid("foodid") fweight("weight") hhsize("hhsize") nmodules(`nmodules') ncore(`ncoref') ndiff(`ndiff') `egalshare'
	gen itemcode = foodid
	order itemcode, before(foodid)
	save "`lc_sdTemp'/fsim_fpartition.dta", replace
	export excel using "`lc_sdOut'/FoodConsumption.xls", replace first(var) sheet("Items")
	collapse (sum) hhshare totshare, by(itemmod)
	export excel using "`lc_sdOut'/FoodConsumption.xls", sheetreplace first(var) sheet("Module Share")
	*non-food partition
	use "`lc_sdTemp'/HH-NonFood.dta", clear
	quiet: RCS_partition xnonfood, hhid("hhid") itemid("nonfoodid") fweight("weight") hhsize("hhsize") nmodules(`nmodules') ncore(`ncorenf') ndiff(`ndiff') `egalshare'
	gen itemcode = nonfoodid
	order itemcode, before(nonfoodid)
	*save assignment
	save "`lc_sdTemp'/fsim_nfpartition.dta", replace
	export excel using "`lc_sdOut'/NonFoodConsumption.xls", replace first(var) sheet("Items")
	collapse (sum) hhshare totshare, by(itemmod)
	export excel using "`lc_sdOut'/NonFoodConsumption.xls", sheetreplace first(var) sheet("Module Share")
end

capture: program drop RCS_assign
program define RCS_assign
	syntax using/, dirbase(string) nmodules(integer) nsim(integer) rseed(integer)
	set seed `rseed'
	local N = `nsim'
	local M = `nmodules'
	*prepare output directories
	local lc_sdTemp = "`dirbase'/Temp"
	local lc_sdOut = "`dirbase'/Out"
	*start iteration over simulations
	forvalues isim = 1 / `N' {
		*get household assignment to modules
		use "`using'", clear
		keep hhid cluster weight 
		*all EAs have 9 households, make sure that 9th household gets not same module for all EAs
		gen r = runiform()
		sort cluster r
		*get random start value for sequence for each cluster
		by cluster: egen i = seq()
		quiet: gen module = 1+int((4-1+1)*runiform()) if i==1
		quiet: bysort cluster: replace module = mod(module[_n-1],4)+1 if i>1
		label var module "Optional food module assigned to household"
		*get non-food module
	*	gen add = cluster
	*	replace add = cluster - 400+55 if cluster>=400
	*	replace add = mod(add,4)
	*	gen module_nf = mod(module+add-1,4) + 1 
	*	recode module_nf (3=4) (4=3) (1=2) (2=1) if cluster > 50
		quiet: gen module_nf = module
		keep hhid module module_nf
		ren module hhmod_f 
		ren module_nf hhmod_nf
		label var hhmod_nf "Optional non-food module assigned to household"
		save "`lc_sdTemp'/HH-ModuleAssignment.dta", replace

		*prepare dataset for MI 
		*FOOD CONSUMPTION
*		local lc_sdTemp "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4\Temp"
*		local M=4
		use "`lc_sdTemp'/HH-Food.dta", clear
		*get household assigned module
		quiet: merge m:1 hhid using "`lc_sdTemp'/HH-ModuleAssignment.dta", nogen assert(match) keepusing(hhmod_f)
		ren hhmod_f hhmod
		*add food assigned  module
		quiet: merge m:1 foodid using "`lc_sdTemp'/fsim_fpartition.dta", nogen assert(match) keepusing(itemmod itemred)
		*get total food consumption
		bysort hhid: egen  xfood_t = sum(xfood) // The total of the original
		*get reduced food consumption
		gen  xfred = xfood if itemred
		quiet: bysort hhid: egen  xfood_r = sum(xfred) // The total of the reduced
		drop xfred
		quiet: clonevar cfood=xfood // Keep the original value as c*
		*randomly assign module to households
		quiet: replace xfood = . if (itemmod>0) & (itemmod!=hhmod)
		quiet: bysort hhid: egen  xfood_t2 = sum(xfood) // The total of the moduled
		save "`lc_sdTemp'/food-consumption.dta", replace // ST added
		*aggregate by modules; use itemmod to consider core module
		collapse (sum) xfood cfood (mean) xfood_t xfood_t2 xfood_r (firstnm) weight, by(hhid hhmod itemmod)
		keep hhid hhmod xfood xfood_t xfood_r itemmod weight cfood xfood_t2 
		ren xfood* xfcons*
		ren cfood* cfcons*
		quiet: reshape wide xfcons cfcons, i(hhid hhmod weight xfcons_t xfcons_t2 xfcons_r) j(itemmod)
*		quiet: reshape wide xfcons oxfcons, i(hhid) j(itemmod)
		*ensure zero entries are not missing
		forvalues jmod = 1/`M' {
			quiet: replace xfcons`jmod' = 0 if (xfcons`jmod'>=.) & (hhmod==`jmod')
			quiet: replace xfcons`jmod' = . if (hhmod!=`jmod')
		}
		save "`lc_sdTemp'/hh-food-consumption.dta", replace
		*NON-FOOD CONSUMPTION
		use "`lc_sdTemp'/HH-NonFood.dta", clear
		*get household assigned module
		quiet: merge m:1 hhid using "`lc_sdTemp'/HH-ModuleAssignment.dta", nogen keep(match) keepusing(hhmod_nf)
		ren hhmod_nf hhmod
		*add food assigned  module
		quiet: merge m:1 nonfoodid using "`lc_sdTemp'/fsim_nfpartition.dta", nogen keep(match) keepusing(itemmod itemred)
		*get total food consumption
		quiet: bysort hhid: egen   xnonfood_t = sum(xnonfood) // The total of the original
		*get reduced food consumption
		gen   xnonfred = xnonfood if itemred
		quiet: bysort hhid: egen  xnonfood_r = sum(xnonfred)  // The total of the reduced
		drop xnonfred
		quiet: clonevar cnonfood=xnonfood // Keep the original value
		*randomly assign module to households
		quiet: replace xnonfood = . if (itemmod>0) & (itemmod!=hhmod)
		quiet: bysort hhid: egen xnonfood_t2 = sum(xnonfood) // The total of the moduled
		save "`lc_sdTemp'/nonfood-consumption.dta", replace // ST added
		*aggregate by modules; use itemmod to consider core module
		collapse (sum) xnonfood cnonfood (mean) xnonfood_t xnonfood_t2 xnonfood_r, by(hhid hhmod itemmod)
		keep hhid xnonfood xnonfood_t xnonfood_r hhmod itemmod xnonfood cnonfood xnonfood_t2
		ren xnonfood* xnfcons*
		ren cnonfood* cnfcons*
		quiet: reshape wide xnfcons cnfcons, i(hhid hhmod xnfcons_t xnfcons_t2 xnfcons_r) j(itemmod)
		*ensure zero entries are not missing
		forvalues jmod = 1/`M' {
			quiet: replace xnfcons`jmod' = 0 if (xnfcons`jmod'>=.) & (hhmod==`jmod')
			quiet: replace xnfcons`jmod' = . if (hhmod!=`jmod')
		}
		save "`lc_sdTemp'/hh-nonfood-consumption.dta", replace
		
		*merge datasets (For hh level data)
		use "`using'", clear
		drop xfood* xnonfood*
		quiet: merge 1:1 hhid using "`lc_sdTemp'/hh-food-consumption.dta", nogen assert(master match)
		quiet: merge 1:1 hhid using "`lc_sdTemp'/hh-nonfood-consumption.dta", nogen assert(master match)
		order x*, last
		*there are a few households apparently without consumption, we drop them
		quiet: drop if xfcons0>=.
		capture: quiet: drop if hhcook_1>=.
		*get per capita variables
		ren xfcons_t2 mfcons // moduled sum (m* in the final data)
		ren xnfcons_t2 mnfcons // moduled sum (m* in the final data)
		ren (x*cons_t x*cons_r) (c*cons r*cons) // (original and reduced) (to c* r* in the end)
		foreach v of varlist xfcons* xnfcons* cfcons* cnfcons* rfcons rnfcons mfcons mnfcons {
			quiet: gen `v'_pc = `v'/hhsize
		} 
		*create percentiles
		foreach v of varlist xfcons0_pc xnfcons0_pc xdurables_pc {
			xtile p`v' = `v' [pweight=weight], nquantiles(4)
		}

		*prepare check variables
		quiet: gen ccons_pc = cfcons_pc + cnfcons_pc + xdurables_pc  // original (complete), ccons_pc=fcons_pc 
		quiet: gen rcons_pc = rfcons_pc + rnfcons_pc + xdurables_pc // reduced
		quiet: gen mcons_pc = mfcons_pc + mnfcons_pc + xdurables_pc // moduled
		
		forvalue d=0/4{
		label var xfcons`d'_pc "PC RCS food cons. in module `d'"
		label var cfcons`d'_pc "PC original food cons. in module `d'"
		label var xnfcons`d'_pc "PC RCS non-food cons. in module `d'"
		label var cnfcons`d'_pc "PC original non-food cons. in module `d'"
		}
		label var rfcons_pc "PC reduced food cons."
		label var rnfcons_pc "PC reduced non-food cons."
		
		label var mfcons_pc "RCS PC food nominal cons"
		label var mnfcons_pc "RCS PC non-food nominal cons"
		label var cfcons_pc "Original nominal PC food cons" 
		label var cnfcons_pc "Original nominal PC non-food cons"
		label var ccons_pc "Original nominal PC cons"
		label var rcons_pc "Reduced nominal PC cons" 
		label var mcons_pc "RCS nominal PC cons"
		
		save "`lc_sdTemp'/mi_`isim'.dta", replace
		
		*Prepare item level database to get item shares
		use "`lc_sdTemp'/food-consumption.dta",clear
		rename foodid itemid
		rename xfood cons_value
		label var cons_value "RCS Expenditure (1,000 shillings/household/month)"
		rename cfood fcons_value
		label var fcons_value "Original Expenditure (1,000 shillings/household/month)"
		rename hhmod mod_hh 
		rename itemmod  mod_item
		drop xfood_t xfood_r xfood_t2 itemred
		save "`lc_sdTemp'/food-consumption_mi_`isim'.dta",replace
		
		use "`lc_sdTemp'/nonfood-consumption.dta",clear
		rename nonfoodid itemid
		rename xnonfood cons_value
		label var cons_value "RCS Expenditure (1,000 shillings/household/month)"
		rename cnonfood fcons_value
		label var fcons_value "Original Expenditure (1,000 shillings/household/month)"
		rename hhmod mod_hh 
		rename itemmod  mod_item
		drop xnonfood_t xnonfood_r xnonfood_t2 itemred
		save "`lc_sdTemp'/nonfood-consumption_mi_`isim'.dta",replace
		
		}
end


capture: program drop RCS_simulate
program define RCS_simulate
	syntax using/, dirbase(string) nmodules(integer) nsim(integer) nmi(integer) lmethod(namelist) model(string) model2(string) model3(string) rseed(integer) co(string)
	*prepare output directories
	local lc_sdTemp = "`dirbase'/Temp"
	local lc_sdOut = "`dirbase'/Out"
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'

	set seed `rseed'
	
	*start iteration over simulations
	forvalues isim = 1 / `N' {
		*ESTIMATE BASED ON DIFFERENT METHODS
		foreach smethod of local lmethod {
			*change directory to make sure we are in a writable directory for some mi commands
			cd "`lc_sdTemp'"
			*load prepared dataset for mi
			use "`lc_sdTemp'/mi_`isim'.dta", clear
			*prepare variables
			quiet: gen xcons_pc = .
			quiet: gen xfcons_pc = .
			quiet: foreach v of varlist xfcons?_pc xnfcons?_pc xdurables_pc {
				*save original
				gen o`v' = `v'
				*remove outliers
				summ `v',d
				replace `v'= `r(p99)' if (`v'>`r(p99)') & (`v'<.)
				*replace `v'= 0.1 if `v'==.
				*replace `v'= 0.1 if `v'<0.1 // This low value truncation is justified since we take log(Y+0.1) in order to avoid too small number becomes lower bound. 
				*log transform
				gen ln`v' = log(`v'+0.1)
			}
			*method selection
			local mipre = ""
			if ("`smethod'"=="avg") {
				quiet: forvalues imod = 1/`M' {
					egen lnavg_xfcons`imod'_pc = mean(lnxfcons`imod'_pc)
					replace lnxfcons`imod'_pc = lnavg_xfcons`imod'_pc if xfcons`imod'_pc>=.
					egen lnavg_xnfcons`imod'_pc = mean(lnxfcons`imod'_pc)
					replace lnxnfcons`imod'_pc = lnavg_xnfcons`imod'_pc if xnfcons`imod'_pc>=.
				}
				drop lnavg_x*
			}
			else if ("`smethod'"=="med") {
				quiet: forvalues imod = 1/`M' {
					egen lnavg_xfcons`imod'_pc = median(lnxfcons`imod'_pc)
					replace lnxfcons`imod'_pc = lnavg_xfcons`imod'_pc if xfcons`imod'_pc>=.
					egen lnavg_xnfcons`imod'_pc = median(lnxfcons`imod'_pc)
					replace lnxnfcons`imod'_pc = lnavg_xnfcons`imod'_pc if xnfcons`imod'_pc>=.
				}
				drop lnavg_x*
			}
			
			else if ("`smethod'"=="tobit") {
*				quiet: forvalues imod = 1/`M' {
				forvalues imod = 1/`M' {
				*food
					tobit lnxfcons`imod'_pc xfcons0_pc xnfcons0_pc xdurables_pc `model' [aweight=weight], ll // To make this work with logY, changed ll(0) to ll. i.cluster dropped due to overfitting, but can include higher location.					
					mata: cond(st_matrix("e(V)"))
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					matrix btobit=e(b)
					scalar sigma=btobit[1,e(df_m)+2]
					scalar gamma=ln(0.1)
					replace xfcons`imod'_pc = exp(lny`imod'_pc+0.5*sigma^2)*(1-normal((gamma-lny`imod'_pc-sigma^2)/sigma)) if xfcons`imod'_pc>=. 
					drop lny`imod'_pc
			
					*non-food
					tobit lnxnfcons`imod'_pc xfcons0_pc xnfcons0_pc xdurables_pc `model' [aweight=weight], ll // To make this work with logY, changed ll(0) to ll. i.cluster dropped due to overfitting, but can include higher location.					
					mata: cond(st_matrix("e(V)"))
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					matrix btobit=e(b)
					scalar sigma=btobit[1,e(df_m)+2]
					scalar gamma=ln(0.1)
					replace xnfcons`imod'_pc= exp(lny`imod'_pc+0.5*sigma^2)*(1-normal((gamma-lny`imod'_pc-sigma^2)/sigma)) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc
					}
			}
			
			else if ("`smethod'"=="tobit2") {
*				quiet: forvalues imod = 1/`M' {
				forvalues imod = 1/`M' {
				*food
					tobit lnxfcons`imod'_pc xfcons0_pc xnfcons0_pc xdurables_pc `model2' [aweight=weight], ll // To make this work with logY, changed ll(0) to ll. i.cluster dropped due to overfitting, but can include higher location.					
					mata: cond(st_matrix("e(V)"))
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					matrix btobit=e(b)
					scalar sigma=btobit[1,e(df_m)+2]
					scalar gamma=ln(0.1)
					replace xfcons`imod'_pc = exp(lny`imod'_pc+0.5*sigma^2)*(1-normal((gamma-lny`imod'_pc-sigma^2)/sigma)) if xfcons`imod'_pc>=. 
					drop lny`imod'_pc
				
					*non-food
					tobit lnxnfcons`imod'_pc xfcons0_pc xnfcons0_pc xdurables_pc `model2' [aweight=weight], ll // To make this work with logY, changed ll(0) to ll. i.cluster dropped due to overfitting, but can include higher location.					
					mata: cond(st_matrix("e(V)"))
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					matrix btobit=e(b)
					scalar sigma=btobit[1,e(df_m)+2]
					scalar gamma=ln(0.1)
					replace xnfcons`imod'_pc= exp(lny`imod'_pc+0.5*sigma^2)*(1-normal((gamma-lny`imod'_pc-sigma^2)/sigma)) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc
					}
			}


			else if ("`smethod'"=="tobit3") {
*				quiet: forvalues imod = 1/`M' {
				forvalues imod = 1/`M' {
				*food
					tobit lnxfcons`imod'_pc xfcons0_pc xnfcons0_pc xdurables_pc `model3' [aweight=weight], ll // To make this work with logY, changed ll(0) to ll. i.cluster dropped due to overfitting, but can include higher location.					
					mata: cond(st_matrix("e(V)"))
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					matrix btobit=e(b)
					scalar sigma=btobit[1,e(df_m)+2]
					scalar gamma=ln(0.1)
					replace xfcons`imod'_pc = exp(lny`imod'_pc+0.5*sigma^2)*(1-normal((gamma-lny`imod'_pc-sigma^2)/sigma)) if xfcons`imod'_pc>=. 
					drop lny`imod'_pc
				
					*non-food
					tobit lnxnfcons`imod'_pc xfcons0_pc xnfcons0_pc xdurables_pc `model3' [aweight=weight], ll // To make this work with logY, changed ll(0) to ll. i.cluster dropped due to overfitting, but can include higher location.					
					mata: cond(st_matrix("e(V)"))
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					matrix btobit=e(b)
					scalar sigma=btobit[1,e(df_m)+2]
					scalar gamma=ln(0.1)
					replace xnfcons`imod'_pc= exp(lny`imod'_pc+0.5*sigma^2)*(1-normal((gamma-lny`imod'_pc-sigma^2)/sigma)) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc
					}
			}
			
			else if ("`smethod'"=="reg") {
				*quiet: forvalues imod = 1/`M' {
				forvalues imod = 1/`M' {
					*food
	*				reg lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight] // Dropped i.cluster due to overfitting. 
					reg lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' [aweight=weight]
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc=lny`imod'_pc if xfcons`imod'_pc>=. // Prediction is reflected in lnvar (zero is exp(ln(0.1))-0.1=0)
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod'
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"
					drop lny`imod'_pc resid`imod' exp_res_`imod'
					
					*non-food
    *				reg lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight]  // Dropped i.cluster due to overfitting. 
					reg lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' [aweight=weight]
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc=lny`imod'_pc if xnfcons`imod'_pc>=. // Prediction is reflected in lnvar (zero is exp(ln(0.1))-0.1=0)
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod'
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					drop lny`imod'_pc resid`imod' exp_res_`imod'

					display as text "End of " as result "`M'"					
				}			
				}
			
						else if ("`smethod'"=="reg2") {
				*quiet: forvalues imod = 1/`M' {
				forvalues imod = 1/`M' {
					*food
	*				reg lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight] // Dropped i.cluster due to overfitting. 
					reg lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2' [aweight=weight]
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc=lny`imod'_pc if xfcons`imod'_pc>=. // Prediction is reflected in lnvar (zero is exp(ln(0.1))-0.1=0)
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod'
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"
					drop lny`imod'_pc resid`imod' exp_res_`imod'
					
					*non-food
    *				reg lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight]  // Dropped i.cluster due to overfitting. 
					reg lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2' [aweight=weight]
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc=lny`imod'_pc if xnfcons`imod'_pc>=. // Prediction is reflected in lnvar (zero is exp(ln(0.1))-0.1=0)
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod'
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					drop lny`imod'_pc resid`imod' exp_res_`imod'

					display as text "End of " as result "`M'"					
				}
			}


						else if ("`smethod'"=="reg3") {
				*quiet: forvalues imod = 1/`M' {
				forvalues imod = 1/`M' {
					*food
	*				reg lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight] // Dropped i.cluster due to overfitting. 
					reg lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3' [aweight=weight]
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc=lny`imod'_pc if xfcons`imod'_pc>=. // Prediction is reflected in lnvar (zero is exp(ln(0.1))-0.1=0)
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod'
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"
					drop lny`imod'_pc resid`imod' exp_res_`imod'
					
					*non-food
    *				reg lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight]  // Dropped i.cluster due to overfitting. 
					reg lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3' [aweight=weight]
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc=lny`imod'_pc if xnfcons`imod'_pc>=. // Prediction is reflected in lnvar (zero is exp(ln(0.1))-0.1=0)
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod'
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					drop lny`imod'_pc resid`imod' exp_res_`imod'

					display as text "End of " as result "`M'"					
				}
			}
			

			else if ("`smethod'"=="twopart") {
				forvalues imod = 1/`M' {
					*food
					gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
					replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
					probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'           [pweight=weight] // part I specification
					predict p`imod'_pc 
					regress lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' [aweight=weight] if dxfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc = lny`imod'_pc if xfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"						
					replace xfcons`imod'_pc= round((exp(lnxfcons`imod'_pc ))*`smear_fd`imod''*p`imod'_pc, 0.0001) if xfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc

					*non-food
					gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
					replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
					probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'           [pweight=weight] // part I specification
					predict p`imod'_pc
					regress lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' [pweight=weight] if dxnfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc = lny`imod'_pc if xnfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					replace xnfcons`imod'_pc=round((exp(lnxnfcons`imod'_pc))*`smear_nf`imod''*p`imod'_pc, 0.0001) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc
				}
			}
			

			else if ("`smethod'"=="twopart2") {
				forvalues imod = 1/`M' {
					*food
					gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
					replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
					probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2'           [pweight=weight] // part I specification
					predict p`imod'_pc 
					regress lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2' [aweight=weight] if dxfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc = lny`imod'_pc if xfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"
					replace xfcons`imod'_pc= round((exp(lnxfcons`imod'_pc ))*`smear_fd`imod''*p`imod'_pc, 0.0001) if xfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc
					
					*non-food
					gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
					replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
					probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2'           [pweight=weight] // part I specification
					predict p`imod'_pc 
					regress lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2' [pweight=weight] if dxnfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc = lny`imod'_pc if xnfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					replace xnfcons`imod'_pc=round((exp(lnxnfcons`imod'_pc))*`smear_nf`imod''*p`imod'_pc, 0.0001) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc
					}
			}

			
			else if ("`smethod'"=="twopart3") {
				forvalues imod = 1/`M' {
					*food
					gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
					replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
					probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3'           [pweight=weight] // part I specification
					predict p`imod'_pc 
					regress lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3' [aweight=weight] if dxfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc = lny`imod'_pc if xfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"
					replace xfcons`imod'_pc= round((exp(lnxfcons`imod'_pc ))*`smear_fd`imod''*p`imod'_pc, 0.0001) if xfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc			
					
					*non-food
					gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
					replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
					probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3'           [pweight=weight] // part I specification
					predict p`imod'_pc 
					regress lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3' [pweight=weight] if dxnfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc = lny`imod'_pc if xnfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					replace xnfcons`imod'_pc=round((exp(lnxnfcons`imod'_pc))*`smear_nf`imod''*p`imod'_pc, 0.0001) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc
					}
			}
			
			
			
			else if ("`smethod'"=="twopartII1") {
				forvalues imod = 1/`M' {
					*food
					gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
					replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
					probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'           [pweight=weight] // part I specification
					predict p`imod'_pc  if xfcons`imod'_pc==. // predicted prob used for missing data
					predict p if xfcons`imod'_pc!=. // predicted prob in sample used to get cut-off prob. 
					*Obtain sample probability of Y=0
					su dxfcons`imod'_pc [aweight=weight] 
					local spr=r(mean)
					display "Sample probability=" as text `spr' as result
					gsort +p // Sort predicted probabilities from smaller to larger.
					gen order=_n // Put 1-N
					su p 
					local N=r(N) // Sample size (in sample)
					*This will produce all cases when the number of households with zero will increase one by one.
					quiet: forvalue i=1/`N' {
					gen _`i'=0 if (`i'>=order) 
					replace _`i'=1 if p!=. & _`i'==.
											}
					*This will detect and record when the predicted mean of each case above is close (or the smallest) to the sample mean.  
					quiet: forvalue i=1/`N' {
					su _`i' [aweight=weight]
					local pr_mean=r(mean)
					if (`pr_mean'>=`spr') {
														local thresh=`i'
													   }
					                 }
					display "Chosen cut-off order=" as text `thresh' as result
					su p if order==`thresh'
					local prob=r(mean)
					display "Chosen cut-off probability=" as text `prob' 

					regress lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' [aweight=weight] if dxfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc = lny`imod'_pc if xfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"						

					replace xfcons`imod'_pc =  0 if (p`imod'_pc < `prob') & xfcons`imod'_pc==. // Have zero when predicted probabilities are smaller than cut-off prob identified above. 
					replace xfcons`imod'_pc= round((exp(lnxfcons`imod'_pc ))*`smear_fd`imod'', 0.0001) if xfcons`imod'_pc>=. // Use (unconditional) expection of Y  when Y!=0. with smearing
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc  order p _*
					
					*non-food
					gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
					replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
					probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'           [pweight=weight] // part I specification
					predict p`imod'_pc if xnfcons`imod'_pc==. // predicted prob used for missing data
					predict p if xnfcons`imod'_pc!=. // predicted prob in sample used to get cut-off prob. 
					*Obtain sample probability of Y=0
					su dxnfcons`imod'_pc [aweight=weight] 
					local spr=r(mean)
					display "Sample probability=" as text `spr' as result
					gsort +p // Sort predicted probabilities from smaller to larger.
					gen order=_n // Put 1-N
					su p 
					local N=r(N) // Sample size (in sample)
					*This will produce all cases when households with one (Y>0) will increase one by one from the smallest to the largest predicted probabilities. 
					quiet: forvalue i=1/`N' {
					gen _`i'=0 if (`i'>=order) 
					replace _`i'=1 if p!=. & _`i'==.
											}
					*This will produce all cases when the number of households with zero will increase one by one.
					quiet: forvalue i=1/`N' {
					su _`i' [aweight=weight]
					local pr_mean=r(mean)
					if (`pr_mean'>=`spr') {
														local thresh=`i'
													   }
					                 }
					display "Chosen cut-off order=" as text `thresh' as result
					su p if order==`thresh'
					local prob=r(mean)
					display "Chosen cut-off probability=" as text `prob' 
				
					regress lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' [pweight=weight] if dxnfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc = lny`imod'_pc if xnfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					
					replace xnfcons`imod'_pc =  0 if (p`imod'_pc < `prob') & xnfcons`imod'_pc==. // Have zero when predicted probabilities are smaller than cut-off prob identified above. 
					replace xnfcons`imod'_pc=round((exp(lnxnfcons`imod'_pc))*`smear_nf`imod'', 0.0001) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc order p _*
				}
			}
			

			else if ("`smethod'"=="twopartII2") {
				forvalues imod = 1/`M' {
					*food
					gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
					replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
					probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2'           [pweight=weight] // part I specification
					predict p`imod'_pc  if xfcons`imod'_pc==. // predicted prob used for missing data
					predict p if xfcons`imod'_pc!=. // predicted prob in sample used to get cut-off prob. 
					*Obtain sample probability of Y=0
					su dxfcons`imod'_pc [aweight=weight] 
					local spr=r(mean)
					display "Sample probability=" as text `spr' as result
					gsort +p // Sort predicted probabilities from smaller to larger.
					gen order=_n // Put 1-N
					su p 
					local N=r(N) // Sample size (in sample)
					*This will produce all cases when the number of households with zero will increase one by one.
					quiet: forvalue i=1/`N' {
					gen _`i'=0 if (`i'>=order) 
					replace _`i'=1 if p!=. & _`i'==.
											}
					*This will detect and record when the predicted mean of each case above is close (or the smallest) to the sample mean.  
					quiet: forvalue i=1/`N' {
					su _`i' [aweight=weight]
					local pr_mean=r(mean)
					if (`pr_mean'>=`spr') {
														local thresh=`i'
													   }
					                 }
					display "Chosen cut-off order=" as text `thresh' as result
					su p if order==`thresh'
					local prob=r(mean)
					display "Chosen cut-off probability=" as text `prob' 

					regress lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2' [aweight=weight] if dxfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc = lny`imod'_pc if xfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"						

					replace xfcons`imod'_pc =  0 if (p`imod'_pc < `prob') & xfcons`imod'_pc==. // Have zero when predicted probabilities are smaller than cut-off prob identified above. 
					replace xfcons`imod'_pc= round((exp(lnxfcons`imod'_pc ))*`smear_fd`imod'', 0.0001) if xfcons`imod'_pc>=. // Use (unconditional) expection of Y  when Y!=0. with smearing
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc  order p _*
					
					*non-food
					gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
					replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
					probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2'           [pweight=weight] // part I specification
					predict p`imod'_pc if xnfcons`imod'_pc==. // predicted prob used for missing data
					predict p if xnfcons`imod'_pc!=. // predicted prob in sample used to get cut-off prob. 
					*Obtain sample probability of Y=0
					su dxnfcons`imod'_pc [aweight=weight] 
					local spr=r(mean)
					display "Sample probability=" as text `spr' as result
					gsort +p // Sort predicted probabilities from smaller to larger.
					gen order=_n // Put 1-N
					su p 
					local N=r(N) // Sample size (in sample)
					*This will produce all cases when households with one (Y>0) will increase one by one from the smallest to the largest predicted probabilities. 
					quiet: forvalue i=1/`N' {
					gen _`i'=0 if (`i'>=order) 
					replace _`i'=1 if p!=. & _`i'==.
											}
					*This will produce all cases when the number of households with zero will increase one by one.
					quiet: forvalue i=1/`N' {
					su _`i' [aweight=weight]
					local pr_mean=r(mean)
					if (`pr_mean'>=`spr') {
														local thresh=`i'
													   }
					                 }
					display "Chosen cut-off order=" as text `thresh' as result
					su p if order==`thresh'
					local prob=r(mean)
					display "Chosen cut-off probability=" as text `prob' 
				
					regress lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model2' [pweight=weight] if dxnfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc = lny`imod'_pc if xnfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					
					replace xnfcons`imod'_pc =  0 if (p`imod'_pc < `prob') & xnfcons`imod'_pc==. // Have zero when predicted probabilities are smaller than cut-off prob identified above. 
					replace xnfcons`imod'_pc=round((exp(lnxnfcons`imod'_pc))*`smear_nf`imod'', 0.0001) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc order p _*
				}
			}
						

			else if ("`smethod'"=="twopartII3") {
				forvalues imod = 1/`M' {
					*food
					gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
					replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
					probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3'           [pweight=weight] // part I specification
					predict p`imod'_pc  if xfcons`imod'_pc==. // predicted prob used for missing data
					predict p if xfcons`imod'_pc!=. // predicted prob in sample used to get cut-off prob. 
					*Obtain sample probability of Y=0
					su dxfcons`imod'_pc [aweight=weight] 
					local spr=r(mean)
					display "Sample probability=" as text `spr' as result
					gsort +p // Sort predicted probabilities from smaller to larger.
					gen order=_n // Put 1-N
					su p 
					local N=r(N) // Sample size (in sample)
					*This will produce all cases when the number of households with zero will increase one by one.
					quiet: forvalue i=1/`N' {
					gen _`i'=0 if (`i'>=order) 
					replace _`i'=1 if p!=. & _`i'==.
											}
					*This will detect and record when the predicted mean of each case above is close (or the smallest) to the sample mean.  
					quiet: forvalue i=1/`N' {
					su _`i' [aweight=weight]
					local pr_mean=r(mean)
					if (`pr_mean'>=`spr') {
														local thresh=`i'
													   }
					                 }
					display "Chosen cut-off order=" as text `thresh' as result
					su p if order==`thresh'
					local prob=r(mean)
					display "Chosen cut-off probability=" as text `prob' 

					regress lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3' [aweight=weight] if dxfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc = lny`imod'_pc if xfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"						

					replace xfcons`imod'_pc =  0 if (p`imod'_pc < `prob') & xfcons`imod'_pc==. // Have zero when predicted probabilities are smaller than cut-off prob identified above. 
					replace xfcons`imod'_pc= round((exp(lnxfcons`imod'_pc ))*`smear_fd`imod'', 0.0001) if xfcons`imod'_pc>=. // Use (unconditional) expection of Y  when Y!=0. with smearing
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc  order p _*
					
					*non-food
					gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
					replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
					probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3'           [pweight=weight] // part I specification
					predict p`imod'_pc if xnfcons`imod'_pc==. // predicted prob used for missing data
					predict p if xnfcons`imod'_pc!=. // predicted prob in sample used to get cut-off prob. 
					*Obtain sample probability of Y=0
					su dxnfcons`imod'_pc [aweight=weight] 
					local spr=r(mean)
					display "Sample probability=" as text `spr' as result
					gsort +p // Sort predicted probabilities from smaller to larger.
					gen order=_n // Put 1-N
					su p 
					local N=r(N) // Sample size (in sample)
					*This will produce all cases when households with one (Y>0) will increase one by one from the smallest to the largest predicted probabilities. 
					quiet: forvalue i=1/`N' {
					gen _`i'=0 if (`i'>=order) 
					replace _`i'=1 if p!=. & _`i'==.
											}
					*This will produce all cases when the number of households with zero will increase one by one.
					quiet: forvalue i=1/`N' {
					su _`i' [aweight=weight]
					local pr_mean=r(mean)
					if (`pr_mean'>=`spr') {
														local thresh=`i'
													   }
					                 }
					display "Chosen cut-off order=" as text `thresh' as result
					su p if order==`thresh'
					local prob=r(mean)
					display "Chosen cut-off probability=" as text `prob' 
				
					regress lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model3' [pweight=weight] if dxnfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc = lny`imod'_pc if xnfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					
					replace xnfcons`imod'_pc =  0 if (p`imod'_pc < `prob') & xnfcons`imod'_pc==. // Have zero when predicted probabilities are smaller than cut-off prob identified above. 
					replace xnfcons`imod'_pc=round((exp(lnxnfcons`imod'_pc))*`smear_nf`imod'', 0.0001) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc order p _*
				}
			}
			
			
			
			else if ("`smethod'"=="MImvn" | "`smethod'"=="MICE") {
				local mipre = "mi passive: "
				*run MI
				mi set wide
				mi register imputed lnxfcons1_pc lnxnfcons1_pc lnxfcons2_pc lnxnfcons2_pc lnxfcons3_pc lnxnfcons3_pc lnxfcons4_pc lnxnfcons4_pc
				mi register regular oxfcons0_pc oxnfcons0_pc oxfcons1_pc oxnfcons1_pc oxfcons2_pc oxnfcons2_pc oxfcons3_pc oxnfcons3_pc oxfcons4_pc oxnfcons4_pc
				mi register regular hh* cluster xfcons0_pc xnfcons0_pc pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc
				mi register passive xcons_pc xfcons_pc xfcons1_pc xnfcons1_pc xfcons2_pc xnfcons2_pc xfcons3_pc xnfcons3_pc xfcons4_pc xnfcons4_pc
				*MI method selection
				if ("`smethod'"=="MImvn") {
					*Multi-variate normal imputation using MCMC
				mi impute mvn lnxfcons1_pc lnxfcons2_pc lnxfcons3_pc lnxfcons4_pc lnxnfcons1_pc lnxnfcons2_pc lnxnfcons3_pc lnxnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model', add(`nI') burnin(1000) noisily
				}
				else if inlist("`smethod'", "MICE") {
					*Chained regressions to estimate the joint
	*				mi impute chained (regress) xfcons1_pc xfcons2_pc xfcons3_pc xfcons4_pc xnfcons1_pc xnfcons2_pc xnfcons3_pc xnfcons4_pc = xfcons0_pc xnfcons0_pc xdurables_pc hhsize pchild psenior i.hhsex i.hhwater hhcook_5 i.hhtoilet i.hhmaterial i.hhmode i.hhplot i.hhfood i.cluster i.hhmod, add(`nI') noimp report
				mi impute chained (regress) lnxfcons1_pc lnxfcons2_pc lnxfcons3_pc lnxfcons4_pc lnxnfcons1_pc lnxnfcons2_pc lnxnfcons3_pc lnxnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model', add(`nI') report noisily
				}
				}
				
				

			else if ("`smethod'"=="TwoMI") {
			*method selection
			local mipre = "mi passive: "
			
			*Make dummy for Y>0:
			forvalues imod = 1/4 {
			gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
			replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
			gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
			replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
			clonevar odxfcons`imod'_pc=dxfcons`imod'_pc // keep original for dummy
			clonevar odxnfcons`imod'_pc=dxnfcons`imod'_pc // keep original for dummy
									}

			
			display "`co'"
			
			*1st step:
			**set MI
			mi set wide
			mi register imputed dxfcons1_pc dxnfcons1_pc dxfcons2_pc dxnfcons2_pc dxfcons3_pc dxnfcons3_pc dxfcons4_pc dxnfcons4_pc
			mi register regular oxfcons0_pc oxnfcons0_pc oxfcons1_pc oxnfcons1_pc oxfcons2_pc oxnfcons2_pc oxfcons3_pc oxnfcons3_pc oxfcons4_pc oxnfcons4_pc
			mi register regular hh* cluster xfcons0_pc xnfcons0_pc pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc
			mi register passive xcons_pc xfcons_pc xfcons1_pc xnfcons1_pc xfcons2_pc xnfcons2_pc xfcons3_pc xnfcons3_pc xfcons4_pc xnfcons4_pc
			
			if ("`co'"=="SSD") {

			display "Data=" as text `isim' as result
			**MI method selection used: Chained regressions
			mi impute chained (logit) dxfcons1_pc dxfcons2_pc dxfcons3_pc dxfcons4_pc dxnfcons1_pc dxnfcons2_pc dxnfcons3_pc dxnfcons4_pc = pxfcons0_pc pxnfcons0_pc pxdurables_pc `model', add(`nI') report noisily augment force
			quietly: mi estimate: mean  dxfcons1_pc dxfcons2_pc dxfcons3_pc dxfcons4_pc dxnfcons1_pc dxnfcons2_pc dxnfcons3_pc dxnfcons4_pc	
																			
			**Obtain one predicted dy in the following lines:
			forvalues imod = 1/4 {
			egen m_dxfcons`imod'_pc=rowmean(_?_dxfcons`imod'_pc)
			egen m_dxnfcons`imod'_pc=rowmean(_?_dxnfcons`imod'_pc)
			replace dxfcons`imod'_pc=(m_dxfcons`imod'_pc>0.5)
			replace dxnfcons`imod'_pc=(m_dxnfcons`imod'_pc>0.5)
			}
							
								}
			
			else if ("`co'"=="SOM") {

						local nI=3
			local model = "hhsize pchild psenior i.hhsex i.hhwater i.hhcook hhsleep i.hhhouse i.hhtoilet i.hhwaste"
			
			**MI method selection used: Chained regressions
			mi impute chained (regress) dxfcons1_pc dxfcons2_pc dxfcons3_pc dxfcons4_pc dxnfcons1_pc dxnfcons2_pc dxnfcons3_pc dxnfcons4_pc = pxfcons0_pc pxnfcons0_pc pxdurables_pc `model', add(`nI') report noisily augment force savetrace(miest, replace)
			**Obtain one predicted dy in the following lines:	
			mi estimate: mean  dxfcons1_pc dxfcons2_pc dxfcons3_pc dxfcons4_pc dxnfcons1_pc dxnfcons2_pc dxnfcons3_pc dxnfcons4_pc	
			
			**Obtain one predicted dy in the following lines:
			forvalues imod = 1/4 {
			egen m_dxfcons`imod'_pc=rowmean(_?_dxfcons`imod'_pc)
			egen m_dxnfcons`imod'_pc=rowmean(_?_dxnfcons`imod'_pc)
			replace dxfcons`imod'_pc=(m_dxfcons`imod'_pc>0.5)
			replace dxnfcons`imod'_pc=(m_dxnfcons`imod'_pc>0.5)
			}
			
									}
									
									
					
			*2nd step:
			**Make ly missing when y=0 since we do not want to use them in MI impute for ly. 
			forvalues i = 1/4 {
			replace lnxfcons`i'_pc  =. if oxfcons`i'_pc==0
			replace lnxnfcons`i'_pc =. if oxnfcons`i'_pc==0
								}
								
			**set MI
			mi unset
			mi set wide
			mi register imputed lnxfcons1_pc lnxnfcons1_pc lnxfcons2_pc lnxnfcons2_pc lnxfcons3_pc lnxnfcons3_pc lnxfcons4_pc lnxnfcons4_pc 				
			mi register regular oxfcons0_pc oxnfcons0_pc oxfcons1_pc oxnfcons1_pc oxfcons2_pc oxnfcons2_pc oxfcons3_pc oxnfcons3_pc oxfcons4_pc oxnfcons4_pc
			mi register regular hh* cluster xfcons0_pc xnfcons0_pc pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc 
			mi register regular dxfcons1_pc dxnfcons1_pc dxfcons2_pc dxnfcons2_pc dxfcons3_pc dxnfcons3_pc dxfcons4_pc dxnfcons4_pc
			mi register passive xcons_pc xfcons_pc xfcons1_pc xnfcons1_pc xfcons2_pc xnfcons2_pc xfcons3_pc xnfcons3_pc xfcons4_pc xnfcons4_pc
			
			local nI=3
			local model = "hhsize pchild psenior i.hhsex i.hhwater i.hhcook hhsleep i.hhhouse i.hhtoilet i.hhwaste"
			
			**MI imputation for ly (The prediction(imputation) here is used only for when dy=1, but the numbers are produced not only for dy=1 but also fo dy=0. But replace them later accordingly. 
			mi impute chained (regress if oxfcons1_pc!=0) lnxfcons1_pc (regress if oxnfcons1_pc!=0) lnxnfcons1_pc (regress if oxfcons2_pc!=0) lnxfcons2_pc (regress if oxnfcons2_pc!=0) lnxnfcons2_pc lnxfcons3_pc lnxnfcons3_pc lnxfcons4_pc lnxnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' , add(`nI') report  burnin(10) savetrace(impstats,replace) rseed(1234)
s
			*mi impute chained (regress if dxfcons1_pc>0) lnxfcons1_pc (regress if dxnfcons1_pc>0) lnxnfcons1_pc (regress if dxfcons2_pc>0) lnxfcons2_pc (regress if dxnfcons2_pc>0) lnxnfcons2_pc (regress if dxfcons3_pc>0) lnxfcons3_pc (regress if dxnfcons3_pc>0) lnxnfcons3_pc (regress if dxfcons4_pc>0) lnxfcons4_pc (regress if dxnfcons4_pc>0) lnxnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'  , add(`nI') report burnin(10) savetrace(impstats,replace) rseed(1234) force
			*mi impute chained (regress) lnxfcons1_pc lnxnfcons1_pc ) lnxfcons2_pc (regress if dxnfcons2_pc>0) lnxnfcons2_pc (regress if dxfcons3_pc>0) lnxfcons3_pc (regress if dxnfcons3_pc>0) lnxnfcons3_pc (regress if dxfcons4_pc>0) lnxfcons4_pc (regress if dxnfcons4_pc>0) lnxnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'  , add(`nI') report burnin(10) savetrace(impstats,replace) rseed(1234) force 

			use impstats,clear
			describe
			tsset iter
			tsline lnxfcons1_pc_mean, name(gr1) nodraw
			tsline lnxfcons1_pc_sd, name(gr2) nodraw
			tsline lnxfcons2_pc_mean, name(gr3) nodraw
			tsline lnxfcons2_pc_sd, name(gr4) nodraw
			tsline lnxfcons3_pc_mean, name(gr5) nodraw
			tsline lnxfcons3_pc_sd, name(gr6) nodraw
			tsline lnxfcons3_pc_mean, name(gr7) nodraw
			tsline lnxfcons3_pc_sd, name(gr8) nodraw

			graph combine gr1 gr2 gr3 gr4 gr5 gr6 gr7 gr8, title(Trace plots of summaries of imputed values) cols(2)
			
			
			tsline lnxnfcons1_pc_mean, name(grn1) nodraw
			tsline lnxnfcons1_pc_sd, name(grn2) nodraw
			tsline lnxnfcons2_pc_mean, name(grn3) nodraw
			tsline lnxnfcons2_pc_sd, name(grn4) nodraw
			tsline lnxnfcons3_pc_mean, name(grn5) nodraw
			tsline lnxnfcons3_pc_sd, name(grn6) nodraw
			tsline lnxnfcons3_pc_mean, name(grn7) nodraw
			tsline lnxnfcons3_pc_sd, name(grn8) nodraw

			graph combine grn1 grn2 grn3 grn4 grn5 grn6 grn7 grn8, title(Trace plots of summaries of imputed values) cols(2)
			
			
			use impstats,clear
			keep if m<=3
			reshape wide *mean *sd, i(iter) j(m)
			tsset iter
			tsline lnxfcons1_pc_mean1 lnxfcons1_pc_mean2 lnxfcons1_pc_mean3, ytitle(Mean of bmi) yline(-.3464664) legend(rows(1) label(1 "Chain 1") label(2 "Chain 2") label(3 "Chain 3"))
			lnxfcons1_pc_mean1
			
			cd "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4\Temp"
			use "mi_1.dta", clear
			*prepare variables
			quiet: gen xcons_pc = .
			quiet: gen xfcons_pc = .
			foreach v of varlist xfcons?_pc xnfcons?_pc xdurables_pc {
				*save original
				gen o`v' = `v'
				*remove outliers
				summ `v',d
				replace `v'= `r(p99)' if (`v'>`r(p99)') & (`v'<.)
				*replace `v'= 0.1 if `v'==.
				*replace `v'= 0.1 if `v'<0.1 // This low value truncation is justified since we take log(Y+0.1) in order to avoid too small number becomes lower bound. 
				*log transform
				gen ln`v' = log(`v'+0.1)
			}
			
						*Make dummy for Y>0:
			forvalues imod = 1/4 {
			gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
			replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
			gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
			replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
			clonevar odxfcons`imod'_pc=dxfcons`imod'_pc // keep original for dummy
			clonevar odxnfcons`imod'_pc=dxnfcons`imod'_pc // keep original for dummy
								}
			
			su lnxfcons1_pc,d

			su lnxfcons1_pc if oxfcons1_pc>0,d
			su lnxfcons2_pc if oxfcons2_pc>0,d
			su cfcons1_pc cnfcons1_pc,d
			su xfcons1_pc xnfcons1_pc,d
			su dxfcons1_pc dxnfcons1_pc,d
									
			s
			
						cd "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4\Temp"
			use "mi_1.dta", clear
						su cfcons1_pc cnfcons1_pc,d
						su xfcons1_pc xnfcons1_pc,d
									*Make dummy for Y>0:
			forvalues imod = 1/4 {
			gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
			replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
			gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
			replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
			clonevar odxfcons`imod'_pc=dxfcons`imod'_pc // keep original for dummy
			clonevar odxnfcons`imod'_pc=dxnfcons`imod'_pc // keep original for dummy
								}

			su dxfcons1_pc dxnfcons1_pc,d					
			
			/*
			
			**MI method selection used: Chained regressions
			mi impute chained (logit) dxfcons1_pc dxfcons2_pc dxfcons3_pc dxfcons4_pc  = pxfcons0_pc pxnfcons0_pc pxdurables_pc `model', add(`nI') report noisily augment force
			**Obtain one predicted dy in the following lines:
			
			forvalues imod = 1/4 {
			egen m_dxfcons`imod'_pc=rowmean(_?_dxfcons`imod'_pc)
			replace dxfcons`imod'_pc=(m_dxfcons`imod'_pc>0.5)
			}
		
			mi impute chained (logit) dxnfcons1_pc dxnfcons2_pc dxnfcons3_pc dxnfcons4_pc = pxfcons0_pc pxnfcons0_pc pxdurables_pc `model', add(`nI') report noisily augment force
			mi estimate: mean  dxfcons1_pc dxfcons2_pc dxfcons3_pc dxfcons4_pc dxnfcons1_pc dxnfcons2_pc dxnfcons3_pc dxnfcons4_pc	
			
			**Obtain one predicted dy in the following lines:
			forvalues imod = 1/4 {
			egen m_dxnfcons`imod'_pc=rowmean(_?_dxnfcons`imod'_pc)
			replace dxnfcons`imod'_pc=(m_dxnfcons`imod'_pc>0.5)
			}
	
							}
			
			*2nd step:
			**Make ly missing when y=0 since we do not want to use them in MI impute for ly. 
			forvalues i = 1/4 {
			replace lnxfcons`i'_pc  =. if oxfcons`i'_pc==0
			replace lnxnfcons`i'_pc =. if oxnfcons`i'_pc==0
								}
								
			**set MI
			mi unset
			mi set wide
			mi register imputed lnxfcons1_pc lnxnfcons1_pc lnxfcons2_pc lnxnfcons2_pc lnxfcons3_pc lnxnfcons3_pc lnxfcons4_pc lnxnfcons4_pc 				
			mi register regular oxfcons0_pc oxnfcons0_pc oxfcons1_pc oxnfcons1_pc oxfcons2_pc oxnfcons2_pc oxfcons3_pc oxnfcons3_pc oxfcons4_pc oxnfcons4_pc
			mi register regular hh* cluster xfcons0_pc xnfcons0_pc pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc 
			mi register regular dxfcons1_pc dxnfcons1_pc dxfcons2_pc dxnfcons2_pc dxfcons3_pc dxnfcons3_pc dxfcons4_pc dxnfcons4_pc
			mi register passive xcons_pc xfcons_pc xfcons1_pc xnfcons1_pc xfcons2_pc xnfcons2_pc xfcons3_pc xnfcons3_pc xfcons4_pc xnfcons4_pc

			**MI imputation for ly (The prediction(imputation) here is used only for when dy=1, but the numbers are produced not only for dy=1 but also fo dy=0. But replace them later accordingly. 
			mi impute chained (regress) lnxfcons1_pc lnxnfcons1_pc lnxfcons2_pc lnxnfcons2_pc lnxfcons3_pc lnxnfcons3_pc lnxfcons4_pc lnxnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'  , add(`nI') report 
			
			*/
			
			
			}



			else if ("`smethod'"=="TwoMI2") {
			*method selection
			local mipre = "mi passive: "
			
			*Make dummy for Y>0:
			forvalues imod = 1/4 {
			gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
			replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
			gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
			replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
			clonevar odxfcons`imod'_pc=dxfcons`imod'_pc // keep original for dummy
			clonevar odxnfcons`imod'_pc=dxnfcons`imod'_pc // keep original for dummy
									}

			*1st step (using direct probit prediction, and y=1 if prob>=0.5):
			forvalues imod = 1/4 {
			probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'           [pweight=weight] // part I specification
			predict prf`imod'_pc  if xfcons`imod'_pc==. // predicted prob used for missing data
			replace dxfcons`imod'_pc=1 if (prf`imod'_pc>=0.5 & prf`imod'_pc<=1) & xfcons`imod'_pc==.
			replace dxfcons`imod'_pc=0 if prf`imod'_pc<0.5 & xfcons`imod'_pc==.
			
			
			probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'           [pweight=weight] // part I specification
			predict prnf`imod'_pc  if xnfcons`imod'_pc==. // predicted prob used for missing data
			replace dxnfcons`imod'_pc=1 if (prnf`imod'_pc>=0.5 & prnf`imod'_pc<=1) & xnfcons`imod'_pc==.
			replace dxnfcons`imod'_pc=0 if prnf`imod'_pc<0.5 & xnfcons`imod'_pc==.
									}

								
			*2nd step:
			**Make ly missing when y=0 since we do not want to use them in MI impute for ly. 
			forvalues i = 1/4 {
			replace lnxfcons`i'_pc  =. if oxfcons`i'_pc==0
			replace lnxnfcons`i'_pc =. if oxnfcons`i'_pc==0
								}
								

			**set MI
			mi set wide
			mi register imputed lnxfcons1_pc lnxnfcons1_pc lnxfcons2_pc lnxnfcons2_pc lnxfcons3_pc lnxnfcons3_pc lnxfcons4_pc lnxnfcons4_pc 				
			mi register regular oxfcons0_pc oxnfcons0_pc oxfcons1_pc oxnfcons1_pc oxfcons2_pc oxnfcons2_pc oxfcons3_pc oxnfcons3_pc oxfcons4_pc oxnfcons4_pc
			mi register regular hh* cluster xfcons0_pc xnfcons0_pc pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc 
			mi register regular dxfcons1_pc dxnfcons1_pc dxfcons2_pc dxnfcons2_pc dxfcons3_pc dxnfcons3_pc dxfcons4_pc dxnfcons4_pc
			mi register passive xcons_pc xfcons_pc xfcons1_pc xnfcons1_pc xfcons2_pc xnfcons2_pc xfcons3_pc xnfcons3_pc xfcons4_pc xnfcons4_pc

			**MI imputation for ly (The prediction(imputation) here is used only for when dy=1, but the numbers are produced not only for dy=1 but also fo dy=0. But replace them later accordingly. 
			mi impute chained (regress) lnxfcons1_pc lnxnfcons1_pc lnxfcons2_pc lnxnfcons2_pc lnxfcons3_pc lnxnfcons3_pc lnxfcons4_pc lnxnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'  , add(`nI') report 
			
			}

			
			else if ("`smethod'"=="TwoMI3") {
			*method selection
			local mipre = "mi passive: "
			
			*Make dummy for Y>0:
			forvalues imod = 1/4 {
			gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
			replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
			gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
			replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
			clonevar odxfcons`imod'_pc=dxfcons`imod'_pc // keep original for dummy
			clonevar odxnfcons`imod'_pc=dxnfcons`imod'_pc // keep original for dummy
									}

			*1st step (using direct probit prediction, but using the empirical cut-off prob):
			forvalues imod = 1/`M' {
					*food
					probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'           [pweight=weight] // part I specification
					predict p`imod'_pc  if xfcons`imod'_pc==. // predicted prob used for missing data
					predict p if xfcons`imod'_pc!=. // predicted prob in sample used to get cut-off prob. 
					*Obtain sample probability of Y=0
					su dxfcons`imod'_pc [aweight=weight] 
					local spr=r(mean)
					display "Sample probability=" as text `spr' as result
					gsort +p // Sort predicted probabilities from smaller to larger.
					gen order=_n // Put 1-N
					su p 
					local N=r(N) // Sample size (in sample)
					*This will produce all cases when the number of households with zero will increase one by one.
					quiet: forvalue i=1/`N' {
					gen _`i'=0 if (`i'>=order) 
					replace _`i'=1 if p!=. & _`i'==.
											}
					*This will detect and record when the predicted mean of each case above is close (or the smallest) to the sample mean.  
					quiet: forvalue i=1/`N' {
					su _`i' [aweight=weight]
					local pr_mean=r(mean)
					if (`pr_mean'>=`spr') {
														local thresh=`i'
													   }
					                 }
					display "Chosen cut-off order=" as text `thresh' as result
					su p if order==`thresh'
					local prob=r(mean)
					display "Chosen cut-off probability=" as text `prob' 
	
					replace dxfcons`imod'_pc =  0 if (p`imod'_pc < `prob') & xfcons`imod'_pc==. // Have zero when predicted probabilities are smaller than cut-off prob identified above. 
					replace dxfcons`imod'_pc =  1 if (p`imod'_pc >= `prob' & !missing(p`imod'_pc)) & xfcons`imod'_pc==.
					drop p`imod'_pc order p _*

					*Non-food
					probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'      [pweight=weight] // part I specification
					predict p`imod'_pc if xnfcons`imod'_pc==. // predicted prob used for missing data
					predict p if xnfcons`imod'_pc!=. // predicted prob in sample used to get cut-off prob. 
					*Obtain sample probability of Y=0
					su dxnfcons`imod'_pc [aweight=weight] 
					local spr=r(mean)
					display "Sample probability=" as text `spr' as result
					gsort +p // Sort predicted probabilities from smaller to larger.
					gen order=_n // Put 1-N
					su p 
					local N=r(N) // Sample size (in sample)
					*This will produce all cases when households with one (Y>0) will increase one by one from the smallest to the largest predicted probabilities. 
					quiet: forvalue i=1/`N' {
					gen _`i'=0 if (`i'>=order) 
					replace _`i'=1 if p!=. & _`i'==.
											}
					*This will produce all cases when the number of households with zero will increase one by one.
					quiet: forvalue i=1/`N' {
					su _`i' [aweight=weight]
					local pr_mean=r(mean)
					if (`pr_mean'>=`spr') {
														local thresh=`i'
													   }
					                 }
					display "Chosen cut-off order=" as text `thresh' as result
					su p if order==`thresh'
					local prob=r(mean)
					display "Chosen cut-off probability=" as text `prob' 

					replace dxnfcons`imod'_pc =  0 if (p`imod'_pc < `prob') & xnfcons`imod'_pc==. // Have zero when predicted probabilities are smaller than cut-off prob identified above. 
					replace dxnfcons`imod'_pc =  1 if (p`imod'_pc >= `prob' & !missing(p`imod'_pc)) & xfcons`imod'_pc==.
					drop p`imod'_pc order p _*

									}
		
								
			*2nd step:
			**Make ly missing when y=0 since we do not want to use them in MI impute for ly. 
			forvalues i = 1/4 {
			replace lnxfcons`i'_pc  =. if oxfcons`i'_pc==0
			replace lnxnfcons`i'_pc =. if oxnfcons`i'_pc==0
								}
								

			**set MI
			mi set wide
			mi register imputed lnxfcons1_pc lnxnfcons1_pc lnxfcons2_pc lnxnfcons2_pc lnxfcons3_pc lnxnfcons3_pc lnxfcons4_pc lnxnfcons4_pc 				
			mi register regular oxfcons0_pc oxnfcons0_pc oxfcons1_pc oxnfcons1_pc oxfcons2_pc oxnfcons2_pc oxfcons3_pc oxnfcons3_pc oxfcons4_pc oxnfcons4_pc
			mi register regular hh* cluster xfcons0_pc xnfcons0_pc pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc 
			mi register regular dxfcons1_pc dxnfcons1_pc dxfcons2_pc dxnfcons2_pc dxfcons3_pc dxnfcons3_pc dxfcons4_pc dxnfcons4_pc
			mi register passive xcons_pc xfcons_pc xfcons1_pc xnfcons1_pc xfcons2_pc xnfcons2_pc xfcons3_pc xnfcons3_pc xfcons4_pc xnfcons4_pc

			**MI imputation for ly (The prediction(imputation) here is used only for when dy=1, but the numbers are produced not only for dy=1 but also fo dy=0. But replace them later accordingly. 
			mi impute chained (regress) lnxfcons1_pc lnxnfcons1_pc lnxfcons2_pc lnxnfcons2_pc lnxfcons3_pc lnxnfcons3_pc lnxfcons4_pc lnxnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'  , add(`nI') report 
			
			}
			
					
			
			
			*Anti-log transformation: rounding is to avoid extremely small number close to zero after transformation. 
			if ("`smethod'"=="avg" | "`smethod'"=="med" | "`smethod'"=="MICE" | "`smethod'"=="MImvn" ) {
			quiet: forvalue i=1/`M'{
					`mipre' replace xfcons`i'_pc=round(exp(lnxfcons`i'_pc), 0.0001)
			        `mipre' replace xnfcons`i'_pc=round(exp(lnxnfcons`i'_pc), 0.0001)
					}
			}
			else if ("`smethod'"=="reg" | "`smethod'"=="reg2"  | "`smethod'"=="reg3") {
*			quiet: forvalue i=1/`M'{
			forvalue i=1/`M'{
			dis "check pt"
				replace xfcons`i'_pc= round((exp(lnxfcons`i'_pc ))*`smear_fd`i'', 0.0001) if xfcons`i'_pc>=.
				replace xfcons`i'_pc=oxfcons`i'_pc if oxfcons`i'_pc!=.	// For zero or positive numbers, use the original val.				
				
		        replace xnfcons`i'_pc=round((exp(lnxnfcons`i'_pc))*`smear_nf`i'', 0.0001) if xnfcons`i'_pc>=.
				replace xnfcons`i'_pc=oxnfcons`i'_pc if oxnfcons`i'_pc!=. 	// For zero or positive numbers, use the original val.					
								}
																}
																
			else if ("`smethod'"=="tobit" | "`smethod'"=="tobit2" | "`smethod'"=="tobit3") {
*			quiet: forvalue i=1/`M'{
			forvalue i=1/`M'{
			dis "check pt"
				replace xfcons`i'_pc=oxfcons`i'_pc if oxfcons`i'_pc!=.	// Make sure that non-imputed values come from originals.				
				replace xnfcons`i'_pc=oxnfcons`i'_pc if oxnfcons`i'_pc!=. 	// Make sure that non-imputed values come from originals.					
								}
																}

												
			else if ("`smethod'"=="twopart" | "`smethod'"=="twopart2" | "`smethod'"=="twopart3") {
*			quiet: forvalue i=1/`M'{
			forvalue i=1/`M'{
			dis "check pt"
				replace xfcons`i'_pc=oxfcons`i'_pc if oxfcons`i'_pc!=.	// Make sure that non-imputed values come from originals.		
				replace xnfcons`i'_pc=oxnfcons`i'_pc if oxnfcons`i'_pc!=. 	// Make sure that non-imputed values come from originals.			
								}
																}


			else if ("`smethod'"=="twopartII1" | "`smethod'"=="twopartII2" | "`smethod'"=="twopartII3") {
*			quiet: forvalue i=1/`M'{
			forvalue i=1/`M'{
			dis "check pt"
				replace xfcons`i'_pc=oxfcons`i'_pc if oxfcons`i'_pc!=.	// Make sure that non-imputed values come from originals.		
				replace xnfcons`i'_pc=oxnfcons`i'_pc if oxnfcons`i'_pc!=. 	// Make sure that non-imputed values come from originals.			
								}																
																}
																
			else if ("`smethod'"=="TwoMI" | "`smethod'"=="TwoMI2" | "`smethod'"=="TwoMI3") {
				*Anti-log transformation: rounding is to avoid extremely small number close to zero after transformation. The 2nd step imputaion is used only when predicted y ==1. 
				forvalue i=1/4{
					mi passive: replace xfcons`i'_pc=round(exp(lnxfcons`i'_pc), 0.0001) if dxfcons`i'_pc==1
					mi passive: replace xnfcons`i'_pc=round(exp(lnxnfcons`i'_pc), 0.0001) if dxnfcons`i'_pc==1
					mi passive: replace xfcons`i'_pc= 0  if dxfcons`i'_pc==0
					mi passive: replace xnfcons`i'_pc=0  if dxnfcons`i'_pc==0
					  		}
											}
	
			
			*Add imputed food consumption		
			quiet: `mipre' replace xfcons_pc = 0
			quiet: foreach v of varlist xfcons?_pc {
				`mipre' replace xfcons_pc = xfcons_pc + o`v' if o`v'<.
				`mipre' replace xfcons_pc = xfcons_pc + `v'  if o`v'>=.
			}
	
			*Add durable goods
			quiet: `mipre' replace xcons_pc = xfcons_pc + xdurables_pc
	
			*Add imputed non-food items
			quiet: foreach v of varlist xnfcons?_pc {
				`mipre' replace xcons_pc = xcons_pc + o`v' if o`v'<.
				`mipre' replace xcons_pc = xcons_pc + `v'  if o`v'>=.
			}
			
			*estimate total consumption
			*quiet: mi passive: replace xfcons_pc = xfcons0_pc + xfcons1_pc + xfcons2_pc + xfcons3_pc + xfcons4_pc
			*quiet: mi passive: replace xcons_pc = xfcons_pc + xnfcons0_pc + xnfcons1_pc + xnfcons2_pc + xnfcons3_pc + xnfcons4_pc + 
			save "`lc_sdTemp'/sim_`smethod'_`isim'.dta", replace
		}
	}
	
	
end

capture: program drop RCS_collate
program define RCS_collate
	syntax using/, dirbase(string) nsim(integer) nmi(integer) lmethod(namelist) povline(varname) deflator(varname)
	*prepare output directories
	local lc_sdTemp = "`dirbase'/Temp"
	local lc_sdOut = "`dirbase'/Out"
	local N = `nsim'
	local nI = `nmi'

	*extract detailed results from estimation
	foreach smethod of local lmethod {
		di "Extracting for `smethod':"
		capture: file close fh
		file open fh using "`lc_sdTemp'/simd_`smethod'.txt", replace write
		*prepare title
		use "`lc_sdTemp'/sim_`smethod'_1.dta", clear
		file write fh "Simulation" _tab "Imputation"
		quiet: levelsof hhid, local(xhh)
		foreach id of local xhh {
			file write fh _tab "hh`id'"
		}
		file write fh _n
		*extract consumption
		forvalues isim = 1/`N' {
			di "`isim'"
			use "`lc_sdTemp'/sim_`smethod'_`isim'.dta", clear
			*real and reduced values
			if (`isim'==1) {
				*real values
				file write fh "0" _tab "0"
				foreach id of local xhh {
					quiet: summ ccons_pc if hhid==`id'
					file write fh _tab (r(mean))
				}
				file write fh _n
				*reduced consumption
				file write fh "-1" _tab "0"
				foreach id of local xhh {
					quiet: summ rcons_pc if hhid==`id'
					file write fh _tab (r(mean))
				}
				file write fh _n
			}
			*estimations
			if inlist("`smethod'","med","avg","reg","reg2","reg3","tobit","tobit2","tobit3") {
				file write fh "`isim'" _tab "1"
				foreach id of local xhh {
					quiet: summ xcons_pc if hhid==`id'
					file write fh _tab (r(mean))
				}
				file write fh _n
			}
						*estimations
			if inlist("`smethod'","twopart","twopart2","twopart3","twopartII1","twopartII2","twopartII3") {
				file write fh "`isim'" _tab "1"
				foreach id of local xhh {
					quiet: summ xcons_pc if hhid==`id'
					file write fh _tab (r(mean))
				}
				file write fh _n
			}

			else if inlist("`smethod'","MImvn", "MICE", "TwoMI", "TwoMI2", "TwoMI3") {
				*extract imputations
				forvalues iter=1/`nI' {
					use "`lc_sdTemp'/sim_`smethod'_`isim'.dta", clear
					mi extract `iter', clear
					file write fh "`isim'" _tab "`iter'"
					foreach id of local xhh {
						quiet: summ xcons_pc if hhid==`id'
						file write fh _tab (r(mean))
					}
					file write fh _n
				}
			}
		}	
		file close fh
		*prepare stata file
		insheet using "`lc_sdTemp'/simd_`smethod'.txt", clear
		*summarize imputations for MI
		quiet: reshape long hh, i(simulation imputation) j(hhid)
		ren hh est
		quiet: merge m:1 hhid using "`using'", nogen keep(match) keepusing(weight cluster `povline' `deflator')
		*get reference
		quiet: gen x = est if simulation==0
		quiet: bysort hhid: egen ref = max(x)
		drop x
		quiet: drop if simulation==0
		order ref, after(est)
		*get reduced aggregate
		quiet: gen x = est if simulation==-1
		quiet: bysort hhid: egen red = max(x)
		drop x
		quiet: drop if simulation==-1
		order red, after(ref)
		*deflate consumption variables:
		foreach var of var ref red est {
		gen n_`var'=`var'
		label var n_`var' "Nominal `var'"
		replace `var'=`var'/`deflator'
		label var `var' "Deflated `var'"
		}
		*remove outliers only for prediction
*		quiet: summ ref,d
*		quiet: replace ref = `r(p99)' if ref>`r(p99)'
*		quiet: summ red,d
*		quiet: replace red = `r(p99)' if red>`r(p99)'
		quiet: summ est,d
		quiet: replace est = `r(p99)' if est>`r(p99)'
		*save for analysis
		save "`lc_sdTemp'/simd_`smethod'_imp.dta", replace
		*collapse imputations
		bysort simulation hhid: egen x = mean(est)
		quiet: replace est = x
		drop x
		quiet: drop if imputation>1
		drop imputation
		save "`lc_sdTemp'/simd_`smethod'.dta", replace
	}
end
	
capture: program drop RCS_analyze
program define RCS_analyze
	syntax using/, dirbase(string) lmethod(namelist)  povline(varname) deflator(varname)
	*prepare output directories
	local lc_sdTemp = "`dirbase'/Temp"
	local lc_sdOut = "`dirbase'/Out"
	
	local lind = "fgt0 fgt1 fgt2 gini"
	*analyze relative bias and relative standard error
	capture: file close fh
	file open fh using "`lc_sdOut'/simc.txt", replace write
	file write fh "Method" _tab "Bias (HH)" _tab "SE (HH)" _tab "Bias (CL)" _tab "SE (CL)"_tab "Bias (SIM)" _tab "SE (SIM)"_n
	*add reduced
	use "`lc_sdTemp'/simd_`: word 1 of `lmethod''.dta", clear
	file write fh "red"
	*calculate relative difference at hh-level
	quiet: gen rd = (red - ref) / ref * 100
	quiet: fse "ref" "red" 1
	file write fh _tab (r(bias)) _tab (r(se))
	*per EA
	collapse (mean) red ref [aweight=weight], by(simulation cluster)
	quiet: gen rd = (red - ref) / ref * 100
	quiet: fse "ref" "red" 1
	file write fh _tab (r(bias)) _tab (r(se))
	*simulation
	use "`lc_sdTemp'/simd_`: word 1 of `lmethod''.dta", clear
	collapse (mean) red ref [aweight=weight], by(simulation)
	quiet: gen rd = (red - ref) / ref * 100
	quiet: fse "ref" "red" 1
	file write fh _tab (r(bias)) _tab (r(se)) _n
	*add other methods
	foreach smethod of local lmethod {
		use "`lc_sdTemp'/simd_`smethod'.dta", clear
		file write fh "`smethod'"
		*calculate relative difference at hh-level
		quiet: gen rd = (est - ref) / ref * 100
		quiet: fse "ref" "est" 1
		file write fh _tab (r(bias)) _tab (r(se))
		quiet: hist rd if inrange(rd,-100,100) , normal dens xline(0) name("hh_`smethod'",replace) xtitle("Relative Difference, in %") title("Household Estimation Error (`smethod')") note("Bias: `r(bias)'; Standard Error: `r(se)'") graphregion(color(white)) bgcolor(white)
		graph export "`lc_sdTemp'/hh_rdiff_`smethod'.png", replace
		local gl_hh = "`gl_hh' hh_`smethod'"
		*per EA
		collapse (mean) est ref [aweight=weight], by(simulation cluster)
		quiet: gen rd = (est - ref) / ref * 100
		quiet: fse "ref" "est" 1
		file write fh _tab (r(bias)) _tab (r(se))
		quiet: hist rd if inrange(rd,-100,100) , normal dens xline(0) name("cl_`smethod'",replace) xtitle("Relative Difference, in %") title("Cluster Estimation Error (`smethod')") note("Bias: `r(bias)'; Standard Error: `r(se)'")graphregion(color(white)) bgcolor(white)
		graph export "`lc_sdTemp'/ea_rdiff_`smethod'.png", replace
		local gl_cl = "`gl_cl' cl_`smethod'"
		*simulation
		use "`lc_sdTemp'/simd_`smethod'.dta", clear
		collapse (mean) est ref [aweight=weight], by(simulation)
		quiet: gen rd = (est - ref) / ref * 100
		quiet: fse "ref" "est" 1
		file write fh _tab (r(bias)) _tab (r(se)) _n
		quiet: hist rd if inrange(rd,-100,100) , normal dens xline(0) name("sim_`smethod'",replace) xtitle("Relative Difference, in %") title("Simulation Estimation Error (`smethod')") note("Bias: `r(bias)'; Standard Error: `r(se)'")graphregion(color(white)) bgcolor(white)
		graph export "`lc_sdTemp'/sim_rdiff_`smethod'.png", replace
		local gl_sim = "`gl_sim' sim_`smethod'"
	}
	file close fh
	*combine graphs
	local ls = "hh cl sim"
	foreach s of local ls {
		graph combine `gl_`s'', name("cmb_`s'", replace) xcommon
		graph export "`lc_sdOut'/rdiff_`s'.png", replace
		graph drop `gl_`s'' cmb_`s'
	}
	
	*analyze consumption distribution with poverty rate and inequality measures
	capture: file close fh
	file open fh using "`lc_sdOut'/simp.txt", replace write
	file write fh "Method" _tab "FGT0 (Bias)" _tab "FGT0 (SE)" _tab "FGT1 (Bias)" _tab "FGT1 (SE)" _tab "FGT2 (Bias)" _tab "FGT2 (SE)" _tab "Gini (Bias)"  _tab "Gini (SE)" _n
	*add reduced aggregated
	file write fh "red"
	*prepare dataset
	use "`lc_sdTemp'/simd_`: word 1 of `lmethod''_imp.dta", clear
	replace hhid = hhid * 100 + imputation
	drop imputation
	*calculate poverty indices and gini
	foreach v of var red ref {
		sort simulation `v'
		by simulation: egen i = seq()
		by simulation: egen n = max(i)
		gen `v'_gini = (n + 1 - i) * `v'
		gen `v'_fgt0 = `v' < `povline' if `v'<.
		gen `v'_fgt1 = max(`povline' - `v',0) / `povline'
		gen `v'_fgt2 = `v'_fgt1^2
		drop i n
	}
	collapse (mean) red* ref* (count) n=hhid [aweight=weight], by(simulation)
	replace ref_gini = (n+1-2*ref_gini / ref) / n
	replace red_gini = (n+1-2*red_gini / red) / n
	drop n
	quiet: foreach sind of local lind {
		quiet: summ ref_`sind'
		local ref_mean = r(mean)
		fse "ref_`sind'" "red_`sind'" 0
		file write fh _tab (r(bias)) _tab (r(se))
	}
	file write fh _n
	*add other methods
	quiet: foreach smethod of local lmethod {
		file write fh "`smethod'"
		*prepare dataset
		use "`lc_sdTemp'/simd_`smethod'_imp.dta", clear
		replace hhid = hhid * 100 + imputation
		drop imputation
		*calculate poverty indices and gini
		foreach v of var est ref {
			sort simulation `v'
			by simulation: egen i = seq()
			by simulation: egen n = max(i)
			gen `v'_gini = (n + 1 - i) * `v'
			gen `v'_fgt0 = `v' < `povline' if `v'<.
			gen `v'_fgt1 = max(`povline' - `v',0) / `povline'
			gen `v'_fgt2 = `v'_fgt1^2
			drop i n
		}
		collapse (mean) est* ref* (count) n=hhid [aweight=weight], by(simulation)
		replace ref_gini = (n+1-2*ref_gini / ref) / n
		replace est_gini = (n+1-2*est_gini / est) / n
		drop n
		*make histograms
		quiet: foreach sind of local lind {
			quiet: summ ref_`sind'
			local ref_mean = r(mean)
			fse "ref_`sind'" "est_`sind'" 0
			file write fh _tab (r(bias)) _tab (r(se))
			hist est_`sind' , normal dens xline(`ref_mean') start(0) width(0.01) xscale(range(0 1)) xlabel(0[.25]1) xtitle("`sind'") name("`sind'_`smethod'", replace) title("`sind' (`smethod')") note("Bias: `r(bias)'; Standard Error: `r(se)'") graphregion(color(white)) bgcolor(white)
			graph export "`lc_sdTemp'/sim_`sind'_`smethod'.png", replace
			local gl_`sind' = "`gl_`sind'' `sind'_`smethod'"
		}
		file write fh _n
	}
	file close fh
	*combine graphs
	foreach sind of local lind {
		graph combine `gl_`sind'', name("cmb_`sind'", replace)
		graph export "`lc_sdOut'/`sind'.png", replace
		graph drop `gl_`sind'' cmb_`sind'
	}
end



/*
	
capture: program drop RCS_run
program define RCS_run
	syntax using/, dirout(string) nmodules(integer) ncoref(integer) ncorenf(integer) ndiff(integer) nsim(integer) nmi(integer) lmethod(namelist) povline(real) model(string) [EGALshare] rseed(integer)

	local dirbase = "`dirout'/d`ndiff'm`nmodules'"
	
	RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') `EGALshare'
	RCS_assign using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed')
	RCS_simulate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model(`model') rseed(`rseed')
	RCS_collate using "`using'", dirbase("`dirbase'") nsim(`nsim') nmi(`nmi') lmethod("`lmethod'")
	RCS_analyze using "`using'", dirbase("`dirbase'") lmethod("`lmethod'") povline(`povline')
end

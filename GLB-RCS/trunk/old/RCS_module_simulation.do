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


capture: program drop RCS_simulate_msim
program define RCS_simulate_msim
	syntax using/, dirbase(string) nmodules(integer) nsim(integer) lmethod(namelist) model(string) rseed(integer)
	*prepare output directories
	local lc_sdTemp = "`dirbase'/Temp"
	local lc_sdOut = "`dirbase'/Out"
	local N = `nsim'
	local M = `nmodules'

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
			
			*estimation - twopartII
	
				forvalues imod = 1/`M' {
					*food
					gen     dxfcons`imod'_pc=1 if xfcons`imod'_pc>0 & xfcons`imod'_pc!=.
					replace dxfcons`imod'_pc=0 if xfcons`imod'_p==0 & xfcons`imod'_pc!=.
					*probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'           [pweight=weight] // part I specification
					probit  dxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc           [pweight=weight] // part I specification
					predict p`imod'_pc
					replace     dxfcons`imod'_pc=1 if p`imod'_pc>=0.5 & xfcons`imod'_pc==.
					replace     dxfcons`imod'_pc=0 if p`imod'_pc< 0.5 & xfcons`imod'_pc==.
					
					regress lnxfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' [aweight=weight] if dxfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxfcons`imod'_pc = lny`imod'_pc if xfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_fd`imod'=r(mean)
					dis as text "smear_fd`imod'=" as result "`smear_fd`imod''"						
					replace xfcons`imod'_pc= round((exp(lnxfcons`imod'_pc ))*`smear_fd`imod'', 0.0001) if xfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc

					*non-food
					gen     dxnfcons`imod'_pc=1 if xnfcons`imod'_pc>0 & xnfcons`imod'_pc!=.
					replace dxnfcons`imod'_pc=0 if xnfcons`imod'_p==0 & xnfcons`imod'_pc!=.
					*probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'           [pweight=weight] // part I specification
					probit  dxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc           [pweight=weight] // part I specification
					predict p`imod'_pc
					replace     dxnfcons`imod'_pc=1 if p`imod'_pc>=0.5 & xnfcons`imod'_pc==.
					replace     dxnfcons`imod'_pc=0 if p`imod'_pc< 0.5 & xnfcons`imod'_pc==.

					regress lnxnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' [pweight=weight] if dxnfcons`imod'_pc==1 // part II specification
					predict lny`imod'_pc if xnfcons`imod'_pc>=.  // Prediction with logged dep var
					predict double resid`imod' if e(sample) , residuals  // Prediction with logged dep var
					replace lnxnfcons`imod'_pc = lny`imod'_pc if xnfcons`imod'_pc==.
					gen double exp_res_`imod'=exp(resid`imod')
					sum exp_res_`imod' [aweight=weight]
					local smear_nf`imod'=r(mean)
					dis as text "smear_nf`imod'=" as result "`smear_nf`imod''"
					replace xnfcons`imod'_pc=round((exp(lnxnfcons`imod'_pc))*`smear_nf`imod'', 0.0001) if xnfcons`imod'_pc>=.
					drop lny`imod'_pc resid`imod' exp_res_`imod' p`imod'_pc
				}			
			
			forvalue i=1/`M'{
			dis "check pt"
				replace xfcons`i'_pc=oxfcons`i'_pc if oxfcons`i'_pc!=.	// Make sure that non-imputed values come from originals.		
				replace xnfcons`i'_pc=oxnfcons`i'_pc if oxnfcons`i'_pc!=. 	// Make sure that non-imputed values come from originals.			
				replace xfcons`i'_pc=0 if dxfcons`i'_pc==0 & oxfcons`i'_pc==.	// Use zero when d*==0		
				replace xnfcons`i'_pc=0 if dxnfcons`i'_pc==0 & oxnfcons`i'_pc==. 	// Use zero when d*==0			
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



capture: program drop RCS_collate_msim
program define RCS_collate_msim
	syntax using/, dirbase(string) nsim(integer) lmethod(namelist) povline(string) deflator(string)
	*prepare output directories
	local lc_sdTemp = "`dirbase'/Temp"
	local lc_sdOut = "`dirbase'/Out"
	local N = `nsim'

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
				file write fh "`isim'" _tab "1"
				foreach id of local xhh {
					quiet: summ xcons_pc if hhid==`id'
					file write fh _tab (r(mean))
				}
				file write fh _n
	
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

capture: program drop RCS_analyze_msim
program define RCS_analyze_msim
	syntax using/, dirbase(string) lmethod(namelist)  povline(varname) deflator(varname)
	*prepare output directories
	local lc_sdTemp = "`dirbase'/Temp"
	local lc_sdOut = "`dirbase'/Out"
	
	local lind = "fgt0 fgt1 fgt2 gini"
	*analyze relative bias and relative standard error
	capture: file close fh
	file open fh using "`lc_sdOut'/simc.txt", replace write
	file write fh "Method" _tab "Bias (HH)" _tab "SE (HH)" _tab "Bias (CL)" _tab "SE (CL)"_tab "Bias (SIM)" _tab "SE (SIM)"_n
	
	*add other methods
	foreach smethod of local lmethod {
		use "`lc_sdTemp'/simd_`smethod'.dta", clear

		*add reduced
	*use "`lc_sdTemp'/simd_`: word 1 of `lmethod''.dta", clear
	file write fh "red_`smethod'"
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
	use "`lc_sdTemp'/simd_`smethod'.dta", clear
	*use "`lc_sdTemp'/simd_`: word 1 of `lmethod''.dta", clear
	collapse (mean) red ref [aweight=weight], by(simulation)
	quiet: gen rd = (red - ref) / ref * 100
	quiet: fse "ref" "red" 1
	file write fh _tab (r(bias)) _tab (r(se)) _n
	
		file write fh "`smethod'"
		use "`lc_sdTemp'/simd_`smethod'.dta", clear
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

	*add other methods
	quiet: foreach smethod of local lmethod {

		*add reduced aggregated
	file write fh "red_`smethod'"
	*prepare dataset
	*use "`lc_sdTemp'/simd_`: word 1 of `lmethod''_imp.dta", clear
	use "`lc_sdTemp'/simd_`smethod'_imp.dta", clear
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


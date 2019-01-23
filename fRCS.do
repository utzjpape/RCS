*functions for RCS
* RCS_run: runs the whole suite of programs
*
* RCS_partition: sub-program called in RCS_prepare to partition items into modules
*
* RCS_prepare: must be run first; prepares the dataset; runs RCS_partition
* RCS_masks: masks consumption of households
* RCS_estimate: estimates consumption for each household in each consumption
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
* parameters:
*   hhid: unique identifier for households
*   itemid: unique identifier for items
*   value: variable capturing the value of consumption
*   [REDuced]: number of items to include in the final dataset (scaled to approx sum up to total consumption)
capture: program drop fItems2RCS
program define fItems2RCS
	syntax , hhid(varname) itemid(varname) value(varname) [REDuced(integer 0)]
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
	*reduce dataset if needed
	quiet: if (`reduced'>0) {
		*work in long dataset (but need zero values)
		reshape long `value', i(`hhid') j(`itemid')
		bysort `hhid': egen xt = total(`value')
		gen pt = `value' / xt
		bysort `itemid': egen ppt = mean(pt)
		egen r = rank(ppt)
		replace r= -r
		egen rr = group(r)
		*calculate scaling factor (is done in constant multiples of households)
		egen scale = total(ppt) if rr > `reduced'
		egen xscale = total(ppt)
		gen x = scale/xscale
		egen xfactor = mean(x)
		drop if rr > `reduced'
		replace `value' = `value' / xfactor
		quiet: summ xfactor
		local xf = 1-r(mean)
		drop xt pt ppt r rr scale x xscale xfactor
		reshape wide `value', i(`hhid') j(`itemid')
	}
	if (`reduced'>0) di "Reduced consumption items to `reduced' item, capturing `xf' of consumption."
	*reinstantiate labels
	foreach val of local `itemid'_levels {
		capture: label var `value'`val' "``itemid'_`val''"
	}
end

*RCS_describe prepares some stats from the initial dataset
* parameter:
*   using: input file for RCS
*   dirbase: directory for output
capture: program drop RCS_describe
program define RCS_describe
	syntax using/, dirbase(string)
	*prepare output directories
	capture: mkdir "`dirbase'"
	local lc_sdTemp = "`dirbase'/Temp"
	capture: mkdir "`lc_sdTemp'"
	local lc_sdOut = "`dirbase'/Out"
	capture: mkdir "`lc_sdOut'"

	use "`using'", clear
	*histogram for administered items
	egen xc = anycount(xfood*), values(0)
	egen n_food = rownonmiss(xfood*)
	gen x_food = n_food - xc
	label var n_food "Number of food items"
	label var x_food "Consumed food items"
	hist x_food, freq ylabel(,angle(0)) graphregion(color(white)) bgcolor(white) bcolor(eltblue) name(gRCS_xfood, replace)
	egen t_food = rowtotal(xfood*)
	label var t_food "Food consumption"
	quiet: summ t_food, d
	scatter t_food x_food if t_food<`r(p95)' || lfit t_food x_food if t_food<`r(p95)', ylabel(,angle(0)) legend(size(small)) graphregion(color(white)) bgcolor(white) bcolor(eltblue) name(gRCS_tfood, replace)
	drop xc t_food
	*non food
	egen xc = anycount(xnonfood*), values(0)
	egen n_nonfood = rownonmiss(xnonfood*)
	gen x_nonfood = n_nonfood - xc
	label var n_nonfood "Number of non-food items"
	label var x_nonfood "Consumed non-food items"
	hist x_nonfood, freq ylabel(,angle(0)) graphregion(color(white)) bgcolor(white) bcolor(eltblue) name(gRCS_xnonfood, replace)
	egen t_nonfood = rowtotal(xnonfood*)
	label var t_nonfood "Non-food consumption"
	quiet: summ t_nonfood, d
	scatter t_nonfood x_nonfood if t_nonfood<`r(p95)' || lfit t_nonfood x_nonfood if t_nonfood<`r(p95)', ylabel(,angle(0)) legend(size(small)) graphregion(color(white)) bgcolor(white) bcolor(eltblue) name(gRCS_tnonfood, replace)
	summ x_food n_food x_nonfood n_nonfood
	drop xc n_food n_nonfood x_food x_nonfood t_nonfood
	*produce combined graph
	graph combine gRCS_xfood gRCS_xnonfood gRCS_tfood gRCS_tnonfood, graphregion(color(white)) name(gRCS_xcmb, replace)
	graph export "`lc_sdOut'/descr_nitems.png", replace
	graph drop gRCS_xfood gRCS_tfood gRCS_xnonfood gRCS_tnonfood gRCS_xcmb
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
	*obtain share of household consumption (either democratic or plutocratic)
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
	capture: mkdir "`lc_sdTemp'"
	local lc_sdOut = "`dirbase'/Out"
	capture: mkdir "`lc_sdOut'"
	
	use "`using'", clear
	*make hhid sequential
	sort hhid
	egen hhidx = seq()
	drop hhid
	ren hhidx hhid
	order hhid, first
	save "`using'", replace
	*create food and non-food files
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
	export excel using "`lc_sdOut'/FoodConsumption.xls", replace first(var) sheet("Items")
	keep foodid itemmod itemred
	save "`lc_sdTemp'/fsim_fpartition.dta", replace
	*non-food partition
	use "`lc_sdTemp'/HH-NonFood.dta", clear
	quiet: RCS_partition xnonfood, hhid("hhid") itemid("nonfoodid") fweight("weight") hhsize("hhsize") nmodules(`nmodules') ncore(`ncorenf') ndiff(`ndiff') `egalshare'
	gen itemcode = nonfoodid
	order itemcode, before(nonfoodid)
	*save assignment
	export excel using "`lc_sdOut'/NonFoodConsumption.xls", replace first(var) sheet("Items")
	keep nonfoodid itemmod itemred
	save "`lc_sdTemp'/fsim_nfpartition.dta", replace
end

* descriptive statistics for the model, based on the full dataset
* make sure to run RCS_prepare first
capture: program drop RCS_describe_model
program define RCS_describe_model
	syntax using/, dirbase(string) nmodules(integer) model(string)
	*prepare output directories
	capture: mkdir "`dirbase'"
	local lc_sdTemp = "`dirbase'/Temp"
	capture: mkdir "`lc_sdTemp'"
	local lc_sdOut = "`dirbase'/Out"
	capture: mkdir "`lc_sdOut'"
	
	*summarize at module level
	use "`using'", clear
	keep hhid xnonfood* xfood*
	ren xnonfood* xfoodX*
	reshape long xfood, i(hhid) j(itemid) string
	*get separate IDs
	destring itemid, gen(foodid) force
	destring itemid, gen(nonfoodid) ignore("X")
	replace nonfoodid = . if !missing(foodid)
	gen isfood = !missing(foodid)
	*merge partitioning and collapse
	merge m:1 foodid using "`lc_sdTemp'/fsim_fpartition.dta", nogen keep(master match)
	merge m:1 nonfoodid using "`lc_sdTemp'/fsim_nfpartition.dta", nogen keep(master match match_update) update
	collapse xfood, by(hhid itemmod isfood)
	egen x = concat(isfood itemmod)
	drop isfood itemmod
	reshape wide xfood, i(hhid) j(x) string
	ren (xfood0* xfood1*) (xnfcons* xfcons*)
	save "`lc_sdTemp'/modcons.dta", replace
	*test model
	use "`using'", clear
	merge 1:1 hhid using "`lc_sdTemp'/modcons.dta", nogen keep(match) assert(match)
	*get per capita variables
	foreach v of varlist xfcons* xnfcons* {
		quiet: gen `v'_pc = `v'/hhsize
	} 
	*create percentiles
	foreach v of varlist xfcons0_pc xnfcons0_pc xdurables_pc {
		xtile p`v' = `v' [pweight=weight], nquantiles(4)
	}
	*remove outliers
	foreach v of varlist xfcons?_pc xnfcons?_pc xdurables_pc {
		*remove outliers
		quiet: summ `v',d
		quiet: replace `v'= `r(p99)' if (`v'>`r(p99)') & (`v'<.)
	}
	*run module analytics
	forvalues imod = 1/`nmodules' {
		*food
		reg xfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight]
		*non-food
		reg xnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight]
	}
end

* RCS_mask: creates one output file per simulation with masked consumption
* parameters:
*   using: prepared dataset for analysis
*   dirbase: folder root to save files in Temp and Out folders
*   nmodules: number of modules to split items into
*   nsim: number of simulations to run
*   rseed: random seed for reproducibility
*   Prob: 0-1: probability for an item to be administered in detail (instead of just y/n)
*         >1: maximum number of items to be administered in detail (instead of just y/n)
capture: program drop RCS_mask
program define RCS_mask
	syntax using/, dirbase(string) nmodules(integer) nsim(integer) rseed(integer) [Prob(real 1.0)]
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
		*start module assignment randomly per cluster to make sure they are uniformly across clusters
		gen r = runiform()
		sort cluster r
		*get random start value for sequence for each cluster
		by cluster: egen i = seq()
		quiet: gen module = 1+int((`M')*runiform()) if i==1
		quiet: bysort cluster: replace module = mod(module[_n-1],`M')+1 if i>1
		label var module "Optional food module assigned to household"
		*get non-food module (same as food module by definition)
		quiet: gen module_nf = module
		keep hhid module module_nf
		ren module hhmod_f 
		ren module_nf hhmod_nf
		label var hhmod_nf "Optional non-food module assigned to household"
		save "`lc_sdTemp'/HH-ModuleAssignment.dta", replace

		*prepare dataset for MI 
		*FOOD CONSUMPTION
		use "`lc_sdTemp'/HH-Food.dta", clear
		*get household assigned module
		quiet: merge m:1 hhid using "`lc_sdTemp'/HH-ModuleAssignment.dta", nogen assert(match) keepusing(hhmod_f)
		ren hhmod_f hhmod
		*add food assigned  module
		quiet: merge m:1 foodid using "`lc_sdTemp'/fsim_fpartition.dta", nogen assert(match) keepusing(itemmod itemred)
		*get reduced food consumption
		quiet: gen xfred = xfood if itemred
		quiet: bysort hhid: egen xfcons_r = sum(xfred)
		drop xfred itemred
		*get total food consumption
		quiet: bysort hhid: egen xfcons_t = sum(xfood)
		*remove consumption that is not assigned
		quiet: replace xfood = .z if (itemmod>0) & (itemmod!=hhmod)
		*add binary yes/no indicator whether food is consumed
		quiet: gen bfitem = xfood>0 if !missing(xfood)
		quiet: replace bfitem = .z if xfood==.z
		*mask consumption items with defined probability or maximum number (if not administered)
		quiet: gen r = runiform() if ~missing(bfitem) & (bfitem==1)
		if (`prob'<=1) {
			quiet: replace xfood = .y if ~missing(r) & (r>`prob')
		}
		else {
			quiet: bysort hhid: egen rk = rank(r) if ~missing(r), unique
			quiet: replace xfood = .y if (rk>`prob') & ~missing(r)
			drop rk
		}
		drop r
		*create module consumption
		forvalues kmod = 0/`M' {
			quiet: bysort hhid itemmod: egen cxfood`kmod' = total(xfood) if (itemmod==`kmod') & ((`kmod'==0) | (hhmod==`kmod'))
			quiet: bysort hhid: egen xfcons`kmod' = max(cxfood`kmod')
			drop cxfood`kmod'
		}
		*make items columns and one record per hh
		ren xfood xfitem
		drop itemmod
		quiet: reshape wide xfitem bfitem, i(hhid hhmod weight xfcons*) j(foodid)
		keep hhid hhmod weight xfcons* xfitem* bfitem* 
		order hhid hhmod weight xfcons* xfitem* bfitem*
		*ensure assigned modules are not missing and non-assigned are missing
		forvalues jmod = 1/`M' {
			assert !missing(xfcons`jmod') if (hhmod==`jmod')
			assert missing(xfcons`jmod') if (hhmod!=`jmod')
		}
		*check whether administered consumption is equal to module consumption
		egen xt_items = rowtotal(xfitem*)
		egen xt_mods = rowtotal(xfcons?)
		assert round(xt_items-xt_mods,.1)==0
		drop xt_*
		save "`lc_sdTemp'/hh-food-consumption.dta", replace
		
		*NON-FOOD CONSUMPTION
		use "`lc_sdTemp'/HH-NonFood.dta", clear
		*get household assigned module
		quiet: merge m:1 hhid using "`lc_sdTemp'/HH-ModuleAssignment.dta", nogen assert(match) keepusing(hhmod_f)
		ren hhmod_f hhmod
		*add food assigned  module
		quiet: merge m:1 nonfoodid using "`lc_sdTemp'/fsim_nfpartition.dta", nogen assert(match) keepusing(itemmod itemred)
		*get reduced food consumption
		quiet: gen xnfred = xnonfood if itemred
		quiet: bysort hhid: egen xnfcons_r = sum(xnfred)
		drop xnfred itemred
		*get total consumption
		quiet: bysort hhid: egen xnfcons_t = sum(xnonfood)
		*remove consumption that is not assigned
		quiet: replace xnonfood = .z if (itemmod>0) & (itemmod!=hhmod)
		*add binary yes/no indicator whether food is consumed (for assigned modules)
		quiet: gen bnfitem = xnonfood>0 if !missing(xnonfood)
		quiet: replace bnfitem = .z if xnonfood==.z
		*mask consumption items with defined probability or maximum number (if not administered)
		quiet: gen r = runiform() if ~missing(bnfitem) & (bnfitem==1)
		if (`prob'<=1) {
			quiet: replace xnonfood = .y if ~missing(r) & (r>`prob')
		}
		else {
			quiet: bysort hhid: egen rk = rank(r) if ~missing(r), unique
			quiet: replace xnonfood = .y if (rk>`prob') & ~missing(r)
			drop rk
		}
		drop r
		*create module consumption
		forvalues kmod = 0/`M' {
			quiet: bysort hhid itemmod: egen cxnfood`kmod' = total(xnonfood) if (itemmod==`kmod') & ((`kmod'==0) | (hhmod==`kmod'))
			quiet: bysort hhid: egen xnfcons`kmod' = max(cxnfood`kmod')
			drop cxnfood`kmod'
		}
		*make items columns and one record per hh
		ren xnonfood xnfitem
		drop itemmod
		quiet: reshape wide xnfitem bnfitem, i(hhid hhmod weight xnfcons*) j(nonfoodid)
		keep hhid hhmod weight xnfcons* xnfitem* bnfitem* 
		order hhid hhmod weight xnfcons* xnfitem* bnfitem*
		*ensure assigned modules are not missing and non-assigned are missing
		forvalues jmod = 1/`M' {
			assert !missing(xnfcons`jmod') if (hhmod==`jmod')
			assert missing(xnfcons`jmod') if (hhmod!=`jmod')
		}
		*check whether administered consumption is equal to module consumption
		egen xt_items = rowtotal(xnfitem*)
		egen xt_mods = rowtotal(xnfcons?)
		if (`prob'==1) assert round(xt_items-xt_mods,.1)==0
		drop xt_*
		quiet: compress
		save "`lc_sdTemp'/hh-nonfood-consumption.dta", replace

		*merge food and non-food
		use "`using'", clear
		drop xfood* xnonfood*
		quiet: merge 1:1 hhid using "`lc_sdTemp'/hh-food-consumption.dta", nogen assert(master match)
		quiet: merge 1:1 hhid using "`lc_sdTemp'/hh-nonfood-consumption.dta", nogen assert(master match)
		order xfcons* xnfcons* xf* bf* xnf* bnf*, last
		*get per capita variables
		ren (x*cons_t x*cons_r) (c*cons r*cons)
		foreach v of varlist xfcons* xnfcons* cfcons cnfcons rfcons rnfcons {
			quiet: gen `v'_pc = `v'/hhsize
		} 
		*create percentiles
		foreach v of varlist xfcons0_pc xnfcons0_pc xdurables_pc {
			xtile p`v' = `v' [pweight=weight], nquantiles(4)
		}
		*prepare check variables
		quiet: gen ccons_pc = cfcons_pc + cnfcons_pc + xdurables_pc
		quiet: gen rcons_pc = rfcons_pc + rnfcons_pc + xdurables_pc
		quiet: compress
		save "`lc_sdTemp'/mi_`isim'.dta", replace
	}
end

*will call RCS_estimate_`smethod' defined in fRCS_estimate_.do
capture: program drop RCS_estimate
program define RCS_estimate
	syntax using/, dirbase(string) nmodules(integer) nsim(integer) nmi(integer) lmethod(namelist) model(string) rseed(integer)
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
			quiet: cd "`lc_sdTemp'"
			*load prepared dataset for mi
			use "`lc_sdTemp'/mi_`isim'.dta", clear
			*prepare variables
			quiet: gen xcons_pc = .
			quiet: gen xfcons_pc = .
			foreach v of varlist xfcons?_pc xnfcons?_pc xdurables_pc {
				*save original
				quiet: gen o`v' = `v'
				*remove outliers
				quiet: summ `v',d
				quiet: replace `v'= `r(p99)' if (`v'>`r(p99)') & (`v'<.)
				*log transform
				quiet: gen ln`v' = log(`v')
				quiet: replace ln`v' = log(.1) if `v'==0
			}
			*method selection
			local mipre = ""
			RCS_estimate_`smethod' , nmodules(`nmodules') nmi(`nmi') model("`model'")
			*check if mi dataset
			quiet: mi query
			if "`r(style)'"!="" local mipre = "mi passive:"
			*build aggregates; but replace with originals if available
			quiet: `mipre' replace xcons_pc = xdurables_pc
			foreach v of varlist xfcons?_pc xnfcons?_pc {
				quiet: `mipre' replace xcons_pc = xcons_pc + o`v' if ~missing(o`v')
				quiet: `mipre' replace xcons_pc = xcons_pc + `v' if missing(o`v')
			}
			*cleaning
			if ("`mipre'"!="") {
				keep hhid xcons_pc ccons_pc rcons_pc _*xcons_pc _mi*
				mi register imputed xcons_pc
				mi update
			}
			else {
				keep hhid xcons_pc ccons_pc rcons_pc
			}
			quiet: compress
			save "`lc_sdTemp'/sim_`smethod'_`isim'.dta", replace
		}
	}
end

capture: program drop RCS_collate
program define RCS_collate
	syntax using/, dirbase(string) nsim(integer) nmi(integer) lmethod(namelist)
	*prepare output directories
	local lc_sdTemp = "`dirbase'/Temp"
	local lc_sdOut = "`dirbase'/Out"
	local N = `nsim'
	local nI = `nmi'

	*extract detailed results from estimation
	foreach smethod of local lmethod {
		di "Extracting for `smethod':"
		*is mi dataset?
		forvalues isim = 1/`N' {
			use "`lc_sdTemp'/sim_`smethod'_`isim'.dta", clear
			gen simulation = `isim'
			quiet: mi query
			quiet: if ("`r(style)'"=="") {
				if (`isim'==1) {
					*extract reference and reduced
					ren (xcons_pc ccons_pc rcons_pc) (est1 est0 est99)
					reshape long est, i(hhid) j(x)
					recode x (99=-1)
					replace simulation = x
					drop x
				}
				else {
					ren xcons_pc est
					drop ccons_pc rcons_pc
				}
				gen imputation = (simulation>0)
			}
			quiet: else {
				*convert to long format and get observation for reference and reduced
				quiet: mi convert flong, clear
				quiet: mi unset
				ren (mi_m xcons_pc) (imputation est)
				if (`isim'==1) {
					expand 2 if imputation==0, gen(x)
					replace imputation = -1 if x==1
					drop x
					*move ref/red indicator from Imputation to Simulation level
					replace est = ccons_pc if imputation==0
					replace est = rcons_pc if imputation==-1
					replace simulation = imputation if imputation <1
					replace imputation = 0 if imputation < 1
				}
				else {
					drop if imputation<1
				}
			}
			keep simulation imputation hhid est
			order simulation imputation hhid est
			*append simulations
			if (`isim'>1) {
				append using "`lc_sdTemp'/simd_`smethod'.dta"
			}
			quiet: save "`lc_sdTemp'/simd_`smethod'.dta", replace
		}
		*prepare analysis file
		quiet: merge m:1 hhid using "`using'", nogen keep(match) keepusing(weight cluster urban)
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
		*remove outliers for ref and est
		quiet: summ ref,d
		quiet: replace ref = `r(p99)' if ref>`r(p99)'
		quiet: summ red,d
		quiet: replace red = `r(p99)' if red>`r(p99)'
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
	
* parameters:
*   dirbase: folder anchor for files
*   lmethod: list of methods to analyze
*   povline: poverty line
*   urban: -1: no restriction, 0: rural only, 1: urban only
capture: program drop RCS_analyze
program define RCS_analyze
	syntax using/, dirbase(string) lmethod(namelist) povline(real) [Urban(integer -1)]
	*prepare output directories
	local lc_sdTemp = "`dirbase'/Temp"
	local lc_sdOut = "`dirbase'/Out"
	
	*configure urban/rural filter
	local sfsuff = ""
	local sdrop = " if urban==1-`urban'" 
	if (`urban'==0) {
		local sfsuff = "-rural"
	}
	else if (`urban'==1) {
		local sfsuff = "-urban"
	}	
	*analyze relative bias and relative standard error
	local lind = "fgt0 fgt1 fgt2 gini"
	capture: file close fh
	file open fh using "`lc_sdOut'/simc`sfsuff'.txt", replace write
	file write fh "Method" _tab "Bias (HH)" _tab "SE (HH)" _tab "Bias (CL)" _tab "SE (CL)"_tab "Bias (SIM)" _tab "SE (SIM)"_n
	*add reduced
	use "`lc_sdTemp'/simd_`: word 1 of `lmethod''.dta", clear
	quiet: drop `sdrop'
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
	quiet: drop `sdrop'
	collapse (mean) red ref [aweight=weight], by(simulation)
	quiet: gen rd = (red - ref) / ref * 100
	quiet: fse "ref" "red" 1
	file write fh _tab (r(bias)) _tab (r(se)) _n
	*add other methods
	foreach smethod of local lmethod {
		use "`lc_sdTemp'/simd_`smethod'.dta", clear
		quiet: drop `sdrop'
		file write fh "`smethod'"
		*calculate relative difference at hh-level
		quiet: gen rd = (est - ref) / ref * 100
		quiet: fse "ref" "est" 1
		file write fh _tab (r(bias)) _tab (r(se))
		quiet: hist rd if inrange(rd,-100,100) , normal dens xline(0) name("hh_`smethod'",replace) xtitle("Relative Difference, in %") title("Household Estimation Error (`smethod')") note("Bias: `r(bias)'; Standard Error: `r(se)'") graphregion(color(white)) bgcolor(white)
		graph export "`lc_sdTemp'/hh_rdiff_`smethod'`sfsuff'.png", replace
		local gl_hh = "`gl_hh' hh_`smethod'"
		*per EA
		collapse (mean) est ref [aweight=weight], by(simulation cluster)
		quiet: gen rd = (est - ref) / ref * 100
		quiet: fse "ref" "est" 1
		file write fh _tab (r(bias)) _tab (r(se))
		quiet: hist rd if inrange(rd,-100,100) , normal dens xline(0) name("cl_`smethod'",replace) xtitle("Relative Difference, in %") title("Cluster Estimation Error (`smethod')") note("Bias: `r(bias)'; Standard Error: `r(se)'")graphregion(color(white)) bgcolor(white)
		graph export "`lc_sdTemp'/ea_rdiff_`smethod'`sfsuff'.png", replace
		local gl_cl = "`gl_cl' cl_`smethod'"
		*simulation
		use "`lc_sdTemp'/simd_`smethod'.dta", clear
		quiet: drop `sdrop'
		collapse (mean) est ref [aweight=weight], by(simulation)
		quiet: gen rd = (est - ref) / ref * 100
		quiet: fse "ref" "est" 1
		file write fh _tab (r(bias)) _tab (r(se)) _n
		quiet: hist rd if inrange(rd,-100,100) , normal dens xline(0) name("sim_`smethod'",replace) xtitle("Relative Difference, in %") title("Simulation Estimation Error (`smethod')") note("Bias: `r(bias)'; Standard Error: `r(se)'")graphregion(color(white)) bgcolor(white)
		graph export "`lc_sdTemp'/sim_rdiff_`smethod'`sfsuff'.png", replace
		local gl_sim = "`gl_sim' sim_`smethod'"
	}
	file close fh
	*combine graphs
	local ls = "hh cl sim"
	foreach s of local ls {
		graph combine `gl_`s'', name("cmb_`s'", replace) xcommon
		graph export "`lc_sdOut'/rdiff_`s'`sfsuff'.png", replace
		graph drop `gl_`s'' cmb_`s'
	}
	
	*analyze consumption distribution with poverty rate and inequality measures
	capture: file close fh
	file open fh using "`lc_sdOut'/simp`sfsuff'.txt", replace write
	file write fh "Method" _tab "FGT0 (Bias)" _tab "FGT0 (SE)" _tab "FGT1 (Bias)" _tab "FGT1 (SE)" _tab "FGT2 (Bias)" _tab "FGT2 (SE)" _tab "Gini (Bias)"  _tab "Gini (SE)" _n
	*add reduced aggregated
	file write fh "red"
	*prepare dataset
	use "`lc_sdTemp'/simd_`: word 1 of `lmethod''_imp.dta", clear
	quiet: merge m:1 hhid using "`using'", assert(using match) keep(match) keepusing(hhsize) nogen
	quiet: drop `sdrop'
	quiet: replace hhid = hhid * 100 + imputation
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
	collapse (mean) red* ref* (count) n=hhid [pweight=weight*hhsize], by(simulation)
	quiet: replace ref_gini = (n+1-2*ref_gini / ref) / n
	quiet: replace red_gini = (n+1-2*red_gini / red) / n
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
		quiet: merge m:1 hhid using "`using'", assert(using match) keep(match) keepusing(hhsize) nogen
		quiet: drop `sdrop'
		quiet: replace hhid = hhid * 100 + imputation
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
		collapse (mean) est* ref* (count) n=hhid [pweight=weight*hhsize], by(simulation)
		quiet: replace ref_gini = (n+1-2*ref_gini / ref) / n
		quiet: replace est_gini = (n+1-2*est_gini / est) / n
		drop n
		*make histograms
		quiet: foreach sind of local lind {
			quiet: summ ref_`sind'
			local ref_mean = r(mean)
			fse "ref_`sind'" "est_`sind'" 0
			file write fh _tab (r(bias)) _tab (r(se))
			hist est_`sind' , normal dens xline(`ref_mean') start(0) width(0.01) xscale(range(0 1)) xlabel(0[.25]1) xtitle("`sind'") name("`sind'_`smethod'", replace) title("`sind' (`smethod')") note("Bias: `r(bias)'; Standard Error: `r(se)'") graphregion(color(white)) bgcolor(white)
			graph export "`lc_sdTemp'/sim_`sind'_`smethod'`sfsuff'.png", replace
			local gl_`sind' = "`gl_`sind'' `sind'_`smethod'"
		}
		file write fh _n
	}
	file close fh
	*combine graphs
	foreach sind of local lind {
		graph combine `gl_`sind'', name("cmb_`sind'", replace) graphregion(col(white))
		graph export "`lc_sdOut'/`sind'`sfsuff'.png", replace
		graph drop `gl_`sind'' cmb_`sind'
	}

	*analyze FGT0 bias with range of poverty lines
	di "Analyze FGT0 bias for each percentile..."
	*write output file
	capture: file close fh
	file open fh using "`lc_sdOut'/simfgt0`sfsuff'.txt", replace write
	file write fh "Method"
	*add poverty lines to header
	forvalues rx = 1/100 {
		file write fh _tab "p`rx'"
	}
	file write fh _n
	*obtain poverty lines
	use "`lc_sdTemp'/simd_`: word 1 of `lmethod''_imp.dta", clear
	quiet: merge m:1 hhid using "`using'", assert(using match) keep(match) keepusing(hhsize) nogen
	quiet: drop `sdrop'
	_pctile ref if simulation==1 & imputation==1 [pweight=weight*hhsize], nq(100)
	forvalues i = 1/100 {
		local pline`i' = r(r`i')
	}
	quiet: replace hhid = hhid * 100 + imputation
	drop imputation		
	*calculate FGT0 for list of poverty lines for ref and red
	foreach v of var ref red {
		file write fh "`v'"
		*iterate over poverty lines
		forvalues i = 1/100 {
			gen x = `v' < `pline`i'' if `v'<.
			quiet: mean x [pw=weight*hhsize]
			local z = _b[x]
			file write fh _tab "`z'"
			drop x
		}
		file write fh _n
	}
	*and now for remaining methods
	foreach smethod of local lmethod {
		di "... `smethod' ..."
		file write fh "`smethod'"
		use "`lc_sdTemp'/simd_`smethod'_imp.dta", clear
		quiet: merge m:1 hhid using "`using'", assert(using match) keep(match) keepusing(hhsize) nogen
		quiet: drop `sdrop'
		*iterate over poverty lines
		forvalues i = 1/100 {
			gen x = est < `pline`i'' if est<.
			quiet: mean x [pw=weight*hhsize]
			local z = _b[x]
			file write fh _tab "`z'"
			drop x
		}
		file write fh _n
	}
	file close fh
	*show graph
	import delimited "`lc_sdOut'/simfgt0`sfsuff'.txt", clear
	reshape long p, i(method) j(x)
	twoway (line p x, sort), by(method)
end
	
capture: program drop RCS_run
program define RCS_run
	syntax using/, dirbase(string) nmodules(integer) ncoref(integer) ncorenf(integer) ndiff(integer) nsim(integer) nmi(integer) lmethod(namelist) povline(real) model(string) [EGALshare] rseed(integer) [Prob(real 1.0)]
	
	RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') `EGALshare'
	RCS_mask using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed') prob(`prob')
	RCS_estimate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model(`model') rseed(`rseed')
	RCS_collate using "`using'", dirbase("`dirbase'") nsim(`nsim') nmi(`nmi') lmethod("`lmethod'")
	RCS_analyze using "`using'", dirbase("`dirbase'") lmethod("`lmethod'") povline(`povline')
end

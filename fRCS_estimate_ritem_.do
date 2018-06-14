*estimation function for the random item approach for fRCS::RCS_estimate_ritem

*use the xxx to estimate missing consumption
capture: program drop RCS_estimate_ritem_ujp
program define RCS_estimate_ritem_ujp
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	qui ds
	local all_vars `r(varlist)'
	* renaming					
	rename xfitem* x1*
	rename xnfitem* x2*
	rename bfitem* c1*
	rename bnfitem* c2* 
	* reshape to long format
	qui reshape long x c, i(hhid) j(item)
	*flag for food items
	gen food = floor(item/(10^floor(log10(item))))==1
	*calculate probability of b for each item
	gen b = ~missing(x) if c
	bysort item: egen nb = total(b)
	bysort item: egen ncb = count(b)
	gen pb = nb/ncb
	drop nb ncb
	* per capita value
	gen y = x/hhsize
	* estimate, predict, impute
	quiet: levelsof item, local(litem)
	gen yhat=.
	foreach item of local litem {
		quiet: reg y `model' if c==1 & ~missing(x)
		quiet: predict yhati, xb
		quiet: replace yhat = yhati if item==`item'
		drop yhati
	}
	replace x = yhat * hhsize if c==1 & missing(x)
	drop food pb b y yhat
	qui reshape wide x c, i(hhid) j(item)
	rename x1* xfitem*
	rename x2* xnfitem*
	rename c1* bfitem*
	rename c2* bnfitem*
	* keep only original variables
	keep `all_vars' 
	* totals
	egen aux_xfcons1 = rowtotal(xfitem*)
	egen aux_xnfcons1 = rowtotal(xnfitem*)
	replace xfcons1_pc = aux_xfcons1/hhsize
	replace xnfcons1_pc = aux_xnfcons1/hhsize
	*all consumption is collected in module 1; thus, need to set module 0 to be zero
	replace xfcons0_pc = 0
	replace xnfcons0_pc = 0
	drop aux*
	* don't use originals
	replace oxfcons0_pc = .
	replace oxnfcons0_pc = .
	replace oxfcons1_pc = .
	replace oxnfcons1_pc = .
end


*SIMONS CODE:

*use the xxx to estimate missing consumption
capture: program drop RCS_estimate_ritem_mi
program define RCS_estimate_ritem_mi
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	local mipre = "mi passive: "
	*get IDs for food and non-food items
	local sflist = ""
	foreach v of varlist xfitem* {
		local sflist "`sflist' `:subinstr local v "xfitem" ""'"
	}
	local snflist = ""
	foreach v of varlist xnfitem* {
		local snflist "`snflist' `:subinstr local v "xnfitem" ""'"
	}
	*collect items with sufficient observations
	local sbitem = ""
	local sxitem = ""
	*food
	foreach id of local sflist {
		quiet: count if bfitem`id'>0 & ~missing(xfitem`id')
		if (`r(N)'>5) {
			*include for MI
			local sxitem "`sxitem' xfitem`id'"
			quiet: replace xfitem`id' = . if missing(xfitem`id')
			local sbitem "`sbitem' bfitem`id'"
		}
		else {
			*excluded from MI, replace with average if necessary
			quiet: summ xfitem`id' if bfitem`id'==1
			if (`r(N)'>0) {
				quiet: replace xfitem`id' = `r(mean)' if bfitem`id'==1 & missing(xfitem`id')
			}
		}
	}
	*nonfood
	foreach id of local snflist {
		quiet: count if bnfitem`id'>0 & ~missing(xnfitem`id')
		if (`r(N)'>5) {
			*include for MI
			local sxitem "`sxitem' xnfitem`id'"
			quiet: replace xnfitem`id' = . if missing(xnfitem`id')
			local sbitem "`sbitem' bnfitem`id'"
		} 
		else {
			*excluded from MI, replace with average if necessary
			quiet: summ xnfitem`id' if bnfitem`id'==1
			if (`r(N)'>0) {
				quiet: replace xnfitem`id' = `r(mean)' if bnfitem`id'==1 & missing(xnfitem`id')
			}
		}
	}
	
	*run MI
	mi set wide
	mi register imputed `sxitem'
	mi register regular `sbitem'
	mi register regular hh* cluster strata pchild psenior xdurables_pc
	mi register passive xcons_pc xfcons_pc xfcons1_pc xnfcons1_pc
	mi impute chained (regress) `sxitem' = `sbitem' i.pxdurables_pc `model', add(`nI') report
	`mipre' egen aux_xfcons1 = rowtotal(xfitem*)
	`mipre' egen aux_xnfcons1 = rowtotal(xnfitem*)
	`mipre' replace xfcons1_pc = aux_xfcons1/hhsize
	`mipre' replace xnfcons1_pc = aux_xnfcons1/hhsize
	drop aux*
	* don't use originals
	replace oxfcons0_pc = .
	replace oxnfcons0_pc = .
	replace oxfcons1_pc = .
	replace oxnfcons1_pc = .
	*all consumption is collected in module 1; thus, need to set module 0 to be zero
	replace xfcons0_pc = 0
	replace xnfcons0_pc = 0
end

*use the xxx to estimate missing consumption
capture: program drop RCS_estimate_ritem_avg
program define RCS_estimate_ritem_avg
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	* initial variable list
	qui ds
	local all_vars `r(varlist)'
	* renaming					
	rename xfitem* x1*
	rename xnfitem* x2*
	rename bfitem* b1*
	rename bnfitem* b2* 
	* reshape to long format
	qui reshape long x b, i(hhid) j(item)
	gen food = item<2000
	* total number of items consumed per category (food - nonfood) and number evaluated
	egen nc = sum(b), by(hhid food)
	egen ne = sum(x>0 & ~missing(x)), by(hhid food)
	* total items consumed
	egen nt = sum(b), by(hhid) 
	* weigh
	gen w = (nc/ne)*weight
	* per capita value
	gen y = x/hhsize
	* estimate, predict, impute
	quiet reg y [pw=w] if b==1 & ~missing(x)
	*predict xb, xb
	replace x = _b[_cons]*hhsize if b==1 & missing(x)
	drop food nc ne w y
	qui reshape wide x b, i(hhid) j(item)
	rename x1* xfitem*
	rename x2* xnfitem*
	rename b1* bfitem*
	rename b2* bnfitem*
	* keep only original variables
	keep `all_vars' 
	* totals
	egen aux_xfcons1 = rowtotal(xfitem*)
	egen aux_xnfcons1 = rowtotal(xnfitem*)
	replace xfcons1_pc = aux_xfcons1/hhsize
	replace xnfcons1_pc = aux_xnfcons1/hhsize
	drop aux*
	* don't use originals
	replace oxfcons0_pc = .
	replace oxnfcons0_pc = .
	replace oxfcons1_pc = .
	replace oxnfcons1_pc = .
	*all consumption is collected in module 1; thus, need to set module 0 to be zero
	replace xfcons0_pc = 0
	replace xnfcons0_pc = 0
end

*use the xxx to estimate missing consumption
capture: program drop RCS_estimate_ritem_med
program define RCS_estimate_ritem_med
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	* initial variable list
	qui ds
	local all_vars `r(varlist)'
	* renaming					
	rename xfitem* x1*
	rename xnfitem* x2*
	rename bfitem* b1*
	rename bnfitem* b2* 
	* reshape to long format
	qui reshape long x b, i(hhid) j(item)
	gen food = item<2000
	* total number of items consumed per category (food - nonfood) and number evaluated
	egen nc = sum(b), by(hhid food)
	egen ne = sum(x>0 & ~missing(x)), by(hhid food)
	* total items consumed
	egen nt = sum(b), by(hhid) 
	* weight
	gen w = (nc/ne)*weight
	* per capita value
	gen y = x/hhsize
	* estimate, predict, impute
	egen aux_xb = median(y) if b==1 & ~missing(x), by(hhid food)
	egen xb = mean(aux_xb), by(hhid food)
	drop aux*
	*quiet reg y i.hhid##i.food if b==1 & ~missing(x)
	*predict xb, xb
	replace x = xb*hhsize if b==1 & missing(x)
	drop food nc ne w y xb nt 
	qui reshape wide x b, i(hhid) j(item)
	rename x1* xfitem*
	rename x2* xnfitem*
	rename b1* bfitem*
	rename b2* bnfitem*
	* keep only original variables
	keep `all_vars' 
	* totals
	egen aux_xfcons1 = rowtotal(xfitem*)
	egen aux_xnfcons1 = rowtotal(xnfitem*)
	replace xfcons1_pc = aux_xfcons1/hhsize
	replace xnfcons1_pc = aux_xnfcons1/hhsize
	drop aux*
	* don't use originals
	replace oxfcons0_pc = .
	replace oxnfcons0_pc = .
	replace oxfcons1_pc = .
	replace oxnfcons1_pc = .
	*all consumption is collected in module 1; thus, need to set module 0 to be zero
	replace xfcons0_pc = 0
	replace xnfcons0_pc = 0
end

*use the xxx to estimate missing consumption
capture: program drop RCS_estimate_ritem_ols_lin_fe
program define RCS_estimate_ritem_ols_lin_fe
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	* initial variable list
	qui ds
	local all_vars `r(varlist)'
	* renaming					
	rename xfitem* x1*
	rename xnfitem* x2*
	rename bfitem* b1*
	rename bnfitem* b2* 
	* reshape to long format
	qui reshape long x b, i(hhid) j(item)
	gen food = item<2000
	* total number of items consumed per category (food - nonfood) and number evaluated
	egen nc = sum(b), by(hhid food)
	egen ne = sum(x>0 & ~missing(x)), by(hhid food)
	* total items consumed
	egen nt = sum(b), by(hhid) 
	* weight
	gen w = (nc/ne)*weight
	* per capita value
	gen y = x/hhsize
	* estimate, predict, impute
	quiet reg y i.hhid##i.food if b==1 & ~missing(x)
	predict xb, xb
	replace x = xb*hhsize if b==1 & missing(x)
	drop food nc ne w y xb nt 
	qui reshape wide x b, i(hhid) j(item)
	rename x1* xfitem*
	rename x2* xnfitem*
	rename b1* bfitem*
	rename b2* bnfitem*
	* keep only original variables
	keep `all_vars' 
	* totals
	egen aux_xfcons1 = rowtotal(xfitem*)
	egen aux_xnfcons1 = rowtotal(xnfitem*)
	replace xfcons1_pc = aux_xfcons1/hhsize
	replace xnfcons1_pc = aux_xnfcons1/hhsize
	drop aux*
	* don't use originals
	replace oxfcons0_pc = .
	replace oxnfcons0_pc = .
	replace oxfcons1_pc = .
	replace oxnfcons1_pc = .
	*all consumption is collected in module 1; thus, need to set module 0 to be zero
	replace xfcons0_pc = 0
	replace xnfcons0_pc = 0
end

*use the xxx to estimate missing consumption
capture: program drop RCS_estimate_ritem_parx_log
program define RCS_estimate_ritem_parx_log
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	qui ds
	local all_vars `r(varlist)'
	* reshape to long format
	qui reshape long xfitem xnfitem bfitem bnfitem, i(hhid) j(item)
	* original yes/no
	gen obfitem = bfitem
	gen obnfitem = bnfitem 
	* no value or zero
	replace bfitem = 0 if xfitem==.y
	replace bnfitem = 0 if xnfitem==.y
	* back to wide format
	qui reshape wide xfitem xnfitem bfitem bnfitem obfitem obnfitem, i(hhid) j(item)
	keep `all_vars' obfitem* obnfitem*
	*partial aggregate
	egen xpartial = rowtotal(xfitem* xnfitem*) 
	gen lxpartial = log(xpartial)
	* estimation
	qui reg lxpartial `model' bfitem* bnfitem*
	qui reshape long xfitem xnfitem bfitem bnfitem obfitem obnfitem, i(hhid) j(item)
	replace bfitem = obfitem
	replace bnfitem = obnfitem
	drop obfitem obnfitem
	qui reshape wide xfitem xnfitem bfitem bnfitem , i(hhid) j(item)
	keep `all_vars' 
	predict xb, xb
	replace xb = exp(xb)
	* assign zero nonfood consumption
	replace xfcons1_pc = xb/hhsize
	replace xnfcons1_pc = 0
	drop xb  
	* keep only initial variable list
	keep `all_vars'
	*all consumption is collected in module 1; thus, need to set module 0 to be zero
	replace xfcons0_pc = 0
	replace xnfcons0_pc = 0
	* don't use originals
	replace oxfcons0_pc = .
	replace oxnfcons0_pc = .
	replace oxfcons1_pc = .
	replace oxnfcons1_pc = .
end

*use the xxx to estimate missing consumption
capture: program drop RCS_estimate_ritem_parx_lin
program define RCS_estimate_ritem_parx_lin
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	qui ds
	local all_vars `r(varlist)'
	* reshape to long format
	qui reshape long xfitem xnfitem bfitem bnfitem, i(hhid) j(item)
	* original yes/no
	gen obfitem = bfitem
	gen obnfitem = bnfitem 
	* no value or zero
	replace bfitem = 0 if xfitem==.y
	replace bnfitem = 0 if xnfitem==.y
	* back to wide format
	qui reshape wide xfitem xnfitem bfitem bnfitem obfitem obnfitem, i(hhid) j(item)
	keep `all_vars' obfitem* obnfitem*
	*partial aggregate
	egen xpartial = rowtotal(xfitem* xnfitem*) 
	* estimation
	qui reg xpartial `model' bfitem* bnfitem*
	qui reshape long xfitem xnfitem bfitem bnfitem obfitem obnfitem, i(hhid) j(item)
	replace bfitem = obfitem
	replace bnfitem = obnfitem
	drop obfitem obnfitem
	qui reshape wide xfitem xnfitem bfitem bnfitem , i(hhid) j(item)
	keep `all_vars' 
	predict xb, xb
	* assign zero nonfood consumption
	replace xfcons1_pc = xb/hhsize
	replace xnfcons1_pc = 0
	drop xb  
	* keep only initial variable list
	keep `all_vars'
	*all consumption is collected in module 1; thus, need to set module 0 to be zero
	replace xfcons0_pc = 0
	replace xnfcons0_pc = 0
	* don't use originals
	replace oxfcons0_pc = .
	replace oxnfcons0_pc = .
	replace oxfcons1_pc = .
	replace oxnfcons1_pc = .
end

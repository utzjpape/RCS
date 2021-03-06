*estimation function for the random item approach for fRCS::RCS_estimate_ritem

*use mi to estimate missing consumption, all items at once
capture: program drop RCS_estimate_ri_mi_seq
program define RCS_estimate_ri_mi_seq
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
	local M = `nmodules'
	local nI = `nmi'
	*prepare dataset
	qui ds
	local all_vars `r(varlist)'
	*calculating number of consumed items
	egen n1 = rowtotal(bfitem*)
	gen n1sq = n1*n1
	egen n2 = rowtotal(bnfitem*)
	gen n2sq = n2*n2
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
	local mipre = "mi passive: "
	*run MI
	mi set wide
	mi register imputed y
	mi register regular n1 n1sq n2 n2sq c x
	mi register regular hh* cluster strata pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc
	*replaced if c==1 with conditional option
	mi impute regress y = i.pxdurables_pc `model', add(`nI') conditional(c==1) by(item)
	*transform into household-level dataset
	assert (y==0) if (c==0)
	keep hhid xdurables_pc ccons_pc rcons_pc y _* item hhsize 
	*sum at the hh-level
	mi convert flongsep _rcs_ri_mi_par, clear
	mi xeq: by hhid, sort: egen y_sum = total(y)
	*maintain mi consistency
	mi xeq 0: gen Mis_y = (y==.) ; by hhid, sort: egen Mis_total = total(Mis_y) ; replace y_sum = . if Mis_total>0
	*only keep hh-level records
	mi xeq: sort hhid item; by hhid: drop if _n>1 ; drop item y Mis_y Mis_total ; ren y_sum xfcons1_pc
	mi register imputed xfcons1_pc
	mi convert wide, clear
	mi erase _rcs_ri_mi_par
	*note that we stop distinguishing between food and non-food here (for computational efficiencies)
	gen xnfcons1_pc = 0
	*all consumption is collected in module 1; thus, need to set module 0 to be zero
	gen xfcons0_pc = 0
	gen xnfcons0_pc = 0
	gen xcons_pc = .
	mi register passive xcons_pc
	* don't use originals
	gen oxfcons0_pc = .
	gen oxnfcons0_pc = .
	gen oxfcons1_pc = .
	gen oxnfcons1_pc = .
end

*use mi to estimate missing consumption, all items at once
capture: program drop RCS_estimate_ri_mi_par
program define RCS_estimate_ri_mi_par
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
	local M = `nmodules'
	local nI = `nmi'
	*calculating number of consumed items
	egen n1 = rowtotal(bfitem*)
	gen n1sq = n1*n1
	egen n2 = rowtotal(bnfitem*)
	gen n2sq = n2*n2
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
	local mipre = "mi passive: "
	*run MI
	mi set wide
	mi register imputed y
	mi register regular n1 n1sq n2 n2sq c x
	mi register regular hh* cluster strata pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc
	*replaced if c==1 with conditional option
	mi impute regress y = i.pxdurables_pc i.pxdurables_pc#i.item `model', add(`nI') conditional(c==1)
	*transform into household-level dataset
	assert (y==0) if (c==0)
	keep hhid xdurables_pc ccons_pc rcons_pc y _* item hhsize 
	*sum at the hh-level
	mi convert flongsep _rcs_ri_mi_par, clear
	mi xeq: by hhid, sort: egen y_sum = total(y)
	*maintain mi consistency
	mi xeq 0: gen Mis_y = (y==.) ; by hhid, sort: egen Mis_total = total(Mis_y) ; replace y_sum = . if Mis_total>0
	*only keep hh-level records
	mi xeq: sort hhid item; by hhid: drop if _n>1 ; drop item y Mis_y Mis_total ; ren y_sum xfcons1_pc
	mi register imputed xfcons1_pc
	mi convert wide, clear
	mi erase _rcs_ri_mi_par
	*note that we stop distinguishing between food and non-food here (for computational efficiencies)
	gen xnfcons1_pc = 0
	*all consumption is collected in module 1; thus, need to set module 0 to be zero
	gen xfcons0_pc = 0
	gen xnfcons0_pc = 0
	gen xcons_pc = .
	mi register passive xcons_pc
	* don't use originals
	gen oxfcons0_pc = .
	gen oxnfcons0_pc = .
	gen oxfcons1_pc = .
	gen oxnfcons1_pc = .
end

*use the xxx to estimate missing consumption, all items at once
capture: program drop RCS_estimate_ri_par
program define RCS_estimate_ri_par
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	qui ds
	local all_vars `r(varlist)'
	*calculating number of consumed items
	egen n1 = rowtotal(bfitem*)
	gen n1sq = n1*n1
	egen n2 = rowtotal(bnfitem*)
	gen n2sq = n2*n2
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
	reg y `model' i.pxdurables_pc n1 n1sq n2 n2sq if (c==1) & ~missing(x)
	predict yhat if (c==1) & missing(x), xb
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

*use the xxx to estimate missing consumption, item-by-item
capture: program drop RCS_estimate_ri_seq
program define RCS_estimate_ri_seq
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	qui ds
	local all_vars `r(varlist)'
	*calculating number of consumed items
	egen n1 = rowtotal(bfitem*)
	gen n1sq = n1*n1
	egen n2 = rowtotal(bnfitem*)
	gen n2sq = n2*n2
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
		quiet: capture: reg y `model' n1 n1sq n2 n2sq if (c==1) & ~missing(x) & (item==`item')
		if _rc==0 {
			quiet: predict yhati if (c==1) & missing(x) & (item==`item'), xb
			quiet: replace yhat = yhati if ~missing(yhati)
			drop yhati
		}
		else {
			*insufficient observations
			quiet: summ y if (c==1) & ~missing(x) & (item==`item')
			if !missing(r(mean)) {
				quiet: replace yhat = r(mean) if (c==1) & missing(x) & (item==`item')
			}
			else {
				quiet: replace yhat = 0 if (c==1) & missing(x) & (item==`item')
			}
		}
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
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
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
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
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
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
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
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
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
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
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
	syntax , nmodules(integer) nmi(integer) model(string) logmodel(string)
	*prepare output directories
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

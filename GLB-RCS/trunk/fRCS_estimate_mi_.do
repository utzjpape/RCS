*estimation function using multiple imputations for fRCS::RCS_estimate

*use Multi-variate normal imputation using MCMC
capture: program drop RCS_estimate_mi_mvn
program define RCS_estimate_mi_mvn
	syntax , nmodules(integer) nmi(integer) model(string)
	*prepare output directories
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	local mipre = "mi passive: "
	*run MI
	mi set wide
	mi register imputed xfcons?_pc xnfcons?_pc
	mi register regular oxfcons?_pc oxnfcons?_pc
	mi register regular hh* cluster lnxfcons0_pc lnxnfcons0_pc strata pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc
	mi register passive xcons_pc xfcons_pc
	mi impute mvn xfcons?_pc xnfcons?_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model', add(`nI') burnin(1000)
end

*using chained equations
capture: program drop RCS_estimate_mi_ce
program define RCS_estimate_mi_ce
	syntax , nmodules(integer) nmi(integer) model(string)
	*prepare output directories
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	local mipre = "mi passive: "
	*run MI
	mi set wide
	mi register imputed xfcons?_pc xnfcons?_pc
	mi register regular oxfcons?_pc oxnfcons?_pc
	mi register regular hh* cluster lnxfcons0_pc lnxnfcons0_pc strata pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc
	mi register passive xcons_pc xfcons_pc
	mi impute chained (regress) xfcons?_pc xnfcons?_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model', add(`nI') report
end

*log regressions with multiple imputations
capture: program drop RCS_estimate_mi_regl
program define RCS_estimate_mi_regl
	syntax , nmodules(integer) nmi(integer) model(string)
	RCS_estimate_mi_reg , nmodules(`nmodules') nmi(`nmi') model(`model') log
end

*truncated regressions with multiple imputations
capture: program drop RCS_estimate_mi_treg
program define RCS_estimate_mi_treg
	syntax , nmodules(integer) nmi(integer) model(string)
	RCS_estimate_mi_reg , nmodules(`nmodules') nmi(`nmi') model(`model') log trunc
end

*regression but with multiple imputations
capture: program drop RCS_estimate_mi_reg
program define RCS_estimate_mi_reg
	syntax , nmodules(integer) nmi(integer) model(string) [log] [TRUNCated]
	*prepare output directories
	local M = `nmodules'
	local nI = `nmi'
	* reshape to long format
	ren (xfcons0 xnfcons0) (fcore nfcore)
	ren (xfcons? xnfcons?) (x1? x0?)
	qui reshape long x0 x1, i(hhid) j(imod)
	qui reshape long x, i(hhid imod) j(food)
	* per capita value
	gen y = x/hhsize
	if ("`log'"!="") {
		replace y = log(y)
	}
	* estimate, predict, impute
	local mipre = "mi passive: "
	*run MI
	mi set wide
	mi register imputed y
	mi register regular imod food
	mi register regular hh* cluster strata pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc
	*run ols or truncated regression
	if ("`truncated'"=="") {
		mi impute regress y = i.pxdurables_pc `model', add(`nI') by(imod food)
	}
	else {
		mi impute truncreg y = i.pxdurables_pc `model', ll(0) add(`nI') by(imod food)
	}
	*transform into household-level dataset
	keep hhid xdurables_pc ccons_pc rcons_pc y _* imod food hhsize fcore nfcore
	if ("`log'"!="") {
		mi xeq: replace y = exp(y)
	}
	*sum at the hh-level
	quiet {
		mi convert flongsep _rcs_mi_reg, clear
		mi xeq: by hhid, sort: egen y_sum = total(y)
		*maintain mi consistency
		mi xeq 0: gen Mis_y = (y==.) ; by hhid, sort: egen Mis_total = total(Mis_y) ; replace y_sum = . if Mis_total>0
		*only keep hh-level records
		mi xeq: sort hhid imod food; by hhid: drop if _n>1 ; drop imod food y Mis_y Mis_total ; ren y_sum xfcons1_pc
		mi register imputed xfcons1_pc
		mi convert wide, clear
		mi erase _rcs_mi_reg
		*note that we stop distinguishing between food and non-food here (for computational efficiencies)
		gen xnfcons1_pc = 0
		*all consumption is collected in module 1; thus, need to set module 0 to be zero
		gen xfcons0_pc = fcore / hhsize
		gen xnfcons0_pc = nfcore / hhsize
		gen xcons_pc = .
		mi register passive xcons_pc
	}
	* don't use originals or other optional modules
	forvalues imod = 0 / `M' {
		gen oxfcons`imod'_pc = .
		gen oxnfcons`imod'_pc = .
		if (`imod' > 1) {
			gen xfcons`imod'_pc = 0
			gen xnfcons`imod'_pc = 0
		}
	}
end

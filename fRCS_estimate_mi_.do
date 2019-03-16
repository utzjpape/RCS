*estimation function using multiple imputations for fRCS::RCS_estimate

*use Multi-variate normal imputation using MCMC
capture: program drop RCS_estimate_mi_mvn
program define RCS_estimate_mi_mvn
	syntax , nmodules(integer) nmi(integer) model(string)
	*prepare output directories
	local M = `nmodules'
	local nI = `nmi'
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
	RCS_estimate_mi_reg , nmodules(`nmodules') nmi(`nmi') model(`model') log reg(truncated)
end

*truncated regressions with multiple imputations
capture: program drop RCS_estimate_mi_2ce
program define RCS_estimate_mi_2ce
	syntax , nmodules(integer) nmi(integer) model(string)
	RCS_estimate_mi_reg , nmodules(`nmodules') nmi(`nmi') model(`model') reg(twostep)
end

*truncated regressions with multiple imputations
capture: program drop RCS_estimate_mi_2cel
program define RCS_estimate_mi_2cel
	syntax , nmodules(integer) nmi(integer) model(string)
	RCS_estimate_mi_reg , nmodules(`nmodules') nmi(`nmi') model(`model') log reg(twostep)
end

*regression but with multiple imputations
* parameters:
*   nmodules: number of modules
*   nmi: number of multiple imputations
*   model: model for imputation
*   log: log-transform
*   regfunc: normal or truncated or twostep: 
*      normal: standard regression
*      truncated: use regression truncated at 0
*      twostep: use a two-step MICE approach to predict consumption based on whether consumed
capture: program drop RCS_estimate_mi_reg
program define RCS_estimate_mi_reg
	syntax , nmodules(integer) nmi(integer) model(string) [log REGfunc(string)]
	if ("`regfunc'"=="") local regfunc = "normal"
	if !inlist("`regfunc'","normal","truncated","twostep") {
		di "In RCS_estimate_mi_reg the parameter 'regfunc' must be 'normal', 'truncated' or 'twostep'."
		error 1
	}
	*prepare output directories
	local M = `nmodules'
	local nI = `nmi'
	* reshape to long format
	ren (xfcons0_pc xnfcons0_pc) (fcore nfcore)
	ren (xfcons?_pc xnfcons?_pc) (y1? y0?)
	qui reshape long y0 y1, i(hhid) j(imod)
	qui reshape long y, i(hhid imod) j(food)
	*remember 0 consumption
	gen y_0 = y==0
	if ("`log'"!="") {
		*regularize for zero consumption
		replace y = .01 if y<=0
		replace y = log(y)
	}
	*get models degrees of freedom and make sure we have sufficient observation
	*quiet: reg y i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model'
	*local df = e(df_m)
	*run MI
	mi set wide
	mi register imputed y y_0
	mi register regular imod food
	mi register regular hh* cluster strata pchild psenior cfcons_pc cnfcons_pc pxfcons0_pc pxnfcons0_pc pxdurables_pc
	*run ols or truncated regression
	if ("`regfunc'"=="normal") {
		mi impute regress y = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model', add(`nI') by(imod food)
	}
	else if ("`regfunc'"=="truncated") {
		mi impute truncreg y = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model', ll(0) add(`nI') by(imod food)
	}
	else if ("`regfunc'"=="twostep") {
		mi impute chained (logit) y_0 (reg, cond(if y_0==0)) y = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model', add(`nI') by(imod food)
	}
	*transform into household-level dataset
	keep hhid xdurables_pc ccons_pc rcons_pc y y_0 _* imod food hhsize fcore nfcore oxfcons* oxnfcons*
	if ("`log'"!="") {
		mi xeq: replace y = exp(y)
	}
	*reshape back to the hh-level
	mi xeq: replace y = 0 if y_0
	drop y_0
	mi reshape wide y, i(hhid imod) j(food)
	mi rename y0 xnfcons
	mi rename y1 xfcons
	mi reshape wide xfcons xnfcons, i(hhid) j(imod)
	foreach v of var xfcons* xnfcons* {
		mi ren `v' `v'_pc
	}
	mi ren fcore xfcons0_pc
	mi ren nfcore xnfcons0_pc
	gen xcons_pc = .
	mi register passive xcons_pc
end

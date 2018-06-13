*estimation function using multiple imputations for fRCS::RCS_estimate

*use Multi-variate normal imputation using MCMC
capture: program drop RCS_estimate_mi_mvn
program define RCS_estimate_mi_mvn
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	local mipre = "mi passive: "
	*run MI
	mi set wide
	mi register imputed xfcons1_pc xnfcons1_pc xfcons2_pc xnfcons2_pc xfcons3_pc xnfcons3_pc xfcons4_pc xnfcons4_pc
	mi register regular oxfcons0_pc oxnfcons0_pc oxfcons1_pc oxnfcons1_pc oxfcons2_pc oxnfcons2_pc oxfcons3_pc oxnfcons3_pc oxfcons4_pc oxnfcons4_pc
	mi register regular hh* cluster lnxfcons0_pc lnxnfcons0_pc strata pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc
	mi register passive xcons_pc xfcons_pc
	mi impute mvn xfcons1_pc xfcons2_pc xfcons3_pc xfcons4_pc xnfcons1_pc xnfcons2_pc xnfcons3_pc xnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model', add(`nI') burnin(1000)
end

*using chained equations
capture: program drop RCS_estimate_mi_ce
program define RCS_estimate_mi_ce
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	local mipre = "mi passive: "
	*run MI
	mi set wide
	mi register imputed xfcons1_pc xnfcons1_pc xfcons2_pc xnfcons2_pc xfcons3_pc xnfcons3_pc xfcons4_pc xnfcons4_pc
	mi register regular oxfcons0_pc oxnfcons0_pc oxfcons1_pc oxnfcons1_pc oxfcons2_pc oxnfcons2_pc oxfcons3_pc oxnfcons3_pc oxfcons4_pc oxnfcons4_pc
	mi register regular hh* cluster lnxfcons0_pc lnxnfcons0_pc strata pchild psenior cfcons_pc cnfcons_pc xdurables_pc lnxdurables_pc
	mi register passive xcons_pc xfcons_pc
	mi impute chained (regress) xfcons1_pc xfcons2_pc xfcons3_pc xfcons4_pc xnfcons1_pc xnfcons2_pc xnfcons3_pc xnfcons4_pc = i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model', add(`nI') report
end

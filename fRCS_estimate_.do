*estimation function for fRCS::RCS_estimate

*use the average to estimate missing consumption
capture: program drop RCS_estimate_avg
program define RCS_estimate_avg
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	quiet: forvalues imod = 1/`M' {
		egen avg_xfcons`imod'_pc = mean(xfcons`imod'_pc)
		replace xfcons`imod'_pc = avg_xfcons`imod'_pc if xfcons`imod'_pc>=.
		egen avg_xnfcons`imod'_pc = mean(xnfcons`imod'_pc)
		replace xnfcons`imod'_pc = avg_xnfcons`imod'_pc if xnfcons`imod'_pc>=.
	}
	drop avg_x*
end

*use the median to estimate missing consumption
capture: program drop RCS_estimate_med
program define RCS_estimate_med
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	quiet: forvalues imod = 1/`M' {
		egen avg_xfcons`imod'_pc = median(xfcons`imod'_pc)
		replace xfcons`imod'_pc = avg_xfcons`imod'_pc if xfcons`imod'_pc>=.
		egen avg_xnfcons`imod'_pc = median(xnfcons`imod'_pc)
		replace xnfcons`imod'_pc = avg_xnfcons`imod'_pc if xnfcons`imod'_pc>=.
	}
	drop avg_x*
end

*use the tobit to estimate missing consumption
capture: program drop RCS_estimate_tobit
program define RCS_estimate_tobit
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	quiet: forvalues imod = 1/`M' {
		*food
		tobit xfcons`imod'_pc xfcons0_pc xnfcons0_pc xdurables_pc `model' i.cluster [aweight=weight], ll(0)
		predict y`imod'_pc if xfcons`imod'_pc>=.
		replace xfcons`imod'_pc = max(y`imod'_pc,0) if xfcons`imod'_pc>=.
		drop y`imod'_pc
		*non-food
		tobit xnfcons`imod'_pc xfcons0_pc xnfcons0_pc xdurables_pc `model' i.cluster [aweight=weight], ll(0)
		predict y`imod'_pc if xnfcons`imod'_pc>=.
		replace xnfcons`imod'_pc = max(y`imod'_pc,0) if xnfcons`imod'_pc>=.
		drop y`imod'_pc
	}
end

*use the tobit to estimate missing consumption
capture: program drop RCS_estimate_reg
program define RCS_estimate_reg
	syntax , nmodules(integer) nsim(integer) nmi(integer) model(string)
	*prepare output directories
	local N = `nsim'
	local M = `nmodules'
	local nI = `nmi'
	*start estimation
	quiet: forvalues imod = 1/`M' {
		*food
		reg xfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight]
		predict y`imod'_pc if xfcons`imod'_pc>=.
		replace xfcons`imod'_pc = max(y`imod'_pc,0) if xfcons`imod'_pc>=.
		drop y`imod'_pc
		*non-food
		reg xnfcons`imod'_pc i.pxfcons0_pc i.pxnfcons0_pc i.pxdurables_pc `model' i.cluster [aweight=weight]
		predict y`imod'_pc if xnfcons`imod'_pc>=.
		replace xnfcons`imod'_pc = max(y`imod'_pc,0) if xnfcons`imod'_pc>=.
		drop y`imod'_pc
	}
end

*SIMULATE RCS FOR KENYA

clear all
ma drop all
set more off

*parameters
*Poverty line from povcalnet
local xpovline = 1.90 * 35.4296

local using= "${gsdData}/KEN-HHData.dta"
capture confirm file "`using'"
if _rc != 0 {
	quiet: do "${gsdDo}/prep-KEN.do"
}
local usingred= "${gsdData}/KEN-HHData-red.dta"
capture confirm file "`usingred'"
if _rc != 0 {
	use "`using'", clear
	gen r = runiform()
	bysort cluster: egen xr = mean(r)
	drop if xr < .5
	drop r xr
	local lis = "food nonfood"
	foreach food of local lis {
		reshape long x`food', i(hhid cluster id_hh) j(fid)
		replace x`food' = 0 if missing(x`food')
		bysort hhid cluster id_hh: egen xtotal= total(x`food')
		gen r`food' = x`food' / xtotal
		bysort fid: egen xshare = mean(r`food')
		drop if xshare < .01
		drop xtotal r`food' xshare
		reshape wide x`food', i(hhid cluster id_hh) j(fid)
	}
	save "`usingred'", replace
}

*start RCS code
*number of modules
local nmodules = 9
*number of simulations (should be 20)
local nsim = 10
*number of imputations (should be 50)
local nmi = 50
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3
*methods
local lmethod = "med avg reg tobit mi_ce mi_reg mi_regl mi_2ce mi_2cel"

local lk = "1 5 10 20 50"
local lk = "1 50"
local lk = "0 50"

*run over different p
foreach k of local lk {
	local ncoref = `k'
	local ncorenf = `k'
	local dirbase = "${gsdOutput}/KEN-red-c`k'-m`nmodules'"
	*just for local execution
	local lc_sdTemp = "`dirbase'/Temp"
	
	*create instance to run RCS simulations
	capture classutil drop .r
	.r = .RCS.new
	*.r.test
	.r.prepare using "`usingred'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff')
	.r.mask , nsim(`nsim')
	.r.estimate , lmethod("`lmethod'") nmi(`nmi')
	.r.collate
	.r.analyze
}

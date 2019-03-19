*SIMULATE Hergeiza for the World Bank Working Paper Series using fRCS from Revision 1585.
*Results are saved in ResultsArchive/WBWPS

clear all
ma drop all
set more off

*prepare data if needed
*if only for Hargeiza, check prep-SLD.do to change local macro
local bH=""
local using= "${gsdData}/SLD`bH'-HHData.dta"
capture confirm file "`using'"
if _rc != 0 {
	quiet: do "${gsdDo}/prep-SLD.do"
}

*other parameters
*number of modules
local nmodules = 4
*number of simulations
local nsim = 20
*number of imputations 
local nmi = 50
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3
*methods
local lmethod = "med avg reg tobit mi_ce mi_2cel"
local kcore = 0
local ncoref = `kcore'
local ncorenf = `kcore'
local ndiff=`ndiff'
local lmethod = "`lmethod'"
local model = "hhsize pchild bwork i.hhsex i.hhwater hhcook_5 i.hhtoilet i.hhmaterial i.hhfood urban"
local dirbase = "${gsdOutput}/SLD-c`kcore'-m`nmodules'"
local rseed = 23081980
local prob = 1

*create instance to run RCS simulations
capture classutil drop .r
.r = .RCS.new
*.r.test
.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff')
.r.mask , nsim(1)
.r.estimate , lmethod("avg med tobit reg mi_ce mi_regl mi_2ce mi_2cel mi_reg")
.r.collate
.r.analyze

*capture classutil drop .r
*.r = .RCS.new
*.r.run using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') nsim(1) nmi(10) lmethod("avg med tobit reg mi_ce mi_regl mi_2ce mi_2cel mi_reg")

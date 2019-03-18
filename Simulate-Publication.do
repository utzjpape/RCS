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

quiet {
	include "${gsdDo}/fRCS.do"
	include "${gsdDo}/fRCS_estimate_.do"
	include "${gsdDo}/fRCS_estimate_mi_.do"
}

*RCS_describe using "`using'", dirbase("`dirbase'") 

*RCS_run using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'") rseed(`rseed')
RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff')
RCS_mask using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed') p(`prob')
RCS_estimate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'") rseed(`rseed')
RCS_collate using "`using'", dirbase("`dirbase'") nsim(`nsim') nmi(`nmi') lmethod("`lmethod'")
RCS_analyze using "`using'", dirbase("`dirbase'") lmethod("`lmethod'")

*run simulation with zero core items
*number of modules
local nmodules = 2
local nsim = 10
local nmi = 50
*methods
local lmethod = "med avg reg tobit mi_ce mi_reg mi_regl"
local using= "${gsdData}/SLD`bH'-HHData.dta"
local shares = "demo"
local ncoref = 5
local ncorenf = 5
local ndiff=3
local povline = `xpovline`bH'' 
local lmethod = "`lmethod'"
local model = "hhsize pchild bwork i.hhsex i.hhwater hhcook_5 i.hhtoilet i.hhmaterial i.hhfood urban"
local dirbase = "${gsdOutput}/SLD`bH'-c`ncoref'-`ncorenf'-m`nmodules'-`shares'"
local rseed = 23081980
local prob = 1

include "${gsdDo}/fRCS.do"
include "${gsdDo}/fRCS_estimate_.do"
include "${gsdDo}/fRCS_estimate_mi_.do"

RCS_describe using "`using'", dirbase("`dirbase'") 

*RCS_run using "${gsdTemp}/HHData.dta", dirout("${gsdOutput}/SOM-d`ndiff'm`M'") nmodules(`M') ncoref(33) ncorenf(25) ndiff(`ndiff') nsim(`N') nmi(`nI') lmethod("`lmethod'") povline(`povline') model("`model'") rseed(`rseed')
RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') shares(`shares')
RCS_mask using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed') p(`prob')
RCS_estimate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'") rseed(`rseed')
RCS_collate using "`using'", dirbase("`dirbase'") nsim(`nsim') nmi(`nmi') lmethod("`lmethod'")
RCS_analyze using "`using'", dirbase("`dirbase'") lmethod("`lmethod'") povline(`povline')


*next steps:
*implement with truncated regression if that works
*reduce core significantly (to maybe 1 item - though then the quartile is not helpful anymore); distribute items randomly

*runs the ritem simluation for MI; make sure files are prepared using S1_simulation-SOM

clear all
ma drop all
set more off

*define parameters
local using= "${gsdData}/SOM-HHData.dta"
local nmodules = 1
local ncoref = 33
local ncorenf = 25
local ndiff=3
local nsim =10
local nmi = 50
local povline = `xpovline'
local lmethod = "ritem_mi"
local model = "hhsize pchild bwork i.hhsex i.hhwater hhcook_5 i.hhtoilet i.hhmaterial i.hhfood"
local rseed = 23081980
local dirbase = "${gsdOutput}/SOM-ritem_test"
local prob = .9

include "${gsdDo}/fRCS.do"
RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff')

*as loop
local lp = "10 5 4 3 2"
foreach prob of local lp {
	local dirbase = "${gsdOutput}/SOM-ritem_`prob'"
	RCS_mask using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed') p(`prob')
	RCS_estimate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'") rseed(`rseed')
	RCS_collate using "`using'", dirbase("`dirbase'") nsim(`nsim') nmi(`nmi') lmethod("`lmethod'")
	RCS_analyze using "`using'", dirbase("`dirbase'") lmethod("`lmethod'") povline(`povline')
}

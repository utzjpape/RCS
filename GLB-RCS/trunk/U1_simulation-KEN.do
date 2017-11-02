*SIMULATE RCS FOR KENYA

clear all
ma drop all
set more off

*parameters
*data directory
local sData = "${gsdDataBox}/KEN-KIHBS2005"

include "${gsdDo}/fRCS.do"

*CREATE MODULES 
*for validation of the method, missing data is assumed to be missing
*as the consumption aggregate implicitly assumes.
*food consumption
use "`sData'/section I Weekly Expenditure.dta", clear
*link deflator
merge m:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", keep(master match) keepusing(fpindex)
*prepare id
gen hhid = id_clust * 10000 + id_hh
ren i02 foodid
drop if foodid>2000
egen xfood = rowtotal(i03k i04k i05k i05ak i06k i07k)
keep hhid foodid xfood fpindex
collapse (sum) xfood, by(hhid foodid fpindex)
replace xfood = xfood / fpindex if xfood<.
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "${gsdTemp}/KEN-HH-FoodItems.dta", replace
*non food consumption
use "`sData'/Section JKL Regular Non Food Items.dta", clear
*link deflator
merge m:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", keep(master match) keepusing(fpindex)
*prepare id
gen hhid = id_clust * 10000 + id_hh
ren j02 nonfoodid
gen xnonfood = j03k + j04k
keep hhid nonfoodid xnonfood fpindex
collapse (sum) xnonfood, by(hhid nonfoodid fpindex)
replace xnonfood = xnonfood / fpindex if xnonfood<.
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "${gsdTemp}/KEN-HH-NonFoodItems.dta", replace

*get confidence interval for poverty
use "`sData'/consumption_aggregated_data.dta", clear
svyset id_clu [pweight=wta_pop]
gen poor = y2_i < z2_i 
svy: mean poor

*get household characteristics
use "`sData'/Section_B_Household_member_Information.dta", clear
gen hhsex = b04 if b03==1
ren b05a age
keep hhsex age id_*
gen age_child = age<15 if age<.
gen age_adult = inrange(age,15,64) if age<.
gen age_senior = age>64 if age<.
collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior (firstnm) hhsex, by(id_clu id_hh)
merge 1:1 id_clu id_hh using "`sData'/Section_G_Housing.dta", nogen assert(match master) keep(match) keepusing(g01 g08a g09a g11 g16 g18 weights)
ren (g01 g08a g09a g11 g16 g18) (hhtenure hhunits hhrooms hhhouse hhcook hhtoilet)
ren weights weight
*simplify by setting missing values to conservative answers
gen hhid = id_clust * 10000 + id_hh
recode hhhouse (99=7) (.=7)
recode hhtoilet (99=2) (.=2)
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
*add durables
merge 1:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", nogen keep(match) keepusing(nfdusevl fpindex)
*remainder of consumption aggregate
gen xdurables = nfdusevl / fpindex
drop nfdusevl fpindex
*add food and non-food
merge 1:1 hhid using "${gsdTemp}/KEN-HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "${gsdTemp}/KEN-HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
ren id_clu cluster
save "${gsdData}/KEN-HHData.dta", replace

*check whether we can reconstruct the consumption aggregate at the item level
use "${gsdData}/KEN-HHData.dta", clear
ren cluster id_clust
merge 1:1 id_clust id_hh using "`sData'/consumption_aggregated_data.dta", nogen keep(match) assert(match using) keepusing(y2_i)
egen ctf = rowtotal(xfood*)
egen ctnf = rowtotal(xnonfood*)
gen ct_pc = (ctf+ctnf) / hhsize + xdurables_pc
assert (round(ct_pc-rpce)==0)
gen poor = rpce < `xpovline'
mean poor [pweight=weight*hhsize]
mean poor [pweight=weight*hhsize], over(urban)

*start RCS code
*run simulation
*number of modules
local nmodules = 4
*number of simulations
local nsim = 20
*number of imputations 
local nmi = 50
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3
*methods
local lmethod = "med avg reg tobit MICE MImvn"
*other parameters
local using= "${gsdData}/KEN-HHData.dta"
local dirbase = "${gsdOutput}/KEN-d`ndiff'm`nmodules'"
local ncoref = 30
local ncorenf = 25
local ndiff=`ndiff'
local povline = `xpovline'
local lmethod = "`lmethod'"
local model = "hhsize pchild bwork i.hhsex i.hhwater hhcook_5 i.hhtoilet i.hhmaterial i.hhfood"

include "${gsdDo}/fRCS.do"
*RCS_run using "`lc_sdTemp'/HHData.dta", dirbase("${l_sdOut}") nmodules(`M') ncoref(33) ncorenf(25) ndiff(`ndiff') nsim(`N') nmi(`nI') lmethod("`lmethod'") povline(`povline') model("`model'") egalshare
RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') egalshare
RCS_assign using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim')
RCS_simulate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'")
RCS_collate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'")
RCS_analyze using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff') lmethod("`lmethod'") povline(`povline')

*subrun
include "${gsdDo}/fRCS.do"
RCS_simulate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("tobit") model("`model'")
RCS_collate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("tobit")
RCS_analyze using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff') lmethod("`lmethod'") povline(`povline')

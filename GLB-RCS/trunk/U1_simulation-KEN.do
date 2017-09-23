*SIMULATE PARTIAL SURVEYS FOR HERGAISA

clear all
ma drop all
set more off

*parameters
*number of modules
local M = 4
*number of simulations
local N = 20
*number of imputations 
local nI = 50
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*methods
local lmethod = "MICE MImvn"


*data directory
local sData = "${g_sdData}/KEN/KIHBS2005"

include "${l_sdDo}/fRCS.do"
local lc_sdBase = "${l_sdOut}/KEN/d`ndiff'm`M'"
capture: mkdir "`lc_sdBase'"
local lc_sdTemp = "`lc_sdBase'/Temp"
capture: mkdir "`lc_sdTemp'"
local lc_sdOut = "`lc_sdBase'/Out"
capture: mkdir "`lc_sdOut'"


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
save "`lc_sdTemp'/HH-FoodItems.dta", replace
*non food consumption
use "`sData'/Section JKL Regular Non Food Items.dta", clear
*link deflator
merge m:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", keep(master match) keepusing(fpindex)
*prepare id
gen hhid = id_clust * 10000 + id_hh
ren j02 nonfoodid
gen xnonfood = j03k + j04k
keep hhid nonfoodid xnonfood
collapse (sum) xnonfood, by(hhid nonfoodid)
replace xnonfood = xnonfood / fpindex if xnonfood<.
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "`lc_sdTemp'/HH-NonFoodItems.dta", replace

*get confidence interval for poverty
use "`sData'/consumption_aggregated_data.dta", clear
svyset id_clu [pweight=wta_pop]
gen poor = y2_i < z2_i 
mean poor

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
ren id_clu cluster
recode hhhouse (99=7) (.=7)
recode hhwater (99=8) (.=8) (5=8)
recode hhtoilet (99=2) (.=2) (4=3)
recode hhsleep (99=2) (.=2)
recode hhmaterial (99=5) (.=5) (4=5)
recode hhmode (5=4) (99=4) (.=4) (3=4)
recode hhfood (99=2) (.=4)
drop if strata ~= 1
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
gen pwork = nwork / hhsize
gen bwork = nwork>0
*add durables
merge 1:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", nogen keep(match) keepusing(nfdusevl fpindex)
*remainder of consumption aggregate
gen xdurables = nfdusevl / fpindex
drop nfdusevl fpindex
*add food and non-food
merge 1:1 hhid using "`lc_sdTemp'/HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "`lc_sdTemp'/HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
save "`lc_sdTemp'/HHData.dta", replace

*start RCS code
*run simulation
local using= "`lc_sdTemp'/HHData.dta"
local dirout = "${l_sdOut}/KEN"
local nmodules = `M'
local ncoref = 30
local ncorenf = 25
local ndiff=`ndiff'
local nsim = `N'
local nmi = `nI'
local povline = `xpovline'
local lmethod = "`lmethod'"
local model = "hhsize pchild bwork i.hhsex i.hhwater hhcook_5 i.hhtoilet i.hhmaterial i.hhfood"

include "${l_sdDo}/fRCS.do"
*RCS_run using "`lc_sdTemp'/HHData.dta", dirout("${l_sdOut}") nmodules(`M') ncoref(33) ncorenf(25) ndiff(`ndiff') nsim(`N') nmi(`nI') lmethod("`lmethod'") povline(`povline') model("`model'") egalshare
RCS_prepare using "`using'", dirout("`dirout'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff') egalshare
RCS_assign using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim')
RCS_simulate using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'")
RCS_collate using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'")
RCS_analyze using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') lmethod("`lmethod'") povline(`povline')

*subrun
include "${l_sdDo}/fRCS.do"
RCS_simulate using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("tobit") model("`model'")
RCS_collate using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("tobit")
RCS_analyze using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') lmethod("`lmethod'") povline(`povline')

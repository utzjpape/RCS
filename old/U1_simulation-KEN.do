*SIMULATE PARTIAL SURVEYS FOR HERGAISA

clear all
ma drop all
*set maxvar 20000
set maxvar 32767
set more off

*parameters
*number of modules
local M = 4
*number of simulations
local N = 100
*number of imputations 
local nI = 50
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*methods
*local lmethod = "MICE MImvn"
*local lmethod = "reg"
local lmethod = "med avg reg reg2 reg3 tobit tobit2 tobit3 MICE MImvn"

*data directory
local sData = "${gsdDataBox}/KEN-KIHBS2005"


/*. table rururb, c(mean z2_i)
----------------------
Area of   |
residence | mean(z2_i)
----------+-----------
    Rural |   1562.179
    Urban |   2912.798
----------------------

. table rururb [aw=wta_pop], c(mean fpindex)

-------------------------
Area of   |
residence | mean(fpindex)
----------+--------------
    Rural |     1.0138913
    Urban |     1.0317782
-------------------------

 su z2_i [aw=wta_pop] 

    Variable |     Obs      Weight        Mean   Std. Dev.       Min        Max
-------------+-----------------------------------------------------------------
        z2_i |  13,158  35514542.3    1834.139   541.6406   1562.179   2912.798
*/
local xpovline = 1834.139 // This is not the correct way, but let's have this tentatively. 
local deflator_r = 1.0138913 // Copied from above
local deflator_u = 1.0317782 // Copied from above

include "${gsdDo}/fRCS.do"
*include "${gsdDo}/fRCS_KEN.do"

local lc_sdBase = "${gsdOutput}/KEN/d`ndiff'm`M'"
capture: mkdir "${gsdOutput}/KEN"
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
merge m:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", keep(master match) keepusing(fpindex rururb)
*prepare id
gen hhid = id_clust * 10000 + id_hh
ren i02 foodid
drop if foodid>2000
egen xfood = rowtotal(i03k i04k i05k i05ak i06k i07k)
keep hhid foodid xfood fpindex rururb
collapse (sum) xfood (mean)rururb, by(hhid foodid fpindex)
replace xfood = xfood / fpindex if xfood<.
replace xfood = xfood / `deflator_r' if xfood<. & rururb==1 // This makes the poverty line unique
replace xfood = xfood / `deflator_u' if xfood<. & rururb==2 // This makes the poverty line unique
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "`lc_sdTemp'/HH-FoodItems.dta", replace
*non food consumption
use "`sData'/Section JKL Regular Non Food Items.dta", clear
*link deflator
merge m:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", keep(master match) keepusing(fpindex rururb)
*prepare id
gen hhid = id_clust * 10000 + id_hh
ren j02 nonfoodid
gen xnonfood = j03k + j04k
keep hhid nonfoodid xnonfood fpindex rururb
collapse (sum) xnonfood (mean) rururb, by(hhid nonfoodid fpindex)
replace xnonfood = xnonfood / fpindex if xnonfood<. // (ST) I need to check fpindex is right or not. 
replace xnonfood = xnonfood / `deflator_r' if xnonfood<. & rururb==1 // This makes the poverty line unique
replace xnonfood = xnonfood / `deflator_u' if xnonfood<. & rururb==2 // This makes the poverty line unique
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
*ren id_clu cluster
recode hhhouse (99=7) (.=7)
*recode hhwater (99=8) (.=8) (5=8) // (ST) muted since this var is missing. 
recode hhtoilet (99=2) (.=2) (4=3)
*recode hhsleep (99=2) (.=2)  // (ST) muted since this var is missing. 
*recode hhmaterial (99=5) (.=5) (4=5) // (ST) muted since this var is missing.
*recode hhmode (5=4) (99=4) (.=4) (3=4) // (ST) muted since this var is missing.
*recode hhfood (99=2) (.=4)  // (ST) muted since this var is missing.
*drop if strata ~= 1 // (ST) Muted
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
*gen pwork = nwork / hhsize // (ST) Muted
*gen bwork = nwork>0 // (ST) Muted
*add durables
merge 1:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", nogen keep(match) keepusing(nfdusevl fpindex)
*remainder of consumption aggregate
gen xdurables = nfdusevl / fpindex
drop nfdusevl fpindex
*add food and non-food
merge 1:1 hhid using "`lc_sdTemp'/HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "`lc_sdTemp'/HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)

rename id_clust cluster
gen xdurables_pc=0
save "`lc_sdTemp'/HHData.dta", replace

*start RCS code
*run simulation
local using= "`lc_sdTemp'/HHData.dta"
local dirout = "${gsdOutput}/KEN"
local nmodules = `M'
local ncoref = 30
local ncorenf = 25
local ndiff=`ndiff'
local nsim = `N'
local nmi = `nI'
local povline = `xpovline'
local lmethod = "`lmethod'"
local model = "hhsize pchild i.hhsex i.hhtoilet hhrooms"
local model2 = "hhsize pchild i.hhsex"
local model3 = " "
*candidate: hhcook hhhouse
*local model = "hhsize pchild bwork i.hhsex hhcook_5 i.hhtoilet i.hhmaterial i.hhfood"
local rseed = 23081980

*RCS_run using "`lc_sdTemp'/HHData.dta", dirout("${l_sdOut}") nmodules(`M') ncoref(33) ncorenf(25) ndiff(`ndiff') nsim(`N') nmi(`nI') lmethod("`lmethod'") povline(`povline') model("`model'") rseed(`rseed')
local dirbase = "`dirout'/d`ndiff'm`nmodules'"

RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff')
RCS_assign using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed')
/*
RCS_simulate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'") model2("`model2'") model3("`model3'") rseed(`rseed')
*RCS_collate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") // (ST) dropped unnecessary options
RCS_collate using "`using'", dirbase("`dirbase'") nsim(`nsim') nmi(`nmi') lmethod("`lmethod'")
RCS_analyze using "`using'", dirbase("`dirbase'") lmethod("`lmethod'") povline(`povline')

/* Not completed below since "subrun" is the same as above. Revise this. (ST)
*subrun
include "${gsdDo}/fRCS.do"
RCS_simulate using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("tobit") model("`model'")
RCS_collate using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') nsim(`nsim') nmi(`nmi') lmethod("tobit")
RCS_analyze using "`using'", dirout("`dirout'") nmodules(`nmodules') ndiff(`ndiff') lmethod("`lmethod'") povline(`povline')

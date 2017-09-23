clear all
ma drop all
set more off

*parameters
*number of modules
local M = 4
*number of simulations
local N = 10
*number of imputations 
local nI = 20
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*methods
local lmethod = "reg MIchain MICE MImvn"


*data directory
local sData = "${g_sdData}/SSD/NBHS2009"

*deflator to divide nominal expenditures by and poverty line for urban Hargeiza
local xdeflator = 1.094426
local xpovline = 184.1037

include "${l_sdDo}/fRCS.do"
local lc_sdBase = "${l_sdOut}/SSD/d`ndiff'm`M'"
capture: mkdir "`lc_sdBase'"
local lc_sdTemp = "`lc_sdBase'/Temp"
capture: mkdir "`lc_sdTemp'"
local lc_sdOut = "`lc_sdBase'/Out"
capture: mkdir "`lc_sdOut'"


*CREATE MODULES 
*for validation of the method, missing data is assumed to be zero
*as the consumption aggregate implicitly assumes.
*food consumption
use "`sData'/NBHS_FOOD.dta", clear
ren (item value) (foodid xfood)
include "${l_sdDo}/SSD-labels.do"
label values foodid lfoodid
keep hhid foodid xfood
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "`lc_sdTemp'/HH-FoodItems.dta", replace
*non food consumption
use "`sData'/NBHS_NONFOOD.dta", clear
ren (item q3) (nonfoodid xnonfood)
replace nonfoodid = 83003 if nonfoodid==830031
include "${l_sdDo}/SSD-labels.do"
label values nonfoodid lnonfoodid
*module=5 implies 12m recall; module=4 is a 30d recall
replace xnonfood = xnonfood / 12 if module==5
*ATTENTION: Previous code removed households with missing non-food consumption
*as it is unlikely for aperiod of 12m.
*drop if nonfoodid>=.
replace nonfoodid = 11101 if nonfoodid>=.
keep hhid nonfoodid xnonfood
collapse (sum) xnonfood, by(hhid nonfoodid)
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "`lc_sdTemp'/HH-NonFoodItems.dta", replace

*get household characteristics
use "`sData'/NBHS_IND.dta", clear
ren b41 age
gen age_child = age<15 if age<.
gen age_adult = inrange(age,15,64) if age<.
gen age_senior = age>64 if age<.
collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior, by(hhid cluster)
merge 1:1 hhid cluster using "`sData'/NBHS_HH.dta", nogen assert(match) keep(match) keepusing(h1 h5 h9 h3 h7 h8 h10 i* head_sex head_age urban hhweight) 
ren (h1 h5 h9 h3 h7 h8 h10 hhweight) (hhhouse hhwater hhtoilet hhsleep hhlight hhcook hhwaste weight)
ren (head_sex head_age urban) (hhsex age strata)
replace hhsex =1 if hhsex>=.
*collect durables but we won't use them for the moment
local li = "21 22 23 24 25 31 32 33 34 35 36 37 38 39"
gen xdurables = 0
foreach i of local li {
	replace xdurables = xdurables + i`i'_2 * i`i'_3 if i`i'_1==1 & (i`i'_2 * i`i'_3>0)
}
gen xdurables_pc = 0
drop i*
*simplify by setting missing values to conservative answers
*type
recode hhhouse (1/2=1) (3/4=2) (5/20=3) (11=4) (-9=4) (.=4)
label define lhouse 1 "Tent" 2 "Tukul" 3 "House/Apt" 4 "Other", replace
label values hhhouse lhouse
*sleep
recode hhsleep (3/12=3) (-9=0)
label define lsleep 0 "None" 1 "1 Room" 2 "2 Rooms" 3 ">2 Rooms", replace
label values hhsleep lsleep
*water
recode hhwater (1/4=1) (5=2) (6/11=3) (11/12=4) (-9=4)
label define lwater 1 "Borehole" 2 "Hand pump" 3 "Open Water" 4 "Other"
label values hhwater lwater
*light
recode hhlight (1/5=1) (6/10=2) (11=3) (-9=3)
label define llight 1 "Gas / Paraffin" 2 "Other material" 3 "None", replace
label values hhlight llight
*cook
recode hhcook (-9=3) (3/9=3)
label define lcook 1 "Firewood" 2 "Charcoal" 3 "Other", replace
label values hhcook lcook
*toilet
recode hhtoilet (3/5=3) (6=4) (-9=4)
label define ltoilet 1 "Pit" 2 "Shared Pit" 3 "Flush/Bucket" 4 "None", replace
label values hhtoilet ltoilet
*waste
recode hhwaste (-9/2=4) (3=3) (4=2) (5=1) (6=4)
label define lwaste 1 "Burning" 2 "Heap" 3 "Pit" 4 "Other"
label values hhwaste lwaste
*add durables and food and non-food
merge 1:1 hhid using "`lc_sdTemp'/HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "`lc_sdTemp'/HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
save "`lc_sdTemp'/HHData.dta", replace

*start RCS code
*run simulation
local using= "`lc_sdTemp'/HHData.dta"
local dirout = "${l_sdOut}/SSD"
local nmodules = `M'
local ncoref = 33
local ncorenf = 25
local ndiff=`ndiff'
local nsim = `N'
local nmi = `nI'
local povline = `xpovline'
local lmethod = "reg MICE MImvn"
local model = "hhsize pchild psenior i.hhsex i.hhwater i.hhcook hhsleep i.hhhouse i.hhtoilet i.hhwaste"

include "${l_sdDo}/fRCS.do"
*RCS_run using "`lc_sdTemp'/HHData.dta", dirout("${l_sdOut}") nmodules(`M') ncoref(33) ncorenf(25) ndiff(`ndiff') nsim(`N') nmi(`nI') lmethod("`lmethod'") povline(`povline') model("`model'")
RCS_prepare using "`using'", dirout("`dirout'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff')

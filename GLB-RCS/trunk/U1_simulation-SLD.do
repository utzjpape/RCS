*SIMULATE FOR SOMALILAND

clear all
ma drop all
set more off

*parameters
*data directory
local sData = "${gsdDataBox}/SOM-SLHS13"

*leave blank for Somaliland and add suffix for Hergeize like _Hergeiza
local bH = "_Hergeiza"
local bH = ""

*DEFLATOR to divide nominal expenditures by and poverty line for urban Hargeiza
*in 2011, $1 USD PPP was worth 10,731 Somali Shillings PPP, & general inflation in Somaliland from 2011 to 2013 was 58.4%
*so 16,996.43 Somali Shillings PPP (2013 Somaliland prices) could buy $1 USD PPP (2011)
*thus $1.90 USD PPP 2011 corresponds to 32,293.22 Somali Shillings PPP
*then we convert to USD using an average exchange rate of 20,360.53 Somali Shillings per USD in 2013, that is $1.5861 USD PPP (2013 Somaliland prices)
*to finally convert to Somaliland Shillings using an average exchange rate of 6,733.69 Somaliland Shillings per USD in 2013, which gives us a poverty line of 10,680.11 Somaliland Shillings PPP (2013 Somaliland prices) per person per day, equivalent to $1.90 USD PPP (2011) 
local xpovline = 10680.1112312 * .9387317 / (1000 * 12 / 365)
*for Hergeiza calculation, we use the zupper national poverty line (not used)
*local xpovline_Hergeiza = "207.2878"

include "${gsdDo}/fRCS.do"
include "${gsdDo}/fRCS_estimate_.do"
include "${gsdDo}/fRCS_estimate_mi_.do"

*CREATE MODULES 
*for validation of the method, missing data is assumed to be missing
*as the consumption aggregate implicitly assumes.
*food consumption
use "`sData'/food_consumption_clean.dta", clear
merge m:1 hhid using "`sData'/wfilez.dta", nogen keep(match) assert(match) keepusing(paasche)
replace xfood = xfood / paasche if xfood<.
keep hhid foodid xfood
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "${gsdTemp}/SLD-HH-FoodItems.dta", replace
*non food consumption
use "`sData'/non_food_clean.dta", clear
merge m:1 hhid using "`sData'/wfilez.dta", nogen keep(match) assert(match) keepusing(paasche)
replace xnonfood = xnonfood / paasche if xnonfood<.
keep hhid nonfoodid xnonfood
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "${gsdTemp}/SLD-HH-NonFoodItems.dta", replace

*get confidence interval for poverty
*USE rpce 
*IPL FGT0 should be rural 69 (2013) to 64 (2016) and urban 57 (2013) to 52 (2016)
* with Hergeiza being 57 (2013)
use "`sData'/wfilez.dta", clear
*check if Hergeiza only
if ("`bH'"!="") {
	drop if strata !=1
}
svyset cluster [pweight=weight]
gen poor = rpce < `xpovline'
mean poor [pweight=weight*hsize], over(urban)
*graph food share
gen x = rfood_pc + rnonfood_pc
gen ratio = rfood_pc / x
egen r = rank(x)
sort x
*twoway (scatter ratio r) (qfit ratio r), title("Somaliland")
*graph export "${gsdOutput}\SLD_fshare.png", as(png) replace

*get household characteristics
use "`sData'/data_i_proc_public.dta", clear
gen hhsex = S3_A04 if S3_A05==1
ren S3_A06_1 age
gen work = S3_AE07==1
keep hhid hhsex age work strata cluster weight
gen age_child = age<15 if age<.
gen age_adult = inrange(age,15,64) if age<.
gen age_senior = age>64 if age<.
collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior nwork=work (firstnm) hhsex, by(hhid strata cluster weight)
merge 1:1 hhid using "`sData'/data_h_proc_public.dta", nogen assert(match) keep(match) keepusing(S13_G01 S13_G03A S13_G04_* S13_G05 S13_G07 S13_G10 S13_G15 S13_G24 S13_G26)
ren (S13_G01 S13_G03A S13_G05 S13_G07 S13_G10 S13_G15 S13_G24 S13_G26) (hhhouse hhwater hhtoilet hhmaterial hhmode hhplot hhfood hhsleep)
ren S13_G04_* hhcook_*
drop hhcook_7 hhcook_99 hhcook_4
*simplify by setting missing values to conservative answers
recode hhhouse (99=7) (.=7)
recode hhwater (99=8) (.=8) (5=8)
recode hhtoilet (99=2) (.=2) (4=3)
recode hhsleep (99=2) (.=2)
recode hhmaterial (99=5) (.=5) (4=5)
recode hhmode (5=4) (99=4) (.=4) (3=4)
recode hhfood (99=2) (.=4)
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
gen pwork = nwork / hhsize
gen bwork = nwork>0
*add durables and food and non-food
merge 1:1 hhid using "`sData'/wfilez.dta", nogen keep(match) keepusing(rdurables_pc urban)
ren rdurables_pc xdurables_pc
merge 1:1 hhid using "${gsdTemp}/SLD-HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "${gsdTemp}/SLD-HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
*check if Hergeiza only
if ("`bH'"!="") {
	drop if strata !=1
}
*remove a few records (e.g. without consumption)
drop if missing(hhcook_1)
save "${gsdData}/SLD`bH'-HHData.dta", replace

*check whether we can reconstruct the consumption aggregate at the item level
use "${gsdData}/SLD`bH'-HHData.dta", clear
merge 1:1 hhid using "`sData'/wfilez.dta", nogen keep(match) assert(match using) keepusing(rpce pce rfood rfood_pc rnonfood rnonfood_pc)
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
local lmethod = "med avg reg tobit mi_ce"
local using= "${gsdData}/SLD`bH'-HHData.dta"
local ncoref = 33
local ncorenf = 25
local ndiff=`ndiff'
local povline = `xpovline' 
local lmethod = "`lmethod'"
local model = "hhsize pchild bwork i.hhsex i.hhwater hhcook_5 i.hhtoilet i.hhmaterial i.hhfood urban"
local dirbase = "${gsdOutput}/SLD`bH'-d`ndiff'm`nmodules'"
local rseed = 23081980
local prob = 1

include "${gsdDo}/fRCS.do"
include "${gsdDo}/fRCS_estimate_.do"
include "${gsdDo}/fRCS_estimate_mi_.do"

RCS_describe using "`using'", dirbase("`dirbase'") 

*RCS_run using "${gsdTemp}/HHData.dta", dirout("${gsdOutput}/SOM-d`ndiff'm`M'") nmodules(`M') ncoref(33) ncorenf(25) ndiff(`ndiff') nsim(`N') nmi(`nI') lmethod("`lmethod'") povline(`povline') model("`model'") rseed(`rseed')
RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff')
RCS_mask using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed') p(`prob')
RCS_estimate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'") rseed(`rseed')
RCS_collate using "`using'", dirbase("`dirbase'") nsim(`nsim') nmi(`nmi') lmethod("`lmethod'")
RCS_analyze using "`using'", dirbase("`dirbase'") lmethod("`lmethod'") povline(`povline')

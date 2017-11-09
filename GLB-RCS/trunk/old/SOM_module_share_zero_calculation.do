*SIMULATE PARTIAL SURVEYS FOR HERGAISA

clear all
ma drop all
set more off
set matsize 10000

*parameters
*number of modules
local M = 4
*number of simulations
local N = 20
*number of imputations 
local nI = 20
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*country
local co="SOM"
display "`co'"

*methods
local lmethod = "med avg reg reg2 reg3 tobit tobit2 tobit3 twopart twopart2 twopart3 twopartII1 twopartII2 twopartII3 MICE MImvn"
local lmethod="TwoMI TwoMI2 TwoMI3"
local lmethod="TwostMI"
local lmethod="MIreg MItobit MItwopart"
local lmethod = "med avg reg tobit twopart MIreg MItobit MItwopart MICE MImvn TwostMITobit TwostMI"
local lmethod = "avg med reg tobit twopart twopartII MIreg MItobit MItwopart MICE MImvn TwostMITobit TwostMI" // all
*local lmethod = "avg med reg tobit twopart twopartII MIreg MItobit MItwopart MICE MImvn TwostMI" //  TwostMITobit excluded for shortcut
*local lmethod = "MItwopart" // 


*data directory
local sData = "${gsdDataBox}/SOM-SLHS13"

*NOT USED: *deflator to divide nominal expenditures by and poverty line for urban Hargeiza
*NOT USED: local xdeflator = 1.094426

include "${gsdDo}/fRCS_log_sm.do"
local lc_sdBase = "${gsdOutput}/SOM/d`ndiff'm`M'"
capture: mkdir "${gsdOutput}/SOM"
capture: mkdir "`lc_sdBase'"
local lc_sdTemp = "`lc_sdBase'/Temp"
capture: mkdir "`lc_sdTemp'"
local lc_sdOut = "`lc_sdBase'/Out"
capture: mkdir "`lc_sdOut'"


*CREATE MODULES 
*for validation of the method, missing data is assumed to be missing
*as the consumption aggregate implicitly assumes.
*food consumption
use "`sData'/food_consumption_clean.dta", clear
*drop if strata ~= 1
keep hhid foodid xfood
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "`lc_sdTemp'/HH-FoodItems.dta", replace
*non food consumption
use "`sData'/non_food_clean.dta", clear
keep hhid nonfoodid xnonfood
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood)
save "`lc_sdTemp'/HH-NonFoodItems.dta", replace

*get confidence interval for poverty
use "`sData'/wfilez.dta", clear
*drop if strata ~= 1
svyset cluster [pweight=weight]
gen poor = rpce < zupper
mean poor
*graph food share
gen x = rfood_pc + rnonfood_pc
gen ratio = rfood_pc / x
egen r = rank(x)
sort x
twoway (scatter ratio r) (qfit ratio r), title("Hergeiza")
graph export "`lc_sdOut'\Hergeiza_fshare.png", as(png) replace

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
*recode hhwater (99=8) (.=8) (5=8)
recode hhwater (99=8) (.=8) (5=8) (7=8) (3=1) //ST modified
label define G03A 1 "Public water", modify
recode hhtoilet (99=2) (.=2) (4=3)
recode hhsleep (99=2) (.=2)
recode hhmaterial (99=5) (.=5) (4=5)
recode hhmode (5=4) (99=4) (.=4) (3=4)
recode hhfood (99=2) (.=2)
*drop if strata ~= 1
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
gen pwork = nwork / hhsize
gen bwork = nwork>0
*add durables and food and non-food
merge 1:1 hhid using "`sData'/wfilez.dta", nogen keep(match) keepusing(xdurables_pc district urban)
merge 1:1 hhid using "`lc_sdTemp'/HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "`lc_sdTemp'/HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
merge m:1 district urban using "`sData'/paasche.dta", nogen
rename paaschemed deflator
gen povline=207.3 if urban==1 // (Upper pline) Taken from Table 13 on p.128 in the new report
replace povline=180.9 if urban==0 // (Upper pline) Taken from Table 13 on p.128 in the new report
egen totnfood=rowtotal(xnonfood*)
drop if totnfood==0 // 4 HHs with nonfood consumption=0 will be dropped. 
drop totnfood
drop if xdurables_pc==.
save "`lc_sdTemp'/HHData.dta", replace

*start RCS code
*run simulation
local using= "`lc_sdTemp'/HHData.dta"
local dirout = "${gsdOutput}/SOM"
local nmodules = `M'
local ncoref = 33
local ncorenf = 25
local ndiff=`ndiff'
local nsim = `N'
local nmi = `nI'
*local povline = `xpovline'
local lmethod = "`lmethod'"
*local model = "hhsize pchild bwork i.hhsex i.hhwater hhcook_5 i.hhtoilet i.hhmaterial i.hhfood" (ST)

local model = "hhsize pchild bwork i.hhsex i.hhwater hhcook_5 i.hhtoilet io5.hhmaterial i.hhfood"
local model = "hhsize pchild bwork i.hhsex i.hhwater hhcook_5 i.hhtoilet i.hhmaterial i.hhfood"
local model2 = "hhsize pchild bwork i.hhsex"
local model3 = " "
local rseed = 23081980

*RCS_run using "`lc_sdTemp'/HHData.dta", dirout("${l_sdOut}") nmodules(`M') ncoref(33) ncorenf(25) ndiff(`ndiff') nsim(`N') nmi(`nI') lmethod("`lmethod'") povline(`povline') model("`model'") rseed(`rseed')
local dirbase = "`dirout'/d`ndiff'm`nmodules'"

local ST=5
*local EN=60
local EN=80
*Number of items for food and non-food:
local num_f=105
local num_nf=85

forvalue csim = `ST'(5)`EN'{
display "Simulation=" as text `csim' as result "%" as text
local fcsim= ceil(`num_f'*`csim'*0.01)
local nfcsim= ceil(`num_nf'*`csim'*0.01)
display "N of food items in core-module=" as text `fcsim' as result
display "N of non-food items in core-module=" as text `nfcsim' as result
*methods
local lmethod = "sim`csim'per"

local nsim=1

RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`fcsim') ncorenf(`nfcsim') ndiff(`ndiff')
RCS_assign using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed')

use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4\Temp\mi_1.dta" ,clear
foreach var of var xfcons*_pc xnfcons*_pc {
gen d`var'=(`var'>0)
replace d`var'=. if `var'==.
}
forvalue i=1/4{
su dxfcons`i'_pc
gen m_dxfcons`i'_pc=r(mean)
}
forvalue i=1/4{
su dxnfcons`i'_pc
gen m_dxnfcons`i'_pc=r(mean)
}
egen m_dxfcons_pc=rowmean(m_dxfcons*_pc)
egen m_dxnfcons_pc=rowmean(m_dxnfcons*_pc)
gen per=`csim'
gen fcsim=`fcsim'
gen nfcsim=`nfcsim'
keep per fcsim nfcsim m_dxfcons_pc m_dxnfcons_pc m_dxfcons*_pc m_dxnfcons*_pc
order per fcsim nfcsim m_dxfcons_pc m_dxnfcons_pc m_dxfcons*_pc m_dxnfcons*_pc
tempfile f`csim'
keep if _n==1
save `f`csim''
}

use `f5',clear
forvalue i=10(5)60{
append using `f`i''
}
save "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\modsim\d3m4\Out\d_freq_SOM.dta" ,replace
export excel using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\modsim\d3m4\Out\d_freq_SOM.xls", replace first(var) 

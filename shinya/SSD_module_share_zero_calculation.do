***************************************************************
*SSD Simulation
** Note 1: We use the deflator only for food items by urban/rural. 
** Note 2: The poverty lines are separately defined for urban and rural areas.
** Note 3: 115 households do not have non-food item consumption and these households are excluded.
***************************************************************


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
local co="SSD"

*methods
local lmethod = "med avg reg reg2 reg3 tobit tobit2 tobit3 twopart twopart2 twopart3 twopartII1 twopartII2 twopartII3 MICE MImvn"
local lmethod = "TwostMI"
local lmethod = "TwostMITobit"
local lmethod = "MIreg MItobit MItwopart"
local lmethod = "med avg reg tobit twopart MIreg MItwopart MICE MImvn TwostMI"
local lmethod = "MIreg MItwopart"
local lmethod = "med avg reg tobit twopart MIreg MItobit MItwopart MICE MImvn TwostMI"
local lmethod = "avg med reg tobit twopart twopartII MIreg MItobit MItwopart MICE MImvn TwostMITobit TwostMI"
local lmethod = "avg med reg tobit twopart twopartII MIreg MItobit MItwopart MICE MImvn TwostMITobit TwostMI" // All


*MItobit TwostMITobit

*data directory
local sData = "${gsdDataBox}/SSD-NBHS2009"

*deflators
local deflator_u = 1.029 // This deflator is obtained using the data to replicate the official poverty rate since we do not have the deflator in the report.
local deflator_r = .915   // This deflator is obtained using the data to replicate the official poverty rate since we do not have the deflator in the report.
	
*poverty line ($1.90)
local xpovline = 76.40 // $1.90 line in LCU

include "${gsdDo}/fRCS.do"

*Make temp directories
local lc_sdBase = "${gsdOutput}/SSD/d`ndiff'm`M'"
capture: mkdir "${gsdOutput}/SSD"
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
include "${gsdDo}/SSD-labels.do"
label values foodid lfoodid
replace xfood=xfood*4 // The data is weekly. 
keep hhid foodid xfood
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood)
save "`lc_sdTemp'/HH-FoodItems.dta", replace
*non food consumption
use "`sData'/NBHS_NONFOOD.dta", clear
ren (item q3) (nonfoodid xnonfood)
replace nonfoodid = 83003 if nonfoodid==830031
include "${gsdDo}/SSD-labels.do"
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
*ren (head_sex head_age urban) (hhsex age strata) 
ren (head_sex head_age) (hhsex age)
replace hhsex =1 if hhsex>=.
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
*gen pwork = nwork / hhsize
*gen bwork = nwork>0
*collect durables but we won't use them as is for the poverty assessment. 
/*
local li = "21 22 23 24 25 31 32 33 34 35 36 37 38 39"
gen xdurables = 0
foreach i of local li {
	replace xdurables = xdurables + i`i'_2 * i`i'_3 if i`i'_1==1 & (i`i'_2 * i`i'_3>0)
}
*/
gen xdurables_pc = 0
drop i*
*simplify by setting missing values to conservative answers
*type
*recode hhhouse (1/2=1) (3/4=2) (5/20=3) (11=4) (-9=4) (.=4)
*label define lhouse 1 "Tent" 2 "Tukul" 3 "House/Apt" 4 "Other", replace // To avid too few cases in 4
recode hhhouse (1/2=3) (3/4=1) (5/20=2) (11=3) (-9=3) (.=3)
label define lhouse 1 "Tukul" 2 "House/Apt" 3 "Tent/Other", replace
label values hhhouse lhouse
*sleep
recode hhsleep (3/12=3) (-9=0)
label define lsleep 0 "None" 1 "1 Room" 2 "2 Rooms" 3 ">2 Rooms", replace
label values hhsleep lsleep
*water
*recode hhwater (1/4=1) (5=2) (6/11=3) (11/12=4) (-9=4)
recode hhwater (1/4=1) (5=2) (6/11=3) (12/13=4) (-9=4) // ST fixed typo 
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
*recode hhtoilet (3/5=3) (6=4) (-9=4)
*label define ltoilet 1 "Pit" 2 "Shared Pit" 3 "Flush/Bucket" 4 "None", replace
recode hhtoilet (3/5=1) (6=3) (-9=3)
label define ltoilet 1 "Pit/Flush/Bucket" 2 "Shared Pit" 3 "None", replace
label values hhtoilet ltoilet
*waste
recode hhwaste (-9/2=4) (3=3) (4=2) (5=1) (6=4)
label define lwaste 1 "Burning" 2 "Heap" 3 "Pit" 4 "Other"
label values hhwaste lwaste
*add durables and food and non-food
merge 1:1 hhid using "`lc_sdTemp'/HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "`lc_sdTemp'/HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
egen totnfood=rowtotal(xnonfood*)
drop if totnfood==0 //  4 HHs with nonfood consumption=0 will be dropped. 
drop totnfood
gen deflator=`deflator_u' if urban==1
replace deflator=`deflator_r' if urban==2
gen povline=`xpovline'
save "`lc_sdTemp'/HHData.dta", replace

*start RCS code
*run simulation
local using= "`lc_sdTemp'/HHData.dta"
local dirout = "${gsdOutput}/SSD"
local nmodules = `M'
local ncoref = 33
local ncorenf = 25
local ndiff=`ndiff'
local nsim = `N'
local nmi = `nI'
local povline = `xpovline'
local lmethod = "`lmethod'"
local model = "hhsize pchild psenior i.hhsex i.hhwater i.hhcook hhsleep i.hhhouse i.hhtoilet i.hhwaste"
local model2 = "hhsize pchild psenior i.hhsex"
local model3 = " "
local rseed = 23081980

local dirbase = "`dirout'/d`ndiff'm`nmodules'"

local ST=5
*local EN=60
local EN=80
*Number of items for food and non-food:
local num_f=149
local num_nf=118

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

use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4\Temp\mi_1.dta" ,clear
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
save "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\modsim\d3m4\Out\d_freq_SSD.dta" ,replace
export excel using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\modsim\d3m4\Out\d_freq_SSD.xls", replace first(var) 

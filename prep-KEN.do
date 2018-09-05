*Prepare Kenya data

clear all
ma drop all
set more off

*parameters
*data directory
local sData = "${gsdDataBox}/KEN-KIHBS2005"

*PPP (priv cons) from WDI in 2011: 35.42 LCU per int $
*CPI (2011): 114.022 (WDI)
*CPI (2005): 55.527 (WDI)
local xpovline = 1.90 * 35.42 / 114.022 * 55.527
*Poverty line from povcalnet
local xpovline = 1.90 * 35.4296

include "${gsdDo}/fRCS.do"

*CREATE MODULES 
*for validation of the method, missing data is assumed to be missing
*as the consumption aggregate implicitly assumes.
*food consumption
use "`sData'/section I Weekly Expenditure.dta", clear
*link deflator
merge m:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", keep(master match) keepusing(fpindex) nogen
*prepare id
gen hhid = id_clust * 10000 + id_hh
ren i02 foodid
drop if foodid>2000
egen xfood = rowtotal(i04k i05k i05ak i06k i07k)
replace xfood = xfood / 7 / fpindex if xfood<.
keep hhid foodid xfood
collapse (sum) xfood, by(hhid foodid)
fItems2RCS, hhid(hhid) itemid(foodid) value(xfood) red(0)
save "${gsdTemp}/KEN-HH-FoodItems.dta", replace
*non food consumption
use "`sData'/Section JKL Regular Non Food Items.dta", clear
*link deflator
merge m:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", keep(master match) keepusing(fpindex) nogen
*prepare id
gen hhid = id_clust * 10000 + id_hh
ren j02 nonfoodid
gen xnonfood = (j03k + j04k) * 12 / 365.25 / fpindex
keep hhid nonfoodid xnonfood
collapse (sum) xnonfood, by(hhid nonfoodid)
fItems2RCS, hhid(hhid) itemid(nonfoodid) value(xnonfood) red(0)
save "${gsdTemp}/KEN-HH-NonFoodItems.dta", replace

*get household characteristics
use "`sData'/Section_B_Household_member_Information.dta", clear
gen hhsex = b04 if b03==1
ren b05a age
keep hhsex age id_*
gen age_child = age<15 if age<.
gen age_adult = inrange(age,15,64) if age<.
gen age_senior = age>64 if age<.
collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior (firstnm) hhsex, by(id_clu id_hh)
merge 1:1 id_clu id_hh using "`sData'/Section_G_Housing.dta", nogen keep(match) keepusing(g01 g08a g09a g11 g16 g18 weights)
ren (g01 g08a g09a g11 g16 g18) (hhtenure hhunits hhrooms hhhouse hhcook hhtoilet)
ren weights weight
*simplify by setting missing values to conservative answers
gen hhid = id_clust * 10000 + id_hh
recode hhhouse (99=7) (.=7)
recode hhtoilet (99=2) (.=2)
recode hhtenure (1/2=1) (3/6=2) (.=2)
recode hhhouse (1/3=1) (4/7=2)
recode hhcook (1/2=1) (3/8=2) (.=2)
*assign average number of rooms (2) to missing
recode hhrooms (9/30=8) (.=2)
*add variables
gen pchild = nchild / hhsize
gen psenior = nsenior / hhsize
*add durables
merge 1:1 id_clu id_hh using "`sData'/consumption_aggregated_data.dta", nogen keep(match) keepusing(nfdusevl nfdremcs nfdremfd nfdremot fpindex rururb)
ren rururb strata
*remainder of consumption aggregate, incl transfer and rent imputation
gen xdurables = (nfdusevl+nfdremcs+nfdremfd+nfdremot) / fpindex * 12 / 365.25
gen xdurables_pc = xdurables / hhsize
drop nfdusevl fpindex
*add food and non-food
merge 1:1 hhid using "${gsdTemp}/KEN-HH-FoodItems.dta", nogen keep(match) keepusing(xfood*)
merge 1:1 hhid using "${gsdTemp}/KEN-HH-NonFoodItems.dta", nogen keep(match) keepusing(xnonfood*)
ren id_clu cluster
*delete missing consumption
egen ctf = rowtotal(xfood*)
egen ctnf = rowtotal(xnonfood*)
drop if missing(ctf) | missing(ctnf) | (ctf==0)
drop ctf ctnf
gen urban=(strata==2)
save "${gsdData}/KEN-HHData.dta", replace

*check whether we can reconstruct the consumption aggregate at the item level
use "${gsdData}/KEN-HHData.dta", clear
ren cluster id_clust
merge 1:1 id_clust id_hh using "`sData'/consumption_aggregated_data.dta", nogen keep(match) assert(match using) keepusing(y_i y2_i ctry_adq)
egen ctf = rowtotal(xfood*)
egen ctnf = rowtotal(xnonfood*)
gen poor = (ctf+ctnf+xdurables)/hhsize < `xpovline'
mean poor [pweight=weight*hhsize]
mean poor [pweight=weight*hhsize], over(strata)
local model = "hhsize pchild psenior i.hhsex i.hhtoilet i.hhtenure i.hhhouse i.hhcook hhrooms i.strata"
reg y2_i `model'

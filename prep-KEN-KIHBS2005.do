*Prepare Kenya data

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

*get household member information: education and labor
use "`sData'/Section C education.dta", clear
ren c06 edu
recode edu (5=3) (4=2) (7/10=8) (6=11)
gen lit = c24!=1 if !missing(c24)
keep id_clu id_hh b_id edu lit
tempfile fedu
save "`fedu'", replace
*labor
use "`sData'/Section E Labour.dta", clear
ren e_id b_id
ren e04 wor
recode wor (.=0) (2 3=3) (5/9=0)
keep id_clu id_hh b_id wor
tempfile fwor
save "`fwor'", replace

*get household characteristics
use "`sData'/Section_B_Household_member_Information.dta", clear
gen hhsex = b04 if b03==1
ren b05a age
merge 1:1 id_clu id_hh b_id using "`fedu'", nogen keep(master match) keepusing(edu lit)
gen hhedu = edu if b03==1
gen hhlit = lit if b03==1
merge 1:1 id_clu id_hh b_id using "`fwor'", nogen keep(master match) keepusing(wor)
gen hhwor = wor if b03==1
keep hhsex age id_* b_id hhedu hhlit hhwor
gen age_child = age<15 if age<.
gen age_adult = inrange(age,15,64) if age<.
gen age_senior = age>64 if age<.
collapse (count) hhsize=age (sum) nchild=age_child nadult=age_adult nsenior=age_senior (firstnm) hhsex hhedu hhlit hhwor, by(id_clu id_hh)
replace hhedu = 1 if missing(hhedu)
replace hhlit = 0 if missing(hhlit)
replace hhwor = 0 if missing(hhwor)
merge 1:1 id_clu id_hh using "`sData'/Section_G_Housing.dta", nogen keep(match) keepusing(g01 g08a g09a g11 g16 g18 weights)
ren (g01 g08a g09a g11 g16 g18) (hhtenure hhunits hhrooms hhhouse hhcook hhtoilet)
ren weights weight
*simplify by setting missing values to conservative answers
gen hhid = id_clust * 10000 + id_hh
recode hhhouse (99=7) (.=7) (3 = 2)
recode hhtoilet (99=2) (.=2)
recode hhtenure (1/2=1) (3/6=2) (.=2)
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
*prepare variable names for model selection
rename (nchild nadult nsenior hhsex hhtoilet hhtenure hhcook hhrooms pchild psenior hhlit) (mcon_=)
rename (hhhouse hhedu hhwor) (mcat_=)
compress
drop hhunits nfdremcs nfdremfd nfdremot
order hhid strata urban cluster weight hhsize xdurables mcat* mcon*
label var hhid "Unique household ID"
label var hhsize "Number of household members"
label var urban "Is urban"
label var cluster "Geographic cluster"
label var weight "Sampling weight"
label var xdurables "Consumption flow of durables"
label var mcat_hhedu "Education of household head"
label var mcat_hhwor "Employment status of household head"
label define lwor 0 "Not working" 1 "Paid employee" 3 "Own account" 4 "Unpaid family worker"
label val mcat_hhwor lwor
label var mcat_hhhouse "Type of dwelling" 
label var mcon_nchild "Number of children"
label var mcon_nadult "Number of adults"
label var mcon_nsenior "Number of senior"
label var mcon_hhsex "Sex of household head"
label var mcon_hhlit "Household is literate"
label var mcon_hhtenure "Has tenure track"
label var mcon_hhrooms "Number of rooms"
label var mcon_hhcook "Uses improved cooking"
label var mcon_hhtoilet "Has improved toilet"
label var mcon_pchild "Proportion of children"
label var mcon_psenior "Proportion of seniors"
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
local model = "hhsize mcon_pchild mcon_psenior i.mcon_hhsex i.mcat_hhedu i.mcat_hhwor mcon_hhlit i.mcon_hhtoilet i.mcon_hhtenure i.mcat_hhhouse i.mcon_hhcook mcon_hhrooms i.strata"
reg y2_i `model'
gen ly2_i = log(y2_i)
reg ly2_i `model'

*produce reduced dataset
use "${gsdData}/KEN-HHData.dta", clear
gen r = runiform()
bysort cluster: egen xr = mean(r)
drop if xr < .5
drop r xr
tempfile fhh
save "`fhh'", replace
local id = "hhid cluster id_hh"
local lis = "food nonfood"
foreach food of local lis {
	use "`fhh'", clear
	keep `id' x`food'*
	reshape long x`food', i(`id') j(fid)
	replace x`food' = 0 if missing(x`food')
	bysort hhid cluster id_hh: egen xtotal= total(x`food')
	gen r`food' = x`food' / xtotal
	bysort fid: egen xshare = mean(r`food')
	drop if xshare < .001
	drop xtotal r`food' xshare
	reshape wide x`food', i(hhid cluster id_hh) j(fid)
	tempfile f`food'
	save "`f`food''", replace
}
use "`fhh'", clear
drop xfood* xnonfood*
merge 1:1 `id' using "`ffood'", assert(match) nogen
merge 1:1 `id' using "`fnonfood'", assert(match) nogen
compress
save "${gsdData}/KEN-HHDatared.dta", replace

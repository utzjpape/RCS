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
/*
RCS_prepare using "`using'", dirbase("`dirbase'") nmodules(`nmodules') ncoref(`ncoref') ncorenf(`ncorenf') ndiff(`ndiff')
RCS_assign using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') rseed(`rseed')
*/

RCS_simulate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") model("`model'") model2("`model2'") model3("`model3'") rseed(`rseed') co(`co')


local povline="povline"
local deflator="deflator"
RCS_collate using "`using'", dirbase("`dirbase'") nsim(`nsim') nmi(`nmi') lmethod("`lmethod'") povline("`povline'") deflator("`deflator'")
RCS_analyze using "`using'", dirbase("`dirbase'") lmethod("`lmethod'") povline("`povline'") deflator("`deflator'")

/*

*subrun
if (1==2) {
	include "${gsdDo}/fRCS_log.do"
	RCS_simulate using "`using'", dirbase("`dirbase'") nmodules(`nmodules') nsim(`nsim') nmi(`nmi') lmethod("tobit") model("`model'") rseed(`rseed')
	RCS_collate using "`using'", dirbase("`dirbase'") nsim(`nsim') nmi(`nmi') lmethod("tobit")
	RCS_analyze using "`using'", dirbase("`dirbase'") lmethod("`lmethod'") povline(`povline')
}












*OLD CODE

if (1==2) {
	*check error relative to consumption -> lower incomes have larger positive error (over-estimation)
	*while higher incomes are slightly under-estimated
	use "C:\Users\wb390290\Box Sync\Home\Research\RCS\Out\d3m4\Temp\simd_MImvn_imp.dta", clear
	collapse (mean) est ref, by(simulation hhid cluster weight)
	gen x = (est - ref) / ref * 100
	collapse (mean) x ref, by(hhid cluster weight)
	egen r = rank(ref)
	graph twoway (scatter x r) (lfit x r)
}

if (1==2) {	
	foreach id of local xhh {
		quiet: summ hh`id' if simulation==0
		gen x`id' = `r(mean)' - hh`id' if simulation>0
		gen me`id' = x`id'^2 if simulation>0
		quiet: summ x`id' if simulation>0
		replace x`id' = `r(mean)' if simulation==0
		quiet: summ me`id' if simulation>0
		replace me`id' = sqrt(`r(mean)') if simulation==0
	}
	drop if simulation>0
	drop imputation
	reshape long hh x me, i(simulation) j(hhid)
	drop simulation
	summ me x ,d
	*analyze output per cluster
	insheet using "`lc_sdOut'/simd_`smethod'.txt", clear
	reshape long hh, i(simulation imputation) j(hhid)
	merge m:1 hhid using "`sData'/wfilez.dta", nogen keep(match) keepusing(strata weight cluster)
	collapse (mean) hh [aweight=weight], by(simulation imputation cluster)
	levelsof cluster, local(xcl)
	reshape wide hh, i(simulation imputation) j(cluster)
	foreach id of local xcl {
		quiet: summ hh`id' if simulation==0
		gen x`id' = `r(mean)' - hh`id' if simulation>0
		gen me`id' = x`id'^2 if simulation>0
		quiet: summ x`id' if simulation>0
		replace x`id' = `r(mean)' if simulation==0
		quiet: summ me`id' if simulation>0
		replace me`id' = sqrt(`r(mean)') if simulation==0
	}
	drop if simulation>0
	drop imputation
	reshape long hh x me, i(simulation) j(cl)
	drop simulation
	summ me x ,d
}






if (1==2) {
*analysis of simulation results
foreach smethod of local lmethod {
	capture: file close fh
	file open fh using "`lc_sdOut'/simr_`smethod'.txt", replace write
	*prepare title
	file write fh "Simulation" _tab "RealConsAvg" _tab "EstConsAvg" _tab "EstConsAvgSE" _tab "EstConsDiff" _tab "EstConsDiffSE" _n
	*extract consumption
	forvalues isim = 1/`N' {
		use "`lc_sdTemp'/sim_`smethod'_`isim'.dta", clear
		*create difference variable; after next run already integrated in file
		gen diff = .
		if ("`smethod'"="reg") {
			replace diff = xcons_pc - ccons_pc
			local mipre = ""
			local mipost = ""
		}
		else {
			mi register passive diff
			mi passive: replace diff = xcons_pc - ccons_pc
			local mipre = "mi estimate: "
			local mipost = "_mi"
		}
		*real consumption
		mean ccons_pc [aweight=weight]
		matrix B = e(b)
		file write fh "`isim'" _tab (B[1,1]) 
		*average consumption estimated
		`mipre' mean xcons_pc [aweight=weight]
		matrix B = e(b`mipost')
		matrix V = e(V`mipost')
		file write fh _tab (B[1,1]) _tab (sqrt(V[1,1]))
		*average difference 
		`mipre' mean diff [aweight=weight]
		matrix B = e(b`mipost')
		matrix V = e(V`mipost')
		file write fh _tab (B[1,1]) _tab (sqrt(V[1,1])) _n
	}
	file close fh
	*analyze output
	insheet using "`lc_sdOut'/simr_`smethod'.txt", clear
	hist estconsavg, normal dens xline(491.3649)
	graph export "`lc_sdOut'/consavg_`smethod'.png", replace
	hist estconsdiff, normal dens xline(0)
	graph export "`lc_sdOut'/diffavg_`smethod'.png", replace
}
}




	*compare estimates
*	mi estimate: mean xfcons_pc [aweight=weight]
*	mean cfcons_pc [aweight=weight]
*	mi estimate: mean xcons_pc [aweight=weight]
*	mean ccons_pc [aweight=weight]
*	mi estimate: mean ispoorf [aweight=weight]
*	mean ispoorf_chk [aweight=weight]
*	mi estimate: mean ispoor [aweight=weight]
*	mean ispoor_chk [aweight=weight]

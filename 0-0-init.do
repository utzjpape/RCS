
*Initialize work environment

global suser = lower(c(username))

clear all
set more off
set maxvar 30000
set matsize 11000
set seed 23081980 
set sortseed 11041955
set maxiter 100

version 14.2

if ("${suser}"=="wb390290") {
	*Utz
	if inlist("`c(hostname)'","wbgmsutz001") {
		set maxvar 120000
		*on virtual machine
		local swdLocal = "D:/wb390290/RCS"
	} 
	else {
		*Local directory of your checked out copy of the code
		local swdLocal = "C:\Users\WB390290\OneDrive - WBG\Home\Research\RCS\Analysis"
	}
}
else {
	di as error "Configure work environment in 00-init.do before running the code."
	error 1
}

*packages
local lp = "labmask vselect dirlist esttab fastgini grc1leg balancetable"
foreach p of local lp {
	capture : which `p'
	if (_rc) {
		display as result in smcl `"Please install command {it:`p'}."'
		exit 199
	}
}

*datasets to use
local ldata = "KEN-KIHBS NGA-GHS SDN-NBHS2009 SLD-SLHS2013 SSD-NBHS2009"

*prepare directories
global gsdData = "`swdLocal'/Data"
global gsdDo = "`swdLocal'/Do"
global gsdTemp = "`swdLocal'/Temp"
global gsdOutput = "`swdLocal'/Output"
global gsdDataBox = "`swdLocal'/DataBox"
*add ado path
adopath ++ "${gsdDo}/ado"

**If needed, install the directories and packages used in the process 
capture confirm file "`swdLocal'/Data/nul"
scalar define n_data=_rc
capture confirm file "`swdLocal'/Temp/nul"
scalar define n_temp=_rc
capture confirm file "`swdLocal'/Output/nul"
scalar define n_output=_rc
scalar define check=n_data+n_temp+n_output
di check

if check==0 {
		display "No action needed"
}
else {
	capture: mkdir "${gsdData}"
	mkdir "${gsdTemp}"
	mkdir "${gsdOutput}"	
}

*define functions needed

*prepare items in wide format, adding zeros for missings and conserving labels
* parameters:
*   hhid: unique identifier for households
*   itemid: unique identifier for items
*   value: variable capturing the value of consumption
*   [REDuced]: number of items to include in the final dataset (scaled to approx sum up to total consumption)
capture: program drop fItems2RCS
program define fItems2RCS
	syntax , hhid(varlist) itemid(varname) value(varname) [REDuced(integer 0)]
	* save the value labels for variables in local list
	quiet: levelsof `itemid', local(`itemid'_levels)
	foreach val of local `itemid'_levels {
		local `itemid'_`val' : label `: value label `itemid'' `val'
	}
	*remove 0 consumption items
	bysort `itemid': egen x`value' = total(`value')
	quiet: drop if x`value'==0 | missing(x`value')
	drop x`value'
	*create zeros for missing values
	quiet: reshape wide `value', i(`hhid') j(`itemid')
	foreach v of varlist `value'* {
		quiet: replace `v'=0 if `v'>=.
	}
	*reduce dataset if needed
	quiet: if (`reduced'>0) {
		*work in long dataset (but need zero values)
		reshape long `value', i(`hhid') j(`itemid')
		bysort `hhid': egen xt = total(`value')
		gen pt = `value' / xt
		bysort `itemid': egen ppt = mean(pt)
		egen r = rank(ppt)
		replace r= -r
		egen rr = group(r)
		*calculate scaling factor (is done in constant multiples of households)
		egen scale = total(ppt) if rr > `reduced'
		egen xscale = total(ppt)
		gen x = scale/xscale
		egen xfactor = mean(x)
		drop if rr > `reduced'
		replace `value' = `value' / xfactor
		quiet: summ xfactor
		local xf = 1-r(mean)
		drop xt pt ppt r rr scale x xscale xfactor
		reshape wide `value', i(`hhid') j(`itemid')
	}
	if (`reduced'>0) di "Reduced consumption items to `reduced' item, capturing `xf' of consumption."
	*reinstantiate labels
	foreach val of local `itemid'_levels {
		capture: label var `value'`val' "``itemid'_`val''"
	}
end


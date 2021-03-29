
*Initialize work environment

global suser = c(username)

clear all
set more off
set maxvar 30000
set matsize 11000
set seed 23081980 
set sortseed 11041955
set maxiter 100

version 14.2

if (inlist("${suser}","wb390290","WB390290")) {
	*Utz
	if inlist("`c(hostname)'","WIN-82M1NHB392O","wbgmsutz0318") {
		set maxvar 120000
		*on virtual machine
		local swdLocal = "D:\SV-RCS"
		*Box directory where the Data folder can be located
		local swdBox = "D:\Sh-RCS\DataBox"
	} 
	else {
		*Local directory of your checked out copy of the code
		local swdLocal = "C:\Users\wb390290\OneDrive - WBG\Home\Research\RCS\SV-Analysis"
		*Box directory where the Data folder can be located
		local swdBox = "C:\Users\wb390290\OneDrive - WBG\Home\Research\RCS\Sh-RCS\DataBox"
	}
}
*Aroob
else if (inlist("${suser}","wb504672","WB504672")) {	
	*Local directory of your checked out copy of the code
	local swdLocal = "C:\Users\WB504672\OneDrive - WBG\SVN\GLB-RCS"
	*One Drive directory where the Data folder can be located
	local swdBox = "C:\Users\WB504672\WBG\Utz Johann Pape - DataBox"
	local swdCurl = "C:\Users\wb504672\Documents"
}
*Luca
else if (inlist("${suser}","wb502620","WB502620")) {	
	*Local directory of your checked out copy of the code
	local swdLocal = "C:\Users\wb502620\Box Sync/LP Data files/Code/RCS/trunk"
	*Box directory where the Data folder can be located
	local swdBox = "C:\Users\wb502620\Box Sync\HCE 2011"
}
*Simon
else if (inlist("${suser}","wb495217","WB495217")) {	
	*Local directory of your checked out copy of the code
	local swdLocal = "C:\Users\WB495217\OneDrive - WBG\SL WBG Files\FY18 SDN\Poverty Assessment\RCS"
	*Box directory where the Data folder can be located
	local swdBox = "C:\Users\WB495217\WBG\Utz Johann Pape - Sh-RCS\DataBox"
}
else {
	di as error "Configure work environment in 00-init.do before running the code."
	error 1
}


global gsdData = "`swdLocal'/Data"
global gsdDo = "`swdLocal'/Do"
global gsdTemp = "`swdLocal'/Temp"
global gsdOutput = "`swdLocal'/Output"
global gsdDataBox = "`swdBox'"
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
	mkdir "${gsdData}"
	mkdir "${gsdTemp}"
	mkdir "${gsdOutput}"
	
	*install packages used in the process
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


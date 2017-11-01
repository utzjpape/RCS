
*Initialize work environment

global suser = c(username)

clear all
set more off
set maxvar 10000
set matsize 10000
set seed 23081980 
set sortseed 11041955


if (inlist("${suser}","wb390290","WB390290")) {
	*Utz
	*Local directory of your checked out copy of the code
	local swdLocal = "C:\Users\WB390290\OneDrive - WBG\Home\Research\RCS\SV-Analysis"
	*Box directory where the Data folder can be located
	local swdBox = "C:\Users\WB390290\OneDrive - WBG\Home\Research\RCS\Sh-RCS\DataBox"
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

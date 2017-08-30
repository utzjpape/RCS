
*Initialize work environment

global suser = c(username)

clear all
set more off
set maxvar 10000
set seed 23081980 
set sortseed 11041955

if (inlist("${suser}","wb390290","WB390290")) {
	*Utz
	*Local directory of your checked out copy of the code
	local swdLocal = "C:\Users\wb390290\Box Sync\Home\Research\RCS\Code"
	*Box directory where the Data folder can be located
	local swdBox = "C:\Users\wb390290\Box Sync\Home\Research\RCS\Shared\DataBox"
}
if (inlist("${suser}","wb252129","WB252129")) {
	*Shinya
	*Local directory of your checked out copy of the code
	local swdLocal = "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk"
	*Box directory where the Data folder can be located
	local swdBox = "C:\Users\wb252129\Box Sync\Shared\DataBox"
}
*Aroob
else if (inlist("${suser}","wb504672","WB504672")) {	
	*Local directory of your checked out copy of the code
	local swdLocal = "C:\Users\wb504672\Box Sync\RCS\trunk"
	*Box directory where the Data folder can be located
	local swdBox = "C:\Users\wb504672\Box Sync\HCE 2011"
	local swdCurl = "C:\Users\wb504672\Documents"
}
*Luca
else if (inlist("${suser}","wb502620","WB502620")) {	
	*Local directory of your checked out copy of the code
	local swdLocal = "C:\Users\wb502620\Box Sync/LP Data files/Code/RCS/trunk"
	*Box directory where the Data folder can be located
	local swdBox = "C:\Users\wb502620\Box Sync\HCE 2011"
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

*If you have not yet installed the directories and folders below, set local to 1
local installfolders = 0

if (`installfolders'==1) {
	mkdir "${gsdData}"
	mkdir "${gsdTemp}"
	mkdir "${gsdOutput}"
	
	*install packages used in the process
}

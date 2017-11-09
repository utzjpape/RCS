**SOM
capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SOM-level-2017-04-27.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SOM.do"
capture: log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\level"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\level" /M /E /G /H /Y

capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SOM-log-sm-2017-04-27.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SOM_log_sm.do"
log close
capture:mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_sm"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_sm" /M /E /G /H /Y





**SSD
capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SSD-level-2017-04-27.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SSD.do"
log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\level"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\level" /M /E /G /H /Y

capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SSD-log-sm-2017-04-27.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SSD_log_sm.do"
log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\log_sm"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\log_sm" /M /E /G /H /Y


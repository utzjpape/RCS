**SOM
capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SOM-level-2017-04-25.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SOM.do"
capture: log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\level"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\level" /M /E /G /H /Y

capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SOM-log-sm-2017-04-25.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SOM_log_sm.do"
log close
capture:mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_sm"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_sm" /M /E /G /H /Y

capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SOM-level-2017-04-25_TwostMITobit.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SOM_TwostMITobit.do"
capture: log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\level_TwostMITobit"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\level_TwostMITobit" /M /E /G /H /Y


capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SOM-log-sm-2017-04-25_TwostMITobit.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SOM_log_sm_TwostMITobit.do"
log close
capture:mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_sm_TwostMITobit"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_sm_TwostMITobit" /M /E /G /H /Y



capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SOM-level-2017-04-21_tobit.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SOM_tobit.do"
capture: log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\level_tobit"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\level_tobit" /M /E /G /H /Y

capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SOM-log-sm-2017-04-21_tobit2.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SOM_log_sm_tobit.do"
log close
capture:mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_sm_tobit"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_sm_tobit" /M /E /G /H /Y



capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SOM-log-2017-04-21.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SOM_log.do"
log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_no_sm"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SOM\saved\log_no_sm" /M /E /G /H /Y



**SSD
capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SSD-level-2017-04-25.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SSD.do"
log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\level"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\level" /M /E /G /H /Y

capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SSD-log-sm-2017-04-25.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SSD_log_sm.do"
log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\log_sm"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\log_sm" /M /E /G /H /Y


capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SSD-level-2017-04-21_tobit.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SSD_tobit.do"
log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\level_tobit"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\level_tobit" /M /E /G /H /Y














capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SSD-log-sm-2017-04-21_tobit.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SSD_log_sm_tobit.do"
log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\log_sm_tobit"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\log_sm_tobit" /M /E /G /H /Y



capture: log close
log using "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Log\SSD-log-2017-04-21.smcl", replace
do "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Do\U1_simulation-SSD_log.do"
log close
capture: mkdir "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\log_no_sm"
shell xcopy "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4" "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\saved\log_no_sm" /M /E /G /H /Y

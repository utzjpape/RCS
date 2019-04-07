*prepares example dataset

clear all
ma drop all
set more off

*prepare Kenyan dataset
local using= "${gsdData}/KEN-HHDatared.dta"
capture confirm file "`using'"
if _rc != 0 {
	quiet: do "${gsdDo}/prep-KEN.do"
}

*create input dataset for simulation
capture classutil drop .r
.r = .RCS.new
.r.prepare using "`using`red''", dirbase("${gsdOutput}/KEN-Example") nmodules(4) ncoref(10) ncorenf(10) ndiff(3) erase
.r.mask , nsim(1)

*remove variables not needed and label
use "${gsdOutput}/KEN-Example/Temp/mi_1.data", clear

*SIMULATE RCS FOR KENYA

ma drop all
set more off
set maxiter 100

local using0= "${gsdData}/KEN-KIHBS2015P-HHData.dta"
local using1 = "${gsdData}/KEN-KIHBS-HHData.dta"
capture confirm file "`using0'"
if _rc != 0 {
	quiet: do "${gsdDo}/prep-KEN-KIHBS.do"
}
capture confirm file "`using1'"
if _rc != 0 {
	use "`using0'", clear
	append using "${gsdData}/KEN-KIHBS2005P-HHData.dta", gen(train)
	save "`using1'", replace
}

*test run
local train = 0
capture classutil drop .r
.r = .RCS.new
.r.prepare using "`using`train''", dirbase("${gsdOutput}/KEN-KIHBS-xtest") nmodules(1) ncoref(0) ncorenf(0) nsim(2) train(`train')
.r.mask , p(.5)
.r.estimate , lmethod("xmi") nmi(5)
.r.collate
.r.analyze

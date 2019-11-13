*estimate optional consumption using a reduced dataset from Kenya KIHBS 2005/6

ma drop all
set more off
set maxiter 100

*load data file
local using= "${gsdData}/KEN-KIHBS2015P-HHData.dta"
use "`using'", clear

*reduce
gen r = runiform()
bysort cluster: gen xr = r[1]
drop if xr < .75
drop r xr
compress
save "${gsdData}/KEN-KIHBS2015P-HHData-red.dta", replace


*test run
capture classutil drop .r
.r = .RCS.new
.r.prepare using "${gsdData}/KEN-KIHBS2015P-HHData-red.dta", dirbase("${gsdOutput}/KEN-KIHBS-test") nmodules(4) ncoref(0) ncorenf(0) nsim(2) train(0)
.r.mask
.r.estimate , lmethod("swift2") nmi(2) force
.r.collate
.r.analyze

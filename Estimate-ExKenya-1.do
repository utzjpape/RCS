*estimate optional consumption using a reduced dataset from Kenya KIHBS 2005/6

ma drop all
set more off
set maxiter 100

*number of imputations
local nmi = 20

*load data file
local sf = "KEN-Example.dta"
local sf = "KIHBS2005P-Example_c10-m5-r0_vsmall.dta"
if ("${gsdOutput}"!="") local sf = "${gsdOutput}/`sf'"
use "`sf'", clear

*run rapid consumption and then (as we have full consumption) test it
rcs_estimate , hhid("hhid") hhsize("hhsize") strata("urban") weight("weight") hhmod("hhmod") cluster("cluster") xfcons("xfcons") xnfcons("xnfcons") mcon("mcon_*") mcat("mcat_*") rcscons("xcons") nmi(`nmi')
mi estimate: mean xcons [pweight=weight*hhsize]
*calculate poverty rate, using fictious poverty line
mi passive: gen poor = xcons < 1.2
mi estimate: mean poor [pweight=weight*hhsize]

*test imputations, using full consumption 
rcs_test , hhsize("hhsize") weight("weight") rcscons("xcons") fullcons("ccons")

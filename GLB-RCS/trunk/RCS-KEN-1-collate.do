*SIMULATE RCS FOR KENYA

ma drop all
set more off
set maxiter 100

*iterations
local lc = "0 1 3 5 10 20"
local lm = "2 4 6 8 10 12 15 20 25 30 40 50"

*collect results
forvalues t = 0/1 {
	clear
	foreach kc of local lc {
		foreach km of local lm {
			cap: append using "${gsdOutput}/KEN-KIHBS-c`kc'-m`km'-t`t'.dta"
			if _rc==601 di "File ${gsdOutput}/KEN-KIHBS-c`kc'-m`km'-t`t'.dta does not exist."
		}
	}
	save "${gsdOutput}/KEN-KIHBS-t`t'.dta", replace
}

*run simulations for RDE submission

if "${gsdDo}"=="" {
	di as error "0-0-run init.do first
	error 1
}

*test run
foreach sd of local ldata {
	di "Test run for `sd'..."
	*indicate existence of training dataset 
	local train = inlist("`sd'","KEN-KIHBS","NGA-GHS")
	capture classutil drop .r
	.r = .RCS.new
	.r.prepare using "${gsdData}/`sd'-HHData.dta", dirbase("${gsdTemp}/test-`sd'") nmodules(4) ncoref(5) ncorenf(5) nsim(2) train(`train')
	.r.mask 
	.r.estimate , lmethod("mi_2cel") nmi(5)
	.r.collate
	.r.analyze
	.r.cleanup
}

local lc_all = "0 5 10"
local lm_all0 = "2 4 6 8 10"		
local lm_all5 = "2 4 6 8 10 12 20 25 30 40 50"		
local lm_all10 = "2 4 6 8 10"		
local lc_ken = "0 1 3 5 10 20"
local lm_ken = "2 4 6 8 10 12 20 25 30 40 50"		
foreach sd of local ldata {
	*do more detailed run for Kenya
	if sd=="KEN-KIHBS" {
		local lc = lc_ken
	}
	else {
		local lc = lc_all
	}
	foreach kc of local lc {
		if sd=="KEN-KIHBS" {
			local lm = lm_ken		
		}
		else {
			local lm = lm_all`kc'
		}
		foreach km of local lm {
			di "Running for `sd': kc=`kc'; km=`km'"
			local train = inlist("`sd'","KEN-KIHBS","NGA-GHS")
			*number of simulations (should be 20)
			local nsim = 20
			*number of imputations (should be 50)
			local nmi = 50
			*methods
			local lmethod = "mi_2cel"
			local dirbase = "${gsdOutput}/`sd'-c`kc'-m`km'-t`train'"
			local using = "${gsdData}/`sd'-HHData.dta"
			capture confirm file "`dirbase'.dta"
			if _rc != 0 {
				*create instance to run RCS simulations
				capture classutil drop .r
				.r = .RCS.new
				di "... preparing ..."
				.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`km') ncoref(`kc') ncorenf(`kc') nsim(`nsim') train(`train') erase
				di "... masking ..."
				.r.mask
				di "... estimating ..."
				.r.estimate , lmethod("`lmethod'") nmi(`nmi')
				di "... collating ..."
				.r.collate
				di "... analyzing ..."
				.r.analyze , force
				gen kc = `kc'
				label var kc "Parameter: number of core items"
				gen km = `km'
				label var km "Parameter: number of modules"
				gen t = `train'
				label var t "Used training set"
				save "`dirbase'.dta", replace
				.r.cleanup
			}
		}
		
	}
}

*generate output for all countries
clear
foreach sd of local ldata {
	local train = inlist("`sd'","KEN-KIHBS","NGA-GHS")
	foreach kc of local lc_all {
		foreach km of local lm`kc' {
			local dirbase = "${gsdOutput}/`sd'-c`kc'-m`km'-t`train'"
			cap: append using "`dirbase'.dta"
			if _rc==601 di "File `dirbase'.dta does not exist."
			quietly: desc
			if r(k)==33 gen sd=""
			quietly: replace sd = "`sd'" if sd == ""
		}
	}
}
order sd, first
compress
save "${gsdOutput}/RDE-results_all.dta", replace

*generate output for Kenya
local sd = "KEN-KIHBS"
forvalues train = 0/1 {
	clear
	foreach kc of local lc_ken {
		foreach km of local lm_ken {
			local dirbase = "${gsdOutput}/`sd'-c`kc'-m`km'-t`train'"
			cap: append using "`dirbase'.dta"
			if _rc==601 di "File `dirbase'.dta does not exist."
		}
	}
	compress
	save "${gsdOutput}/RDE-results_ken`train'.dta", replace
}
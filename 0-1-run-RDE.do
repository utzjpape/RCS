*run simulations for RDE submission

if "${gsdDo}"=="" {
	di as error "0-0-run init.do first
	error 1
}

capture confirm file "${gsdOutput}/RDE-results_all.dta"
if _rc != 0 {
	*prepare datasets
	foreach sd of local ldata {
		di "Preparing dataset for `sd'..."
		run "${gsdDo}/1-prep-`sd'.do"
	}

	*run simulations and collate dataset
	run "${gsdDo}/2-sim-RDE.do"
}

*analyze Kenya results
run "${gsdDo}/3-ana-RDE-1_ken2015C.do"
run "${gsdDo}/3-ana-RDE-1_ken2015P.do"
run "${gsdDo}/3-ana-RDE-2_all.do"

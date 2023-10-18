*run analysis for RDE submission

if "${gsdDo}"=="" {
	di as error "0-0-run init.do first
	error 1
}

*prepare datasets
local ldata = "KEN-KIHBS NGA-GHS PAK-HIES2015 SDN-NBHS2009 SLD-SLHS2013 SSD-NBHS2009 "
foreach sd of local ldata {
	di "Preparing dataset for `sd'..."
	run "${gsdDo}/1-prep-`sd'.do"
}

*test run
local ldata = "KEN-KIHBS NGA-GHS PAK-HIES2015 SDN-NBHS2009 SLD-SLHS2013 SSD-NBHS2009"
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

*run on server
local ldata = "KEN-KIHBS NGA-GHS PAK-HIES2015 SDN-NBHS2009 SLD-SLHS2013 SSD-NBHS2009"
local ldata = "KEN-KIHBS NGA-GHS SDN-NBHS2009 SLD-SLHS2013 SSD-NBHS2009"
foreach sd of local ldata {
	*indicate existence of training dataset 
	local lc = "0 1 3 5 10 20"
	*run for best method over different number of modules and core
	foreach kc of local lc {
		*local sd = ""
		*local kc = 0
		local lm = "2 4 6 8 10"
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
				.r.prepare using "`using'", dirbase("`dirbase'") nmodules(`km') ncoref(`kc') ncorenf(`kc') nsim(`nsim') train(`train') erase
				.r.mask
				.r.estimate , lmethod("`lmethod'") nmi(`nmi')
				.r.collate
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



*should be run on server
*run "${gsdDo}/RCS-KEN-0-simulate.do"
*run "${gsdDo}/RCS-KEN-1-collate.do"
*can be run on local machine
*run "${gsdDo}/RCS-KEN-2-analysis-2015P.do"
*run "${gsdDo}/RCS-KEN-2-analysis-2015C.do"

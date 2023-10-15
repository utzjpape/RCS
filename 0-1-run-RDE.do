*run analysis for RDE submission

if "${gsdDo}"=="" {
	di as error "0-0-run init.do first
	error 1
}

*prepare datasets
local ldata = "KEN-KIHBS NGA-GHS PAK-HIES2015 SDN-NBHS2009 SLD-SLHS2013 SSD-NBHS2009 "
foreach sdata of local ldata {
	di "Preparing dataset for `sdata'..."
	run "${gsdDo}/1-prep-`sdata'.do"
}
*combine datasets for training for surveys with two timepoints


*test run
local ldata = "KEN-KIHBS2005P KEN-KIHBS2015P NGA-GHS2016 NGA-GHS2019 PAK-HIES2015 SDN-NBHS2009 SLD-SLHS2013 SSD-NBHS2009"
foreach sdata of local ldata {
	di "Test run for `sdata'..."
	*capture classutil drop .r
	.r = .RCS.new
	.r.prepare using "${gsdData}/`sdata'-HHData.dta", dirbase("${gsdTemp}/test-`sdata'") nmodules(4) ncoref(5) ncorenf(5) nsim(2)
	.r.mask 
	.r.estimate , lmethod("mi_2cel") nmi(5)
	.r.collate
	.r.analyze
}

*should be run on server
*run "${gsdDo}/RCS-KEN-0-simulate.do"
*run "${gsdDo}/RCS-KEN-1-collate.do"
*can be run on local machine
*run "${gsdDo}/RCS-KEN-2-analysis-2015P.do"
*run "${gsdDo}/RCS-KEN-2-analysis-2015C.do"


if inlist("`sd'","KEN-KIHBS","NGA-GHS") {
	local using = "`sd'201"
}
else {
	local using = "`sd'"
}
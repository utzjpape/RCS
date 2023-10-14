*run analysis for RDE submission

if "${gsdDo}"=="" {
	di as error "0-0-run init.do first
	error 1
}


*no household data: ETH NGA

run "${gsdDo}/1-prep-ETH-HCE2011.do"

run "${gsdDo}/1-prep-NGA-GHA2016.do"


*prepare datasets
local ldata = "KEN-KIHBS ETH-HCE2011 NGA-GHA2016 PAK-HIES2015 SDN-NBHS2009 SLD-SLHS2013 SSD-NHBS2009 "
foreach sdata in ldata {
	run "${gsdDo}/1-prep-`sdata'.do"
}


stop

*should be run on server
run "${gsdDo}/RCS-KEN-0-simulate.do"
run "${gsdDo}/RCS-KEN-1-collate.do"
*can be run on local machine
run "${gsdDo}/RCS-KEN-2-analysis-2015P.do"
run "${gsdDo}/RCS-KEN-2-analysis-2015C.do"

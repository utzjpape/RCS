*run analysis for RDE submission

if "${gsdDo}"=="" {
	di as error "run init.do first
	error 1
}

*run for Kenya
run "${gsdDo}/prep-KEN-KIHBS.do"
*should be run on server
run "${gsdDo}/RCS-KEN-0-simulate.do"
run "${gsdDo}/RCS-KEN-1-collate.do"
*can be run on local machine
run "${gsdDo}/RCS-KEN-2-analysis-2015P.do"
run "${gsdDo}/RCS-KEN-2-analysis-2015C.do"
